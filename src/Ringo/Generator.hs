module Ringo.Generator
       ( tableDefnSQL
       , factTableDefnSQL
       , dimensionTablePopulateSQL
       , factTablePopulateSQL
       ) where

import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader (Reader, asks)
import Data.List            (nub, find)
import Data.Maybe           (fromJust, fromMaybe, mapMaybe)
import Data.Monoid          ((<>))
import Data.Text            (Text)

import Ringo.Extractor.Internal
import Ringo.Types
import Ringo.Utils

nullableDefnSQL :: Nullable -> Text
nullableDefnSQL Null    = "NULL"
nullableDefnSQL NotNull = "NOT NULL"

columnDefnSQL :: Column -> Text
columnDefnSQL Column {..} =
  columnName <> " " <> columnType <> " " <> nullableDefnSQL columnNullable

joinColumnNames :: [ColumnName] -> Text
joinColumnNames = Text.intercalate ",\n"

fullColName :: TableName -> ColumnName -> ColumnName
fullColName tName cName = tName <> "." <> cName

constraintDefnSQL :: Table -> TableConstraint -> [Text]
constraintDefnSQL Table {..} constraint =
  let alterTableSQL = "ALTER TABLE ONLY " <> tableName <> " ADD "
  in case constraint of
    PrimaryKey cName -> [ alterTableSQL <> "PRIMARY KEY (" <> cName <> ")" ]
    ForeignKey oTableName cNamePairs ->
      [ alterTableSQL <> "FOREIGN KEY (" <> joinColumnNames (map fst cNamePairs) <> ") REFERENCES "
          <> oTableName <> " (" <> joinColumnNames (map snd cNamePairs) <> ")" ]
    UniqueKey cNames -> ["CREATE UNIQUE INDEX ON " <> tableName <> " (" <> joinColumnNames cNames <> ")"]

tableDefnSQL :: Table -> [Text]
tableDefnSQL table@Table {..} =
  tableSQL : concatMap (constraintDefnSQL table) tableConstraints
  where
    tableSQL = "CREATE TABLE " <> tableName <> " (\n"
                 <> (joinColumnNames . map columnDefnSQL $ tableColumns)
                 <> "\n)"

factTableDefnSQL :: Fact -> Table -> Reader Env [Text]
factTableDefnSQL fact table = do
  Settings {..} <- asks envSettings
  allDims       <- extractAllDimensionTables fact

  let factCols  = forMaybe (factColumns fact) $ \col -> case col of
        DimTime cName -> Just $ timeUnitColumnName settingDimTableIdColumnName cName settingTimeUnit
        NoDimId cName -> Just cName
        _             -> Nothing

      dimCols   = [ factDimFKIdColumnName settingDimPrefix settingDimTableIdColumnName tableName
                    | (_, Table {..}) <- allDims ]

      indexSQLs = [ "CREATE INDEX ON " <> tableName table <> " USING btree (" <> col <> ")"
                    | col <- factCols ++ dimCols ]

  return $ tableDefnSQL table ++ indexSQLs

dimColumnMapping :: Text -> Fact -> TableName -> [(ColumnName, ColumnName)]
dimColumnMapping dimPrefix fact dimTableName =
  [ (dimColumnName dName cName, cName)
    | DimVal dName cName <- factColumns fact , dimPrefix <> dName == dimTableName]

coalesceColumn :: TableName -> Column -> Text
coalesceColumn tName Column{..} =
  if columnNullable == Null
    then "coalesce(" <> fqColName <> "," <> defVal columnType <> ")"
    else fqColName
  where
    fqColName = fullColName tName columnName

    defVal colType
     | "integer" `Text.isPrefixOf` colType = "-42"
     | "timestamp" `Text.isPrefixOf` colType = "'00-00-00 00:00:00'"
     | "character" `Text.isPrefixOf` colType = "'XXX_UNKNOWN_'"
     | "uuid" `Text.isPrefixOf` colType = "'00000000-0000-0000-0000-000000000000'::uuid"
     | "boolean" `Text.isPrefixOf` colType = "false"
     | otherwise = error $ "Unknown column type: " ++ Text.unpack colType

dimensionTablePopulateSQL :: TablePopulationMode -> Fact -> TableName -> Reader Env Text
dimensionTablePopulateSQL popMode fact dimTableName = do
  dimPrefix           <- settingDimPrefix <$> asks envSettings
  tables              <- asks envTables
  let factTable       = fromJust $ findTable (factTableName fact) tables
      colMapping      = dimColumnMapping dimPrefix fact dimTableName
      baseSelectC     = "SELECT DISTINCT\n"
                          <> joinColumnNames
                              (map (\(_, cName) ->
                                     let col = fromJust . findColumn cName $ tableColumns factTable
                                     in coalesceColumn (factTableName fact) col <> " AS " <> cName)
                                   colMapping)
                          <> "\n"
                          <> "FROM " <> factTableName fact
      insertC selectC = "INSERT INTO " <> dimTableName
                          <> " (\n" <> joinColumnNames (map fst colMapping) <> "\n) "
                          <> "SELECT x.* FROM (\n" <> selectC <> ") x"
      timeCol         = head [ cName | DimTime cName <- factColumns fact ]
  return $ case popMode of
    FullPopulation        -> insertC baseSelectC
    IncrementalPopulation ->
      insertC (baseSelectC <> "\nWHERE "
                 <> timeCol <> " > ? AND " <> timeCol <> " <= ?"
                 <> " AND (\n"
                 <> Text.intercalate "\nOR " [ c <> " IS NOT NULL" | (_, c) <- colMapping ]
                 <> "\n)")
        <> "\nLEFT JOIN " <> dimTableName <> " ON\n"
        <> Text.intercalate " \nAND "
              [ fullColName dimTableName c1 <> " IS NOT DISTINCT FROM " <> fullColName "x" c2
                | (c1, c2) <- colMapping ]
        <> "\nWHERE " <> Text.intercalate " \nAND "
                           [ fullColName dimTableName c <> " IS NULL" | (c, _) <- colMapping ]

data FactTablePopulateSelectSQL = FactTablePopulateSelectSQL
                                { ftpsSelectCols   :: ![(Text, Text)]
                                , ftpsSelectTable  :: !Text
                                , ftpsJoinClauses  :: ![Text]
                                , ftpsWhereClauses :: ![Text]
                                , ftpsGroupByCols  :: ![Text]
                                } deriving (Show, Eq)

factTablePopulateSQL :: TablePopulationMode -> Fact -> Reader Env [Text]
factTablePopulateSQL popMode fact = do
  Settings {..}      <- asks envSettings
  allDims            <- extractAllDimensionTables fact
  tables             <- asks envTables
  let fTableName     = factTableName fact
      fTable         = fromJust . findTable fTableName $ tables
      dimIdColName   = settingDimTableIdColumnName
      tablePKColName = head [ cName | PrimaryKey cName <- tableConstraints fTable ]

      timeUnitColumnInsertSQL cName =
        let colName = timeUnitColumnName dimIdColName cName settingTimeUnit
        in ( colName
           , "extract(epoch from " <> fullColName fTableName cName <> ")::bigint/"
                <> Text.pack (show $ timeUnitToSeconds settingTimeUnit)
           , True
           )

      factColMap = concatFor (factColumns fact) $ \col -> case col of
        DimTime cName             -> [ timeUnitColumnInsertSQL cName ]
        NoDimId cName             ->
          let sCol = fromJust . findColumn cName $ tableColumns fTable
          in [ (cName, coalesceColumn fTableName sCol, True) ]
        FactCount scName cName    ->
          [ (cName, "count(" <> maybe "*" (fullColName fTableName) scName <> ")", False) ]
        FactSum scName cName      ->
          [ (cName, "sum(" <> fullColName fTableName scName <> ")", False) ]
        FactAverage scName cName  ->
          [ ( cName <> settingAvgCountColumSuffix
            , "count(" <> fullColName fTableName scName <> ")"
            , False
            )
          , ( cName <> settingAvgSumColumnSuffix
            , "sum(" <> fullColName fTableName scName <> ")"
            , False
            )
          ]
        FactCountDistinct _ cName -> [ (cName, "'{}'::json", False)]
        _                        -> []

      dimColMap = for allDims $ \(dimFact, factTable@Table {tableName}) ->
        let colName             = factDimFKIdColumnName settingDimPrefix dimIdColName tableName
            col                 = fromJust . findColumn colName $ tableColumns factSourceTable
            factSourceTableName = factTableName dimFact
            factSourceTable     = fromJust . findTable factSourceTableName $ tables
            insertSQL           = if factTable `elem` tables -- existing dimension table
              then (if columnNullable col == Null then coalesceFKId else id)
                     $ fullColName factSourceTableName colName
              else let
                  dimLookupWhereClauses =
                    [ fullColName tableName c1 <> " = " <> coalesceColumn factSourceTableName col2
                      | (c1, c2) <- dimColumnMapping settingDimPrefix dimFact tableName
                      , let col2 = fromJust . findColumn c2 $ tableColumns factSourceTable ]
                in "SELECT " <> dimIdColName <> " FROM " <> tableName <> "\nWHERE "
                          <> Text.intercalate "\n AND " dimLookupWhereClauses
            insertSQL' = if factSourceTableName == fTableName
                           then insertSQL
                           else coalesceFKId insertSQL

        in (colName, insertSQL', True)

      colMap = [ (cName, (sql, groupByColPrefix <> cName), addAs)
                 | (cName, sql, addAs) <- factColMap ++ dimColMap ]

      joinClauses =
        mapMaybe (\tName -> (\p -> "LEFT JOIN " <> tName <> "\nON "<> p) <$> joinClausePreds fTable tName)
        . nub
        . map (factTableName . fst)
        $ allDims

      timeCol     = fullColName fTableName $ head [ cName | DimTime cName <- factColumns fact ]

      extFactTableName =
        extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

      insertIntoSelectSQL =
        FactTablePopulateSelectSQL
          { ftpsSelectCols   = map snd3 colMap
          , ftpsSelectTable  = fTableName
          , ftpsJoinClauses  = joinClauses
          , ftpsWhereClauses = if popMode == IncrementalPopulation
                                then [timeCol <> " > ?", timeCol <> " <= ?"]
                                else []
          , ftpsGroupByCols  = map ((groupByColPrefix <>) . fst3) . filter thd3 $ colMap
          }

      insertIntoInsertSQL = "INSERT INTO " <> extFactTableName
                              <> " (\n" <> Text.intercalate ",\n " (map fst3 colMap) <> "\n)"

      countDistinctCols   = [ col | col@(FactCountDistinct _ _) <- factColumns fact]

      updateSQLs          =
        let origGroupByCols = ftpsGroupByCols insertIntoSelectSQL
            origSelectCols  = ftpsSelectCols insertIntoSelectSQL

        in for countDistinctCols $ \(FactCountDistinct scName cName) ->
          let unqCol           = fullColName fTableName (fromMaybe tablePKColName scName) <> "::text"

              bucketSelectCols =
                [ ( "hashtext(" <> unqCol <> ") & "
                     <> Text.pack (show $ bucketCount settingFactCountDistinctErrorRate - 1)
                  , cName <> "_bnum")
                , ( "31 - floor(log(2, min(hashtext(" <> unqCol <> ") & ~(1 << 31))))::int"
                  , cName <> "_bhash"
                  )
                ]

              selectSQL        = toSelectSQL $
                insertIntoSelectSQL
                  { ftpsSelectCols   = filter ((`elem` origGroupByCols) . snd) origSelectCols ++ bucketSelectCols
                  , ftpsGroupByCols  = origGroupByCols ++ [cName <> "_bnum"]
                  , ftpsWhereClauses = ftpsWhereClauses insertIntoSelectSQL ++ [ unqCol <> " IS NOT NULL" ]
                  }

              aggSelectClause  =
                "json_object_agg(" <> cName <> "_bnum, " <> cName <> "_bhash) AS " <> cName

          in "UPDATE " <> extFactTableName
               <> "\nSET " <> cName <> " = " <> fullColName "xyz" cName
               <> "\nFROM ("
               <> "\nSELECT " <> joinColumnNames (origGroupByCols ++ [aggSelectClause])
               <> "\nFROM (\n" <> selectSQL <> "\n) zyx"
               <> "\nGROUP BY \n" <> joinColumnNames origGroupByCols
               <> "\n) xyz"
               <> "\n WHERE\n"
               <> Text.intercalate "\nAND "
                    [ fullColName extFactTableName .fromJust . Text.stripPrefix groupByColPrefix $ col
                        <> " = " <> fullColName "xyz" col
                      | col <- origGroupByCols ]

  return $ insertIntoInsertSQL <> "\n" <> toSelectSQL insertIntoSelectSQL :
             if null countDistinctCols then [] else updateSQLs
  where
    groupByColPrefix = "xxff_"

    joinClausePreds table oTableName =
      fmap (\(ForeignKey _ colPairs) ->
              Text.intercalate " AND "
              . map (\(c1, c2) -> fullColName (tableName table) c1 <> " = " <> fullColName oTableName c2)
              $ colPairs )
      . find (\cons -> case cons of
                  ForeignKey tName _ -> tName == oTableName
                  _                  -> False)
      . tableConstraints
      $ table

    toSelectSQL FactTablePopulateSelectSQL {..} =
      "SELECT \n" <> joinColumnNames (map (uncurry asName) ftpsSelectCols)
        <> "\nFROM " <> ftpsSelectTable
        <> (if not . null $ ftpsJoinClauses
              then "\n" <> Text.intercalate "\n" ftpsJoinClauses
              else "")
        <> (if not . null $ ftpsWhereClauses
             then "\nWHERE " <> Text.intercalate "\nAND " ftpsWhereClauses
             else "")
        <> "\nGROUP BY \n"
        <> joinColumnNames ftpsGroupByCols
      where
        asName sql alias = "(" <> sql <> ")" <> " as " <> alias

    coalesceFKId col =
      if "coalesce" `Text.isPrefixOf` col
        then col
        else "coalesce((" <> col <> "), -1)"

    bucketCount :: Double -> Integer
    bucketCount errorRate =
      let power :: Double = fromIntegral (ceiling . logBase 2 $ (1.04 / errorRate) ** 2 :: Integer)
      in ceiling $ 2 ** power


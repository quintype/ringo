module Ringo.Generator
       ( tableDefnSQL
       , factTableDefnSQL
       , dimensionTablePopulateSQL
       , factTablePopulateSQL
       ) where

import qualified Data.Map as Map
import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader (Reader, asks)
import Data.List            (nub, find)
import Data.Maybe           (fromJust, fromMaybe, mapMaybe, listToMaybe)
import Data.Monoid          ((<>))
import Data.Text            (Text)

import Ringo.Extractor.Internal
import Ringo.Types
import Ringo.Utils

columnDefnSQL :: Column -> Text
columnDefnSQL Column {..} =
  columnName <> " " <> columnType <> " " <> nullableDefnSQL columnNullable
  where
    nullableDefnSQL Null    = "NULL"
    nullableDefnSQL NotNull = "NOT NULL"

joinColumnNames :: [ColumnName] -> Text
joinColumnNames = Text.intercalate ",\n"

fullColName :: TableName -> ColumnName -> ColumnName
fullColName tName cName = tName <> "." <> cName

tableDefnSQL :: Table -> [Text]
tableDefnSQL Table {..} =
  tableSQL : concatMap constraintDefnSQL tableConstraints
  where
    tableSQL = "CREATE TABLE " <> tableName <> " (\n"
                 <> (joinColumnNames . map columnDefnSQL $ tableColumns)
                 <> "\n)"

    constraintDefnSQL constraint =
      let alterTableSQL = "ALTER TABLE ONLY " <> tableName <> " ADD "
      in case constraint of
        PrimaryKey cName -> [ alterTableSQL <> "PRIMARY KEY (" <> cName <> ")" ]
        ForeignKey oTableName cNamePairs ->
          [ alterTableSQL <> "FOREIGN KEY (" <> joinColumnNames (map fst cNamePairs) <> ") REFERENCES "
              <> oTableName <> " (" <> joinColumnNames (map snd cNamePairs) <> ")" ]
        UniqueKey cNames -> ["CREATE UNIQUE INDEX ON " <> tableName <> " (" <> joinColumnNames cNames <> ")"]


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

coalesceColumn :: TypeDefaults -> TableName -> Column -> Text
coalesceColumn defaults tName Column{..} =
  if columnNullable == Null
    then "coalesce(" <> fqColName <> ", " <> defVal columnType <> ")"
    else fqColName
  where
    fqColName = fullColName tName columnName

    defVal colType =
      fromMaybe (error $ "Default value not known for column type: " ++ Text.unpack colType)
      . fmap snd
      . find (\(k, _) -> k `Text.isPrefixOf` colType)
      . Map.toList
      $ defaults

dimensionTablePopulateSQL :: TablePopulationMode -> Fact -> TableName -> Reader Env Text
dimensionTablePopulateSQL popMode fact dimTableName = do
  dimPrefix           <- settingDimPrefix <$> asks envSettings
  tables              <- asks envTables
  defaults            <- asks envTypeDefaults
  let factTable       = fromJust $ findTable (factTableName fact) tables
      colMapping      = dimColumnMapping dimPrefix fact dimTableName
      selectCols      = [ coalesceColumn defaults (factTableName fact) col <> " AS " <> cName
                          | (_, cName) <- colMapping
                          , let col    = fromJust . findColumn cName $ tableColumns factTable ]
      baseSelectC     = "SELECT DISTINCT\n" <> joinColumnNames selectCols
                          <> "\nFROM " <> factTableName fact
      baseWhereC      = "(\n"
                          <> Text.intercalate "\nOR " [ c <> " IS NOT NULL" | (_, c) <- colMapping ]
                          <> "\n)"
      insertC selectC whereCs =
        "INSERT INTO " <> dimTableName
          <> " (\n" <> joinColumnNames (map fst colMapping) <> "\n) "
          <> "SELECT x.* FROM (\n"
          <> selectC <> "\nWHERE " <> Text.intercalate " AND\n" whereCs
          <> ") x"
      timeCol         = head [ cName | DimTime cName <- factColumns fact ]
  return $ case popMode of
    FullPopulation        -> insertC baseSelectC [baseWhereC]
    IncrementalPopulation ->
      insertC baseSelectC [baseWhereC, timeCol <> " > ?", timeCol <> " <= ?"]
        <> "\nLEFT JOIN " <> dimTableName <> " ON\n"
        <> Text.intercalate " \nAND "
              [ fullColName dimTableName c1 <> " = " <> fullColName "x" c2
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

factTableUpdateSQL :: Fact -> Text -> FactTablePopulateSelectSQL -> Reader Env [Text]
factTableUpdateSQL fact groupByColPrefix populateSelectSQL@FactTablePopulateSelectSQL {..} = do
  Settings {..}         <- asks envSettings
  tables                <- asks envTables
  let countDistinctCols = [ col | col@(FactCountDistinct _ _) <- factColumns fact]
      fTableName        = factTableName fact
      fTable            = fromJust . findTable fTableName $ tables
      tablePKColName    = head [ cName | PrimaryKey cName <- tableConstraints fTable ]
      extFactTableName  =
        extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

  return $ for countDistinctCols $ \(FactCountDistinct scName cName) ->
    let unqCol           = fullColName fTableName (fromMaybe tablePKColName scName) <> "::text"

        bucketSelectCols =
          [ ( "hashtext(" <> unqCol <> ") & "
               <> Text.pack (show $ bucketCount settingFactCountDistinctErrorRate - 1)
            , cName <> "_bnum"
            )
          , ( "31 - floor(log(2, min(hashtext(" <> unqCol <> ") & ~(1 << 31))))::int"
            , cName <> "_bhash"
            )
          ]

        selectSQL        = toSelectSQL $
          populateSelectSQL
            { ftpsSelectCols   = filter ((`elem` ftpsGroupByCols) . snd) ftpsSelectCols ++ bucketSelectCols
            , ftpsGroupByCols  = ftpsGroupByCols ++ [ cName <> "_bnum" ]
            , ftpsWhereClauses = ftpsWhereClauses ++ [ unqCol <> " IS NOT NULL" ]
            }

        aggSelectClause  =
          "json_object_agg(" <> cName <> "_bnum, " <> cName <> "_bhash) AS " <> cName

    in "UPDATE " <> extFactTableName
         <> "\nSET " <> cName <> " = " <> fullColName "xyz" cName
         <> "\nFROM ("
         <> "\nSELECT " <> joinColumnNames (ftpsGroupByCols ++ [aggSelectClause])
         <> "\nFROM (\n" <> selectSQL <> "\n) zyx"
         <> "\nGROUP BY \n" <> joinColumnNames ftpsGroupByCols
         <> "\n) xyz"
         <> "\n WHERE\n"
         <> Text.intercalate "\nAND "
              [ fullColName extFactTableName .fromJust . Text.stripPrefix groupByColPrefix $ col
                  <> " = " <> fullColName "xyz" col
                | col <- ftpsGroupByCols ]
  where
    bucketCount :: Double -> Integer
    bucketCount errorRate =
      let power :: Double = fromIntegral (ceiling . logBase 2 $ (1.04 / errorRate) ** 2 :: Integer)
      in ceiling $ 2 ** power

factTablePopulateSQL :: TablePopulationMode -> Fact -> Reader Env [Text]
factTablePopulateSQL popMode fact = do
  Settings {..}      <- asks envSettings
  allDims            <- extractAllDimensionTables fact
  tables             <- asks envTables
  defaults           <- asks envTypeDefaults
  let fTableName     = factTableName fact
      fTable         = fromJust . findTable fTableName $ tables
      dimIdColName   = settingDimTableIdColumnName

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
          in [ (cName, coalesceColumn defaults fTableName sCol, True) ]
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

      dimColMap = for allDims $ \(dimFact, factTable@Table {tableName}) -> let
          dimFKIdColName        = factDimFKIdColumnName settingDimPrefix dimIdColName tableName
          factSourceTableName   = factTableName dimFact
          factSourceTable       = fromJust . findTable factSourceTableName $ tables
          dimFKIdColumn         = fromJust . findColumn dimFKIdColName $ tableColumns factSourceTable
          dimLookupWhereClauses =
            [ fullColName tableName dimColName <> " = " <> coalesceColumn defaults factSourceTableName sourceCol
              | (dimColName, sourceColName) <- dimColumnMapping settingDimPrefix dimFact tableName
              , let sourceCol = fromJust . findColumn sourceColName $ tableColumns factSourceTable ]
          insertSQL             = if factTable `elem` tables -- existing dimension table
            then (if columnNullable dimFKIdColumn == Null then coalesceFKId else id)
                   $ fullColName factSourceTableName dimFKIdColName
            else "SELECT " <> dimIdColName <> " FROM " <> tableName <> "\nWHERE "
                  <> Text.intercalate "\n AND " dimLookupWhereClauses
        in (dimFKIdColName, coalesceFKId insertSQL, True)

      colMap              = [ (cName, (sql, groupByColPrefix <> cName), addToGroupBy)
                              | (cName, sql, addToGroupBy) <- factColMap ++ dimColMap ]

      joinClauses         =
        mapMaybe (\tName -> (\p -> "LEFT JOIN " <> tName <> "\nON "<> p) <$> joinClausePreds fTable tName)
        . nub
        . map (factTableName . fst)
        $ allDims

      timeCol             = fullColName fTableName $ head [ cName | DimTime cName <- factColumns fact ]

      extFactTableName    =
        extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

      populateSelectSQL   =
        FactTablePopulateSelectSQL
          { ftpsSelectCols   = map snd3 colMap
          , ftpsSelectTable  = fTableName
          , ftpsJoinClauses  = joinClauses
          , ftpsWhereClauses = if popMode == IncrementalPopulation
                                then [ timeCol <> " > ?", timeCol <> " <= ?" ]
                                else []
          , ftpsGroupByCols  = map ((groupByColPrefix <>) . fst3) . filter thd3 $ colMap
          }

      insertIntoSQL       = "INSERT INTO " <> extFactTableName
                              <> " (\n" <> Text.intercalate ",\n " (map fst3 colMap) <> "\n)\n"
                              <> toSelectSQL populateSelectSQL

  updateSQLs <- factTableUpdateSQL fact groupByColPrefix populateSelectSQL

  return $ insertIntoSQL : updateSQLs
  where
    groupByColPrefix = "xxff_"

    joinClausePreds table oTableName =
      Text.intercalate " AND "
      . map (\(c1, c2) -> fullColName (tableName table) c1 <> " = " <> fullColName oTableName c2)
      <$> listToMaybe [ colPairs | ForeignKey tName colPairs <-  tableConstraints table
                                 , tName == oTableName ]

    coalesceFKId col =
      if "coalesce" `Text.isPrefixOf` col
        then col
        else "coalesce((" <> col <> "), -1)"

toSelectSQL :: FactTablePopulateSelectSQL -> Text
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


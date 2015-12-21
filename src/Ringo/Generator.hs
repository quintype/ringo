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
import Data.List            (nub, find, subsequences, partition, sortBy)
import Data.Maybe           (fromJust, mapMaybe, catMaybes)
import Data.Monoid          ((<>))
import Data.Ord             (comparing)
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

colNamesString :: [ColumnName] -> Text
colNamesString = Text.intercalate ", "

fullColName :: TableName -> ColumnName -> ColumnName
fullColName tName cName = tName <> "." <> cName

constraintDefnSQL :: Table -> TableConstraint -> [Text]
constraintDefnSQL Table {..} constraint =
  let alterTableSQL = "ALTER TABLE ONLY " <> tableName <> " ADD "
  in case constraint of
    PrimaryKey cName -> [ alterTableSQL <> "PRIMARY KEY (" <> cName <> ")" ]
    ForeignKey oTableName cNamePairs ->
      [ alterTableSQL <> "FOREIGN KEY (" <> colNamesString (map fst cNamePairs) <> ") REFERENCES "
          <> oTableName <> " (" <> colNamesString (map snd cNamePairs) <> ")" ]
    UniqueKey cNames -> let
        (notNullCols, nullCols) =
          both (map columnName)
          $ partition ((== NotNull) . columnNullable)
          $ catMaybes [ findColumn cName tableColumns | cName <- cNames ]
        combinations =
          map (\cs -> (cs, [ c | c <- nullCols, c `notElem` cs ]))
          . sortBy (comparing length)
          $ subsequences nullCols
      in [ "CREATE UNIQUE INDEX ON " <> tableName
              <> " (" <> colNamesString (notNullCols ++ nnCols) <> ")"
              <>  if null whereClauses
                    then ""
                    else "\nWHERE "<> Text.intercalate "\nAND " whereClauses
           | (nnCols, nCols) <- combinations
           , not $ null (notNullCols ++ nnCols)
           , let whereClauses =
                   [ c <> " IS NOT NULL" | c <- nnCols ] ++ [ c <> " IS NULL" | c <- nCols ] ]

tableDefnSQL :: Table -> [Text]
tableDefnSQL table@Table {..} =
  tableSQL : concatMap (constraintDefnSQL table) tableConstraints
  where
    tableSQL = "CREATE TABLE " <> tableName <> " (\n"
                 <> (Text.intercalate ",\n" . map columnDefnSQL $ tableColumns)
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

dimensionTablePopulateSQL :: TablePopulationMode -> Fact -> TableName -> Reader Env Text
dimensionTablePopulateSQL popMode fact dimTableName = do
  dimPrefix           <- settingDimPrefix <$> asks envSettings
  let colMapping      = dimColumnMapping dimPrefix fact dimTableName
      baseSelectC     = "SELECT DISTINCT\n" <> colNamesString (map snd colMapping) <> "\n"
                          <> "FROM " <> factTableName fact
      insertC selectC = "INSERT INTO " <> dimTableName
                          <> " (\n" <> colNamesString (map fst colMapping) <> "\n) "
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

factTablePopulateSQL :: TablePopulationMode -> Fact -> Reader Env Text
factTablePopulateSQL popMode fact = do
  Settings {..}    <- asks envSettings
  allDims          <- extractAllDimensionTables fact
  tables           <- asks envTables
  let fTableName   = factTableName fact
      table        = fromJust . findTable fTableName $ tables
      dimIdColName = settingDimTableIdColumnName

      timeUnitColumnInsertSQL cName =
        let colName = timeUnitColumnName dimIdColName cName settingTimeUnit
        in ( colName
           , "floor(extract(epoch from " <> fullColName fTableName cName <> ")/"
                <> Text.pack (show $ timeUnitToSeconds settingTimeUnit) <> ")"
           , True
           )

      factColMap = concatFor (factColumns fact) $ \col -> case col of
        DimTime cName            -> [ timeUnitColumnInsertSQL cName ]
        NoDimId cName            -> [ (cName, fullColName fTableName cName, True) ]
        FactCount cName          -> [ (cName, "count(*)", False) ]
        FactSum scName cName     -> [ (cName, "sum(" <> fullColName fTableName scName <> ")", False) ]
        FactAverage scName cName ->
          [ ( cName <> settingAvgCountColumSuffix
            , "count(" <> fullColName fTableName scName <> ")"
            , False
            )
          , ( cName <> settingAvgSumColumnSuffix
            , "sum(" <> fullColName fTableName scName <> ")"
            , False
            )
          ]
        _                        -> []

      dimColMap = for allDims $ \(dimFact, factTable@Table {..}) ->
        let colName             = factDimFKIdColumnName settingDimPrefix dimIdColName tableName
            factSourceTableName = factTableName dimFact
            insertSQL           = if factTable `elem` tables
              then fullColName factSourceTableName colName
              else let
                  dimLookupWhereClauses =
                    [ fullColName tableName c1 <> " = " <> fullColName factSourceTableName c2
                      | (c1, c2) <- dimColumnMapping settingDimPrefix dimFact tableName ]
                in "SELECT " <> dimIdColName <> " FROM " <> tableName <> "\nWHERE "
                     <> Text.intercalate "\n AND " dimLookupWhereClauses
        in (colName, insertSQL, True)

      colMap = [ (cName, if addAs then asName cName sql else sql, addAs)
                 | (cName, sql, addAs) <- factColMap ++ dimColMap ]

      joinClauses =
        mapMaybe (\tName -> (\p -> "LEFT JOIN " <> tName <> " ON "<> p) <$> joinClausePreds table tName)
        . nub
        . map (factTableName . fst)
        $ allDims

      timeCol     = fullColName fTableName $ head [ cName | DimTime cName <- factColumns fact ]

  return $ "INSERT INTO "
             <> extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit
             <> " (\n" <> unlineCols (map fst3 colMap) <> "\n)"
             <> "\nSELECT \n" <> unlineCols (map snd3 colMap)
             <> "\nFROM " <> fTableName <> "\n" <> Text.intercalate"\n" joinClauses
             <> (if popMode == IncrementalPopulation
                   then "\nWHERE " <> timeCol <> " > ? AND " <> timeCol <> " <= ?"
                   else "")
             <> "\nGROUP BY \n"
             <> unlineCols (map ((groupByColPrefix <>) . fst3) . filter thd3 $ colMap)
  where
    groupByColPrefix        = "xxff_"
    asName cName sql        = "(" <> sql <> ")" <> " as " <> groupByColPrefix <> cName
    unlineCols              = Text.intercalate ",\n "

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

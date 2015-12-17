module Ringo.Generator
       ( tableDefnSQL
       , factTableDefnSQL
       , dimensionTableInsertSQL
       , factTableInsertSQL
       ) where

import qualified Data.Text as Text

import Control.Monad.Reader (Reader, asks)
import Data.List            (intersperse, nub, find)
import Data.Maybe           (fromJust, mapMaybe)
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

colNamesString :: [ColumnName] -> Text
colNamesString cNames = Text.concat (intersperse ", " cNames)

constraintDefnSQL :: TableName -> TableConstraint -> Text
constraintDefnSQL tableName constraint =
  "ALTER TABLE ONLY " <> tableName <> " ADD "
    <> case constraint of
         PrimaryKey cName -> "PRIMARY KEY (" <> cName <> ")"
         UniqueKey cNames -> "UNIQUE (" <> colNamesString cNames <> ")"
         ForeignKey oTableName cNamePairs ->
           "FOREIGN KEY (" <> colNamesString (map fst cNamePairs) <> ") REFERENCES "
             <> oTableName <> " (" <> colNamesString (map snd cNamePairs) <> ")"

tableDefnSQL :: Table -> [Text]
tableDefnSQL Table {..} =
  tableSQL : map (constraintDefnSQL tableName) tableConstraints
  where
    tableSQL = "CREATE TABLE " <> tableName <> " (\n"
                 <> (Text.concat . intersperse ",\n" . map columnDefnSQL $ tableColumns)
                 <> "\n)"

factTableDefnSQL :: Fact -> Table -> Reader Env [Text]
factTableDefnSQL fact table = do
  Settings {..} <- asks envSettings
  allDims       <- extractAllDimensionTables fact

  let factCols  = flip mapMaybe (factColumns fact) $ \col -> case col of
        DimTime cName -> Just $ timeUnitColumnName settingDimTableIdColumnName cName settingTimeUnit
        NoDimId cName -> Just cName
        _             -> Nothing

      dimCols   = flip map allDims $ \(_, Table {..}) ->
        factDimFKIdColumnName settingDimPrefix settingDimTableIdColumnName tableName

      indexSQLs = flip map (factCols ++ dimCols) $ \col ->
        "CREATE INDEX ON " <> tableName table <> " USING btree (" <> col <> ")"
  return $ tableDefnSQL table ++ indexSQLs

dimColumnMapping :: Text -> Fact -> TableName -> [(ColumnName, ColumnName)]
dimColumnMapping dimPrefix fact dimTableName =
  flip mapMaybe (factColumns fact) $ \fCol -> case fCol of
    DimVal dName cName | dimPrefix <> dName == dimTableName ->
      Just (dimColumnName dName cName, cName)
    _ -> Nothing

dimensionTableInsertSQL :: Fact -> TableName -> Reader Env Text
dimensionTableInsertSQL fact dimTableName = do
  dimPrefix      <- settingDimPrefix <$> asks envSettings
  let colMapping = dimColumnMapping dimPrefix fact dimTableName

  return $ "INSERT INTO " <> dimTableName <> " (\n"
             <> colNamesString (map fst colMapping)
             <> "\n) SELECT DISTINCT \n"
             <> colNamesString (map snd colMapping)
             <> "\nFROM " <> factTableName fact

factTableInsertSQL :: Fact -> Reader Env Text
factTableInsertSQL fact = do
  let fTableName   = factTableName fact
  Settings {..}    <- asks envSettings
  allDims          <- extractAllDimensionTables fact
  tables           <- asks envTables
  let table        =  fromJust . findTable fTableName $ tables
      dimIdColName = settingDimTableIdColumnName

  let timeUnitColumnInsertSQL cName =
        let colName = timeUnitColumnName dimIdColName cName settingTimeUnit
        in (colName, "floor(extract(epoch from " <> fullColName fTableName cName <> ")/"
                        <> Text.pack (show $ timeUnitToSeconds settingTimeUnit) <> ")")

      factColMap = flip concatMap (factColumns fact) $ \col -> case col of
        DimTime cName            -> [ timeUnitColumnInsertSQL cName ]
        NoDimId cName            -> [ (cName, fullColName fTableName cName) ]
        FactCount cName          -> [ (cName, "count(*)") ]
        FactSum scName cName     -> [ (cName, "sum(" <> fullColName fTableName scName <> ")") ]
        FactAverage scName cName -> [ ( cName <> settingAvgCountColumSuffix
                                      , "count(" <> fullColName fTableName scName <> ")")
                                    , ( cName <> settingAvgSumColumnSuffix
                                      , "sum(" <> fullColName fTableName scName <> ")") ]
        _                        -> []

      dimColMap = flip map allDims $ \(dimFact, factTable@Table {..}) ->
        let colName             = factDimFKIdColumnName settingDimPrefix dimIdColName tableName
            factSourceTableName = factTableName dimFact
            insertSQL           =
              if factTable `elem` tables
               then fullColName factSourceTableName colName
               else
                let dimLookupWhereClauses =
                      map (\(c1, c2) ->
                            fullColName tableName c1 <> " = " <> fullColName factSourceTableName c2)
                      $ dimColumnMapping settingDimPrefix dimFact tableName
                in "SELECT " <> dimIdColName <> " FROM " <> tableName <> "\nWHERE "
                     <> (Text.concat . intersperse "\n AND " $ dimLookupWhereClauses)
        in (colName, insertSQL)

      colMap = map (\(cName, sql) -> (cName, asName cName sql)) $ factColMap ++ dimColMap

      joinClauses =
        mapMaybe (\tName -> (\p -> "LEFT JOIN " <> tName <> " ON "<> p) <$> joinClausePreds table tName)
        . nub
        . map (\(dimFact, _) -> factTableName dimFact)
        $ allDims

  return $ "INSERT INTO "
             <> extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit
             <> " (\n" <> Text.concat (intersperse ",\n " . map fst $ colMap) <> "\n)"
             <> "\nSELECT \n" <> Text.concat (intersperse ",\n " . map snd $ colMap)
             <> "\nFROM " <> fTableName <> "\n" <> Text.concat (intersperse "\n" joinClauses)
             <> "\nGROUP BY \n" <> Text.concat (intersperse ",\n " . map fst $ colMap)
  where
    fullColName tName cName = tName <> "." <> cName

    asName cName sql = "(" <> sql <> ")" <> " as " <> cName

    joinClausePreds table oTableName =
      fmap (\(ForeignKey _ colPairs) ->
              Text.concat . intersperse " AND "
              . map (\(c1, c2) -> fullColName (tableName table) c1 <> " = " <> fullColName oTableName c2)
              $ colPairs )
      . find (\cons -> case cons of
                  ForeignKey tName _ -> tName == oTableName
                  _                  -> False)
      . tableConstraints
      $ table

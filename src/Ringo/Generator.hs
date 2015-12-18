module Ringo.Generator
       ( tableDefnSQL
       , factTableDefnSQL
       , dimensionTableInsertSQL
       , factTableInsertSQL
       ) where

import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

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
  forMaybe (factColumns fact) $ \fCol -> case fCol of
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
                     <> (Text.concat . intersperse "\n AND " $ dimLookupWhereClauses)
        in (colName, insertSQL, True)

      colMap = [ (cName, if addAs then asName cName sql else sql, addAs)
                 | (cName, sql, addAs) <- factColMap ++ dimColMap ]

      joinClauses =
        mapMaybe (\tName -> (\p -> "LEFT JOIN " <> tName <> " ON "<> p) <$> joinClausePreds table tName)
        . nub
        . map (factTableName . fst)
        $ allDims

  return $ "INSERT INTO "
             <> extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit
             <> " (\n" <> unlineCols (map fst3 colMap) <> "\n)"
             <> "\nSELECT \n" <> unlineCols (map snd3 colMap)
             <> "\nFROM " <> fTableName <> "\n" <> Text.concat (intersperse "\n" joinClauses)
             <> "\nGROUP BY \n"
             <> unlineCols (map ((groupByColPrefix <>) . fst3) . filter thd3 $ colMap)
  where
    groupByColPrefix        = "xxff_"
    fullColName tName cName = tName <> "." <> cName
    asName cName sql        = "(" <> sql <> ")" <> " as " <> groupByColPrefix <> cName
    unlineCols              = Text.concat . intersperse ",\n "

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

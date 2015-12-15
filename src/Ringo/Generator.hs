module Ringo.Generator
       ( tableDefnSQL
       , dimensionTableInsertSQL
       ) where

import qualified Data.Text as Text

import Data.List   (intersperse)
import Data.Maybe  (mapMaybe)
import Data.Monoid ((<>))
import Data.Text   (Text)

import Ringo.Extractor.Internal
import Ringo.Types

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

dimensionTableInsertSQL :: Text -> Fact -> TableName -> Text
dimensionTableInsertSQL dimPrefix fact dimTableName = let
  colMapping = flip mapMaybe (factColumns fact) $ \fCol -> case fCol of
    DimVal dName cName | dimPrefix <> dName == dimTableName -> Just (dimColumnName dName cName, cName)
    _ -> Nothing
  in "INSERT INTO " <> dimTableName <> " (\n"
       <> colNamesString (map fst colMapping)
       <> "\n) SELECT DISTINCT \n"
       <> colNamesString (map snd colMapping)
       <> "\nFROM " <> factTableName fact

module Ringo.Generator
       ( tableDefnSQL
       ) where

import qualified Data.Text as Text

import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)

import Ringo.Types

nullableDefnSQL :: Nullable -> Text
nullableDefnSQL Null    = "NULL"
nullableDefnSQL NotNull = "NOT NULL"

columnDefnSQL :: Column -> Text
columnDefnSQL Column {..} =
  columnName <> " " <> columnType <> " " <> nullableDefnSQL columnNullable

constraintDefnSQL :: TableName -> TableConstraint -> Text
constraintDefnSQL tableName constraint =
  "ALTER TABLE ONLY " <> tableName <> " ADD "
    <> case constraint of
         PrimaryKey cName -> "PRIMARY KEY (" <> cName <> ")"
         UniqueKey cNames -> "UNIQUE (" <> colNamesStr cNames <> ")"
         ForeignKey oTableName cNamePairs ->
           "FOREIGN KEY (" <> colNamesStr (map fst cNamePairs) <> ") REFERENCES "
             <> oTableName <> " (" <> colNamesStr (map snd cNamePairs) <> ")"
  where
    colNamesStr cNames = Text.concat (intersperse ", " cNames)

tableDefnSQL :: Table -> [Text]
tableDefnSQL Table {..} =
  tableSQL : map (constraintDefnSQL tableName) tableConstraints
  where
    tableSQL = "CREATE TABLE " <> tableName <> " (\n"
                 <> (Text.concat . intersperse ",\n" . map columnDefnSQL $ tableColumns)
                 <> "\n)"

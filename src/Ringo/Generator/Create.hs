module Ringo.Generator.Create (tableDefnSQL, factTableDefnSQL) where

import Control.Monad.Reader (Reader, asks)
import Data.Monoid          ((<>))
import Data.Text            (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Types
import Ringo.Utils

tableDefnSQL :: Table -> [Text]
tableDefnSQL Table {..} =
  tableSQL : concatMap constraintDefnSQL tableConstraints
  where
    tableSQL = "CREATE TABLE " <> tableName <> " (\n"
                 <> (joinColumnNames . map columnDefnSQL $ tableColumns)
                 <> "\n)"

    columnDefnSQL Column {..} =
      columnName <> " " <> columnType <> " " <> nullableDefnSQL columnNullable

    nullableDefnSQL Null    = "NULL"
    nullableDefnSQL NotNull = "NOT NULL"

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

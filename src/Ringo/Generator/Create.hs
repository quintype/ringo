module Ringo.Generator.Create (tableDefnSQL, factTableDefnSQL) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader (Reader, asks)
import Data.Monoid          ((<>))
import Data.Text            (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Types
import Ringo.Utils

tableDefnSQL :: Table -> Reader Env [Text]
tableDefnSQL Table {..} = do
  Settings {..} <- asks envSettings
  let tabName = tableName <> settingTableNameSuffixTemplate

      tableSQL = "CREATE TABLE " <> tabName <> " (\n"
                   <> (joinColumnNames . map columnDefnSQL $ tableColumns)
                   <> "\n)"

      columnDefnSQL Column {..} =
        columnName <> " " <> columnType <> " " <> nullableDefnSQL columnNullable

      nullableDefnSQL Null    = "NULL"
      nullableDefnSQL NotNull = "NOT NULL"

      constraintDefnSQL constraint =
        let alterTableSQL = "ALTER TABLE ONLY " <> tabName <> " ADD "
        in case constraint of
          PrimaryKey cName -> [ alterTableSQL <> "PRIMARY KEY (" <> cName <> ")" ]
          ForeignKey oTableName cNamePairs ->
            [ alterTableSQL <> "FOREIGN KEY (" <> joinColumnNames (map fst cNamePairs) <> ") REFERENCES "
                <> oTableName <> " (" <> joinColumnNames (map snd cNamePairs) <> ")" ]
          UniqueKey cNames -> ["CREATE UNIQUE INDEX ON " <> tabName <> " (" <> joinColumnNames cNames <> ")"]

  return $ tableSQL : concatMap constraintDefnSQL tableConstraints

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

      indexSQLs = [ "CREATE INDEX ON " <> tableName table <> settingTableNameSuffixTemplate
                      <> " USING btree (" <> col <> ")"
                    | col <- factCols ++ dimCols ]

  (++ indexSQLs) <$> tableDefnSQL table

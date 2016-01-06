module Ringo.Generator.Create (dimensionTableDefnSQL, factTableDefnSQL) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (listToMaybe, maybeToList)
import Data.Monoid          ((<>))
import Data.Text            (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Types
import Ringo.Utils

tableDefnSQL :: Table -> Reader Env [Text]
tableDefnSQL Table {..} = do
  Settings {..} <- asks envSettings
  let tabName  = tableName <> settingTableNameSuffixTemplate

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

dimensionTableDefnSQL :: Table -> Reader Env [Text]
dimensionTableDefnSQL table@Table {..} = do
  Settings {..} <- asks envSettings
  let tabName        = tableName <> settingTableNameSuffixTemplate
      tablePKColName = head [ cName | PrimaryKey cName <- tableConstraints ]
      nonPKColNames  = [ cName | Column cName _ _ <- tableColumns, cName /= tablePKColName ]
      indexSQLs      = [ "CREATE INDEX ON " <> tabName <> " (" <> cName <> ")"
                         | cName <- nonPKColNames, length nonPKColNames > 1 ]
  (++ indexSQLs) <$> tableDefnSQL table

factTableDefnSQL :: Fact -> Table -> Reader Env [Text]
factTableDefnSQL fact table = do
  Settings {..} <- asks envSettings
  allDims       <- extractAllDimensionTables fact

  let dimTimeCol           = head [ cName | DimTime cName <- factColumns fact ]
      tenantIdCol          = listToMaybe [ cName | TenantId cName <- factColumns fact ]

      dimTimeColName cName = timeUnitColumnName settingDimTableIdColumnName cName settingTimeUnit

      factCols  = forMaybe (factColumns fact) $ \col -> case col of
        DimTime cName  -> Just $ dimTimeColName cName
        NoDimId cName  -> Just cName
        TenantId cName -> Just cName
        _              -> Nothing

      dimCols   = [ factDimFKIdColumnName settingDimPrefix settingDimTableIdColumnName tableName
                    | (_, Table {..}) <- allDims ]

      indexSQLs = [ "CREATE INDEX ON " <> tableName table <> settingTableNameSuffixTemplate
                      <> " USING btree (" <> col <> ")"
                    | col <- factCols ++ dimCols  ++ [ cName <> ", " <> dimTimeColName dimTimeCol
                                                       | cName <- maybeToList tenantIdCol ] ]

  (++ indexSQLs) <$> tableDefnSQL table

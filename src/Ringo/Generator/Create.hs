module Ringo.Generator.Create (tableDefnSQL, factTableDefnSQL) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader     (Reader, asks)
import Database.HsSqlPpp.Syntax ( Statement(..), RowConstraint(..), AlterTableAction(..)
                                , AlterTableOperation(..), Constraint(..), Cascade(..)
                                )
import Data.Monoid              ((<>))
import Data.Text                (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Sql
import Ringo.Types
import Ringo.Utils

tableDefnSQL :: Table -> Reader Env [Text]
tableDefnSQL table = map ppSQL <$> tableDefnSQL' table

tableDefnSQL' :: Table -> Reader Env [Statement]
tableDefnSQL' Table {..} = do
  Settings {..} <- asks envSettings
  let tabName = tableName <> settingTableNameSuffixTemplate

      tableSQL = CreateTable ea (name tabName) (map columnDefnSQL tableColumns) [] Nothing

      columnDefnSQL Column {..} =
        att columnName columnType $ nullableDefnSQL columnNullable

      nullableDefnSQL Null    = NullConstraint ea ""
      nullableDefnSQL NotNull = NotNullConstraint ea ""

      constraintDefnSQL constraint =
        let constr = case constraint of
              PrimaryKey cName -> PrimaryKeyConstraint ea "" [nmc cName]
              ForeignKey oTableName cNamePairs ->
                ReferenceConstraint ea "" (map (nmc . fst) cNamePairs)
                  (name oTableName) (map (nmc . snd) cNamePairs) Restrict Restrict
              UniqueKey cNames -> UniqueConstraint ea "" $ map nmc cNames

        in AlterTable ea (name tabName) $ AlterTableActions ea [AddConstraint ea constr]

  return $ tableSQL : map constraintDefnSQL tableConstraints

factTableDefnSQL :: Fact -> Table -> Reader Env [Text]
factTableDefnSQL fact table = do
  ds <- map ppSQL <$> tableDefnSQL' table
  is <- map (\st -> ppSQL st <> ";\n") <$> factTableIndexSQL' fact table
  return $ ds ++ is

factTableIndexSQL' :: Fact -> Table -> Reader Env [Statement]
factTableIndexSQL' fact table = do
  Settings {..} <- asks envSettings
  allDims       <- extractAllDimensionTables fact

  let factCols  = forMaybe (factColumns fact) $ \col -> case col of
        DimTime cName -> Just $ timeUnitColumnName settingDimTableIdColumnName cName settingTimeUnit
        NoDimId cName -> Just cName
        _             -> Nothing

      dimCols   = [ factDimFKIdColumnName settingDimPrefix settingDimTableIdColumnName tableName
                    | (_, Table {..}) <- allDims ]

  return [ CreateIndexTSQL ea (nmc "") (name $ tableName table <> settingTableNameSuffixTemplate) [nmc col]
           | col <- factCols ++ dimCols ]


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Ringo.Generator.Create (dimensionTableDefnSQL, factTableDefnSQL) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Control.Monad.Reader     (Reader, asks)
import Database.HsSqlPpp.Syntax ( Statement(..), RowConstraint(..), AlterTableAction(..)
                                , AlterTableOperation(..), Constraint(..), Cascade(..) )
import Data.Maybe               (listToMaybe, maybeToList)
import Data.Monoid              ((<>))
import Data.Text                (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Sql
import Ringo.Types
import Ringo.Utils

tableDefnStmts :: Table -> Reader Env [Statement]
tableDefnStmts Table {..} = do
  Settings {..} <- asks envSettings
  let tabName  = tableName <> settingTableNameSuffixTemplate

      tableSQL = CreateTable ea (name tabName) (map columnDefnSQL tableColumns) [] Nothing

      columnDefnSQL Column {..} =
        attDef columnName columnType $ nullableDefnSQL columnNullable

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

tableDefnSQL :: Table -> (Table -> Reader Env [Statement]) -> Reader Env [Text]
tableDefnSQL table indexFn = do
  ds <- map ppStatement <$> tableDefnStmts table
  is <- map (\st -> ppStatement st <> ";\n") <$> indexFn table
  return $ ds ++ is

dimensionTableDefnSQL :: Table -> Reader Env [Text]
dimensionTableDefnSQL table = tableDefnSQL table dimensionTableIndexStmts

dimensionTableIndexStmts :: Table -> Reader Env [Statement]
dimensionTableIndexStmts Table {..} = do
  Settings {..} <- asks envSettings
  let tabName        = tableName <> settingTableNameSuffixTemplate
      tablePKColName = head [ cName | PrimaryKey cName <- tableConstraints ]
      nonPKColNames  = [ cName | Column cName _ _ <- tableColumns, cName /= tablePKColName ]

  return [ CreateIndexTSQL ea (nmc "") (name tabName) [nmc cName]
           | cName <- nonPKColNames, length nonPKColNames > 1 ]

factTableDefnSQL :: Fact -> Table -> Reader Env [Text]
factTableDefnSQL fact table = tableDefnSQL table (factTableIndexStmts fact)

factTableIndexStmts :: Fact -> Table -> Reader Env [Statement]
factTableIndexStmts fact table = do
  Settings {..} <- asks envSettings
  tables        <- asks envTables
  allDims       <- extractAllDimensionTables fact

  let dimTimeCol           = head [ cName | DimTime cName <- factColumns fact ]
      tenantIdCol          = listToMaybe [ cName | TenantId cName <- factColumns fact ]
      tabName              = tableName table <> settingTableNameSuffixTemplate
      dimTimeColName cName = timeUnitColumnName settingDimTableIdColumnName cName settingTimeUnit

      factCols = forMaybe (factColumns fact) $ \col -> case col of
        DimTime cName  -> Just [dimTimeColName cName]
        NoDimId cName  -> Just [cName]
        TenantId cName -> Just [cName]
        _              -> Nothing

      dimCols  = [ [ factDimFKIdColumnName settingDimPrefix settingDimTableIdColumnName dimFact dimTable tables ]
                   | (dimFact, dimTable) <- allDims ]

  return [ CreateIndexTSQL ea (nmc "") (name $ tabName) (map nmc cols)
           | cols <- factCols ++ dimCols ++ [ [cName, dimTimeColName dimTimeCol]
                                                       | cName <- maybeToList tenantIdCol ] ]

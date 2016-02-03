{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Ringo.Generator.Internal where

import qualified Data.Map as Map
import qualified Data.Text as Text

import Database.HsSqlPpp.Syntax (ScalarExpr)
import Data.List                (find)
import Data.Monoid              ((<>))
import Data.Text                (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Sql
import Ringo.Types

dimColumnMapping :: Text -> Fact -> TableName -> [(ColumnName, ColumnName)]
dimColumnMapping dimPrefix fact dimTableName =
  [ (dimColumnName factColTargetTable factColTargetColumn, factColTargetColumn)
    | FactColumn { factColType = DimVal {..}, ..} <- factColumns fact
    , dimPrefix <> factColTargetTable == dimTableName ]

coalesceColumn :: TypeDefaults -> TableName -> Column -> ScalarExpr
coalesceColumn defaults tName Column{..} =
  if columnNullable == Null
    then app "coalesce" [fqColName, num $ defVal columnType]
    else fqColName
  where
    fqColName = eqi tName columnName

    defVal colType =
      maybe (error $ "Default value not known for column type: " ++ Text.unpack colType) snd
      . find (\(k, _) -> k `Text.isPrefixOf` colType)
      . Map.toList
      $ defaults

suffixTableName :: TablePopulationMode -> Text -> TableName -> TableName
suffixTableName popMode suffix tableName = case popMode of
  FullPopulation        -> tableName <> suffix
  IncrementalPopulation -> tableName

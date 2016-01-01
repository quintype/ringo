{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ringo.Generator.Internal where

import qualified Data.Map as Map
import qualified Data.Text as Text

import Data.List   (find)
import Data.Monoid ((<>))
import Data.Text   (Text)

import Ringo.Extractor.Internal
import Ringo.Types

joinColumnNames :: [ColumnName] -> Text
joinColumnNames = Text.intercalate ",\n"

fullColumnName :: TableName -> ColumnName -> ColumnName
fullColumnName tName cName = tName <> "." <> cName

dimColumnMapping :: Text -> Fact -> TableName -> [(ColumnName, ColumnName)]
dimColumnMapping dimPrefix fact dimTableName =
  [ (dimColumnName dName cName, cName)
    | DimVal dName cName <- factColumns fact
    , dimPrefix <> dName == dimTableName ]

coalesceColumn :: TypeDefaults -> TableName -> Column -> Text
coalesceColumn defaults tName Column{..} =
  if columnNullable == Null
    then "coalesce(" <> fqColName <> ", " <> defVal columnType <> ")"
    else fqColName
  where
    fqColName = fullColumnName tName columnName

    defVal colType =
      maybe (error $ "Default value not known for column type: " ++ Text.unpack colType) snd
      . find (\(k, _) -> k `Text.isPrefixOf` colType)
      . Map.toList
      $ defaults

suffixTableName :: TablePopulationMode -> Text -> TableName -> TableName
suffixTableName popMode suffix tableName = case popMode of
  FullPopulation        -> tableName <> suffix
  IncrementalPopulation -> tableName

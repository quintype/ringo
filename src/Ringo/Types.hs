{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Ringo.Types where

import qualified Data.Text as Text

import Data.Map    (Map)
import Data.Monoid ((<>))
import Data.Text   (Text)

showColNames :: [Text] -> String
showColNames cols = Text.unpack $ "(" <> Text.intercalate ", " cols <> ")"

type ColumnName = Text
type ColumnType = Text
type TableName  = Text

data Nullable = Null | NotNull deriving (Eq, Enum)

instance Show Nullable where
  show Null    = "NULL"
  show NotNull = "NOT NULL"

data Column = Column
              { columnName     :: !ColumnName
              , columnType     :: !ColumnType
              , columnNullable :: !Nullable
              } deriving (Eq)

instance Show Column where
  show Column {..} = "Column "
                       ++ Text.unpack columnName ++ " "
                       ++ Text.unpack columnType ++ " "
                       ++ show columnNullable

data TableConstraint = PrimaryKey !ColumnName
                     | UniqueKey  ![ColumnName]
                     | ForeignKey !TableName ![(ColumnName, ColumnName)]
                     deriving (Eq)

instance Show TableConstraint where
  show (PrimaryKey col)          = "PrimaryKey " ++ Text.unpack col
  show (UniqueKey cols)          = "UniqueKey " ++ showColNames cols
  show (ForeignKey tName colMap) = "ForeignKey " ++ showColNames (map fst colMap) ++ " "
                                     ++ Text.unpack tName ++  " " ++ showColNames (map snd colMap)

data Table = Table
             { tableName        :: !TableName
             , tableColumns     :: ![Column]
             , tableConstraints :: ![TableConstraint]
             } deriving (Eq)

instance Show Table where
  show Table {..} =
    unlines $ ("Table " ++ Text.unpack tableName) : (map show tableColumns) ++ (map show tableConstraints)

data TimeUnit = Second | Minute | Hour | Day | Week
                deriving (Eq, Enum, Show, Read)

timeUnitName :: TimeUnit -> Text
timeUnitName = Text.toLower . Text.pack . show

timeUnitToSeconds :: TimeUnit -> Int
timeUnitToSeconds Second = 1
timeUnitToSeconds Minute = 60 * timeUnitToSeconds Second
timeUnitToSeconds Hour   = 60 * timeUnitToSeconds Minute
timeUnitToSeconds Day    = 24 * timeUnitToSeconds Hour
timeUnitToSeconds Week   = 7  * timeUnitToSeconds Day

data Fact = Fact
            { factName            :: !TableName
            , factTableName       :: !TableName
            , factTablePersistent :: !Bool
            , factParentNames     :: ![TableName]
            , factColumns         :: ![FactColumn]
            } deriving (Eq, Show)

data FactColumn = DimTime           !ColumnName
                | NoDimId           !ColumnName
                | TenantId          !ColumnName
                | DimId             !TableName          !ColumnName
                | DimVal            !TableName          !ColumnName
                | FactCount         !(Maybe ColumnName) !ColumnName
                | FactSum           !ColumnName         !ColumnName
                | FactAverage       !ColumnName         !ColumnName
                | FactCountDistinct !(Maybe ColumnName) !ColumnName
                | FactMax           !ColumnName         !ColumnName
                | FactMin           !ColumnName         !ColumnName
                deriving (Eq, Show)

factSourceColumnName :: FactColumn -> Maybe ColumnName
factSourceColumnName (DimTime cName)             = Just cName
factSourceColumnName (NoDimId cName)             = Just cName
factSourceColumnName (TenantId cName)            = Just cName
factSourceColumnName (DimId _ cName)             = Just cName
factSourceColumnName (DimVal _ cName)            = Just cName
factSourceColumnName (FactCount cName _)         = cName
factSourceColumnName (FactSum cName _)           = Just cName
factSourceColumnName (FactAverage cName _)       = Just cName
factSourceColumnName (FactCountDistinct cName _) = cName
factSourceColumnName (FactMax cName _)           = Just cName
factSourceColumnName (FactMin cName _)           = Just cName

data Settings = Settings
                { settingDimPrefix                  :: !Text
                , settingFactPrefix                 :: !Text
                , settingTimeUnit                   :: !TimeUnit
                , settingAvgCountColumSuffix        :: !Text
                , settingAvgSumColumnSuffix         :: !Text
                , settingDimTableIdColumnName       :: !Text
                , settingDimTableIdColumnType       :: !Text
                , settingFactCountColumnType        :: !Text
                , settingFactCountDistinctErrorRate :: !Double
                , settingFactInfix                  :: !Text
                , settingDependenciesJSONFileName   :: !Text
                , settingFactsJSONFileName          :: !Text
                , settingDimensionJSONFileName      :: !Text
                , settingForeignKeyIdCoalesceValue  :: !Int
                , settingTableNameSuffixTemplate    :: !Text
                } deriving (Eq, Show)

defSettings :: Settings
defSettings = Settings
              { settingDimPrefix                  = "dim_"
              , settingFactPrefix                 = "fact_"
              , settingTimeUnit                   = Minute
              , settingAvgCountColumSuffix        = "_count"
              , settingAvgSumColumnSuffix         = "_sum"
              , settingDimTableIdColumnName       = "id"
              , settingDimTableIdColumnType       = "serial"
              , settingFactCountColumnType        = "integer"
              , settingFactCountDistinctErrorRate = 0.05
              , settingFactInfix                  = "_by_"
              , settingDependenciesJSONFileName   = "dependencies.json"
              , settingFactsJSONFileName          = "facts.json"
              , settingDimensionJSONFileName      = "dimensions.json"
              , settingForeignKeyIdCoalesceValue  = -1
              , settingTableNameSuffixTemplate    = "{{suff}}"
              }

data ValidationError = MissingTable             !TableName
                     | MissingFact              !TableName
                     | MissingColumn            !TableName !ColumnName
                     | MissingTimeColumn        !TableName
                     | MissingNotNullConstraint !TableName !ColumnName
                     | MissingTypeDefault       !Text
                     deriving (Eq, Show)

type TypeDefaults = Map Text Text

data Env = Env
           { envTables       :: ![Table]
           , envFacts        :: ![Fact]
           , envSettings     :: !Settings
           , envTypeDefaults :: !TypeDefaults
           } deriving (Eq, Show)

data TablePopulationMode = FullPopulation | IncrementalPopulation deriving (Eq, Show)

type Dependencies = Map TableName [TableName]

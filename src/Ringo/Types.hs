module Ringo.Types where

import qualified Data.Text as Text

import Data.Map  (Map)
import Data.Text (Text)

type ColumnName = Text
type ColumnType = Text
type TableName  = Text

data Nullable = Null | NotNull deriving (Eq, Enum, Show)

data Column = Column
              { columnName     :: !ColumnName
              , columnType     :: !ColumnType
              , columnNullable :: !Nullable
              } deriving (Eq, Show)

data TableConstraint = PrimaryKey !ColumnName
                     | UniqueKey  ![ColumnName]
                     | ForeignKey !TableName ![(ColumnName, ColumnName)]
                     deriving (Eq, Show)

data Table = Table
             { tableName        :: !TableName
             , tableColumns     :: ![Column]
             , tableConstraints :: ![TableConstraint]
             } deriving (Eq, Show)

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
            { factName        :: !TableName
            , factTableName   :: !TableName
            , factParentNames :: ![TableName]
            , factColumns     :: ![FactColumn]
            } deriving (Eq, Show)

data FactColumn = DimTime           !ColumnName
                | NoDimId           !ColumnName
                | DimId             !TableName          !ColumnName
                | DimVal            !TableName          !ColumnName
                | FactCount         !(Maybe ColumnName) !ColumnName
                | FactSum           !ColumnName         !ColumnName
                | FactAverage       !ColumnName         !ColumnName
                | FactCountDistinct !(Maybe ColumnName) !ColumnName
                deriving (Eq, Show)

factColumnName :: FactColumn -> Maybe ColumnName
factColumnName (DimTime cName)             = Just cName
factColumnName (NoDimId cName)             = Just cName
factColumnName (DimId _ cName)             = Just cName
factColumnName (DimVal _ cName)            = Just cName
factColumnName (FactCount cName _)         = cName
factColumnName (FactSum cName _)           = Just cName
factColumnName (FactAverage cName _)       = Just cName
factColumnName (FactCountDistinct cName _) = cName

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

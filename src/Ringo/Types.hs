module Ringo.Types where

import           Data.Text (Text)
import qualified Data.Text as T

type ColumnName = Text
type ColumnType = Text
type TableName = Text

data Nullable = Null | NotNull deriving (Eq, Enum, Show)

data Column = Column
              { columnName     :: !ColumnName
              , columnType     :: !ColumnType
              , columnNullable :: !Nullable
              } deriving (Eq, Show)

data TableContraint = PrimaryKey !ColumnName
                    | UniqueKey  ![ColumnName]
                    | ForeignKey !TableName ![(ColumnName, ColumnName)]
                    deriving (Eq, Show)

data Table = Table
             { tableName        :: !TableName
             , tableColumns     :: ![Column]
             , tableConstraints :: ![TableContraint]
             } deriving (Eq, Show)

data TimeUnit = Second | Minute | Hour | Day | Week | Month | Year
                deriving (Eq, Enum, Show)

timeUnitName :: TimeUnit -> Text
timeUnitName = T.toLower . T.pack . show

data Fact = Fact
            { factName        :: !TableName
            , factTableName   :: !TableName
            , factParentNames :: ![TableName]
            , factColumns     :: ![FactColumn]
            } deriving (Eq, Show)

data FactValType = Count | Sum | Average | CountDistinct deriving (Eq, Enum, Show)

data FactColumn = DimTime           !ColumnName
                | NoDimId           !ColumnName
                | DimId             !TableName  !ColumnName
                | DimVal            !TableName  !ColumnName
                | FactCount         !ColumnName
                | FactSum           !ColumnName !ColumnName
                | FactAverage       !ColumnName !ColumnName
                | FactCountDistinct !ColumnName
                deriving (Eq, Show)

factColumnName :: FactColumn -> Maybe ColumnName
factColumnName (DimTime cName)       = Just cName
factColumnName (NoDimId cName)       = Just cName
factColumnName (DimId _ cName)       = Just cName
factColumnName (DimVal _ cName)      = Just cName
factColumnName (FactCount _)         = Nothing
factColumnName (FactSum cName _)     = Just cName
factColumnName (FactAverage cName _) = Just cName
factColumnName (FactCountDistinct _) = Nothing

data Settings = Settings
                { settingDimPrefix  :: !Text
                , settingFactPrefix :: !Text
                , settingTimeUnit   :: !TimeUnit
                } deriving (Eq, Show)

defSettings :: Settings
defSettings = Settings
              { settingDimPrefix  = "dim_"
              , settingFactPrefix = "fact_"
              , settingTimeUnit   = Minute
              }

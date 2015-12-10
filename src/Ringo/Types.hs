module Ringo.Types where

import           Data.Text (Text)

(&) :: a -> (a -> b) -> b
x & f = f x

type ColumnName = Text
type ColumnType = Text
type TableName = Text

data Nullable = Nullable | NotNullable deriving (Eq, Enum, Show)

data Column = Column
              { columnName        :: ColumnName
              , columnType        :: ColumnType
              , columnNullable    :: Nullable
              } deriving (Eq, Show)

data TableContraint = PrimaryKey ColumnName
                    | UniqueKey [ColumnName]
                    | ForeignKey TableName [(ColumnName, ColumnName)]
                    deriving (Eq, Show)

data Table = Table
             { tableName        :: TableName
             , tableColumns     :: [Column]
             , tableConstraints :: [TableContraint]
             } deriving (Eq, Show)

data TimeUnit = Second | Minute | Hour | Day | Week | Month | Year
                deriving (Eq, Enum, Show)

data Fact = Fact
            { factName      :: TableName
            , factTableName :: TableName
            , factColumns   :: [FactColumn]
            } deriving (Eq, Show)

data FactColumn = DimTime ColumnName
                | NoDimId ColumnName
                | DimId TableName ColumnName
                | DimVal TableName ColumnName
                deriving (Eq, Show)

factColumnName :: FactColumn -> ColumnName
factColumnName (DimTime cName)  = cName
factColumnName (NoDimId cName)  = cName
factColumnName (DimId _ cName)  = cName
factColumnName (DimVal _ cName) = cName

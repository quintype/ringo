module Ringo.Types where

import           Data.Text (Text)

(&) :: a -> (a -> b) -> b
x & f = f x

type ColumnName = Text
type ColumnType = Text
type TableName = Text

data Column = Column
              { columnName     :: ColumnName
              , columnType     :: ColumnType
              , columnNullable :: Bool
              , columnDefault  :: Maybe Text
              } deriving (Eq, Show)

data ColumnRef = ColumnRef ColumnName deriving (Eq, Show)

data TableContraint = PrimaryKey ColumnRef
                    | UniqueKey [ColumnRef]
                    | ForeignKey TableRef [(ColumnRef, ColumnRef)]
                    deriving (Eq, Show)

data Table = Table
             { tableName :: TableName
             , tableColumns :: [Column]
             , tableConstraints :: [TableContraint]
             } deriving (Eq, Show)

data TableRef = TableRef TableName deriving (Eq, Show)

column :: ColumnName -> ColumnType -> Column
column cname ctype = Column cname ctype True Nothing

colNotNull :: Column -> Column
colNotNull c = c { columnNullable = False }

colDefault :: Text -> Column -> Column
colDefault cdefault c = c { columnDefault = Just cdefault }

primaryKey :: ColumnName -> TableContraint
primaryKey = PrimaryKey . ColumnRef

uniqueKey :: [ColumnName] -> TableContraint
uniqueKey = UniqueKey . map ColumnRef

foreignKey :: TableName -> [(ColumnName, ColumnName)] -> TableContraint
foreignKey tableName =
  ForeignKey (TableRef tableName) . map (\(c1, c2) -> (ColumnRef c1, ColumnRef c2))

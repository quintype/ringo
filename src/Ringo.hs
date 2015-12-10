module Ringo where

import Ringo.Types

import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (nub)
import qualified Data.Text as T

data ValidationError = MissingTable TableName
                     | MissingColumn TableName ColumnName
                     deriving (Eq, Show)

indexList :: Ord k => (a -> k) -> [a] -> Map.Map k a
indexList f = Map.fromList . map (\x -> (f x, x))

checkTableForCol :: Table -> ColumnName -> [ValidationError]
checkTableForCol tab colName =
  [MissingColumn (tableName tab) colName |
    not . any ((colName ==) . columnName) . tableColumns $ tab]

validateTable :: [Table] -> Table -> [ValidationError]
validateTable tables table = concatMap checkConstraint . tableConstraints $ table
  where
    tableMap = indexList tableName tables

    checkConstraint (PrimaryKey colName)    = checkTableForCol table colName
    checkConstraint (UniqueKey columnNames) = checkTableForColRefs table columnNames
    checkConstraint (ForeignKey oTableName columnNames) =
      case Map.lookup oTableName tableMap of
        Just oTable ->
          checkTableForColRefs table (map fst columnNames)
          ++ checkTableForColRefs oTable (map snd columnNames)
        Nothing     -> [MissingTable oTableName]

    checkTableForColRefs tab = concatMap (checkTableForCol tab)

validateFact :: [Table] -> Fact -> [ValidationError]
validateFact tables Fact {..} =
  case Map.lookup factTableName tableMap of
    Nothing    -> [MissingTable factTableName]
    Just table -> concatMap (checkColumn table) factColumns
  where
    tableMap = indexList tableName tables

    checkColumn table = checkTableForCol table . factColumnName

extractDimensions :: T.Text -> Table -> Fact -> [Table]
extractDimensions prefix Table {..} Fact {..} =
  map (\(dim, cols) -> Table { tableName        = T.concat [prefix, dim]
                             , tableColumns     = Column "id" "serial" NotNullable : cols
                             , tableConstraints = [ PrimaryKey "id"
                                                  , UniqueKey (map columnName cols)
                                                  ]
                             })
  . Map.toList
  . Map.mapWithKey (\dim -> map (cleanColumn dim) . nub)
  . Map.fromListWith (++)
  . mapMaybe (\fcol -> do
                DimVal d col <- fcol
                column       <- Map.lookup col columnMap
                return (d, [column]))
  . map Just
  $ factColumns
  where
    columnMap = indexList columnName tableColumns

    cleanColumn dim col@Column {..} =
      col { columnName = fromMaybe columnName . T.stripPrefix (T.snoc dim '_') $ columnName }

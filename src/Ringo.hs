module Ringo where

import Ringo.Types
-- import qualified Ringo.Tables as Tables

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Data.Maybe  (mapMaybe, fromMaybe, fromJust)
import Data.Monoid ((<>))
import Data.List   (nub, find)
import Data.Text   (Text)

data ValidationError = MissingTable TableName
                     | MissingFact TableName
                     | MissingColumn TableName ColumnName
                     deriving (Eq, Show)

findTable :: TableName -> [Table] -> Maybe Table
findTable tName = find ((== tName) . tableName)

findFact :: TableName -> [Fact] -> Maybe Fact
findFact fName = find ((== fName) . factName)

findColumn :: ColumnName -> [Column] -> Maybe Column
findColumn cName = find ((== cName) . columnName)

checkTableForCol :: Table -> ColumnName -> [ValidationError]
checkTableForCol tab colName =
  [ MissingColumn (tableName tab) colName |
      not . any ((colName ==) . columnName) . tableColumns $ tab ]

validateTable :: [Table] -> Table -> [ValidationError]
validateTable tables table = concatMap checkConstraint . tableConstraints $ table
  where
    checkConstraint (PrimaryKey colName)    = checkTableForCol table colName
    checkConstraint (UniqueKey columnNames) = checkTableForColRefs table columnNames
    checkConstraint (ForeignKey oTableName columnNames) =
      case findTable oTableName tables of
        Just oTable -> checkTableForColRefs table (map fst columnNames)
                         ++ checkTableForColRefs oTable (map snd columnNames)
        Nothing     -> [ MissingTable oTableName ]

    checkTableForColRefs tab = concatMap (checkTableForCol tab)

validateFact :: [Table] -> [Fact] -> Fact -> [ValidationError]
validateFact tables facts Fact {..} =
  case findTable factTableName tables of
    Nothing    -> [ MissingTable factTableName ]
    Just table -> validateTable tables table
                    ++ concatMap checkFactParents factParentNames
                    ++ concatMap (checkColumn table) factColumns
  where
    checkFactParents fName = case findFact fName facts of
      Nothing    -> [ MissingFact fName ]
      Just pFact -> validateFact tables facts pFact
    checkColumn table = maybe [] (checkTableForCol table) . factColumnName

withFactValidation :: [Table] -> [Fact] -> Fact -> (Table -> a) -> Either [ValidationError] a
withFactValidation tables facts fact func =
  let errors = validateFact tables facts fact
  in if not $ null errors
    then Left errors
    else Right . func . fromJust $ findTable (factTableName fact) tables

extractDimensions' :: [Table] -> Text -> Fact -> Table -> [Table]
extractDimensions' tables prefix fact Table {..} = dimsFromIds ++ dimsFromVals
  where
    dimsFromIds =
      flip mapMaybe (factColumns fact) $ \fcol -> case fcol of
        DimId d _ -> findTable d tables
        _         -> Nothing

    dimsFromVals =
      map (\(dim, cols) -> Table { tableName        = prefix <> dim
                                 , tableColumns     = Column "id" "serial" NotNull : cols
                                 , tableConstraints = [ PrimaryKey "id"
                                                      , UniqueKey (map columnName cols)
                                                      ]
                                 })
      . Map.toList
      . Map.mapWithKey (\dim -> map (cleanColumn dim) . nub)
      . Map.fromListWith (flip (++))
      . mapMaybe (\fcol -> do
                    DimVal d col <- fcol
                    column       <- findColumn col tableColumns
                    return (d, [ column ]))
      . map Just
      . factColumns
      $ fact

    cleanColumn dim col@Column {..} =
      col { columnName = fromMaybe columnName . Text.stripPrefix (dim <> "_") $ columnName }

extractDimensions :: [Table] -> [Fact] -> Text -> Fact -> Either [ValidationError] [Table]
extractDimensions tables facts prefix fact =
  withFactValidation tables facts fact $ extractDimensions' tables prefix fact

extractAllDimensions' :: [Table] -> [Fact] -> Text -> Fact -> Table -> [Table]
extractAllDimensions' tables facts dimPrefix fact table =
  nub $ extractDimensions' tables dimPrefix fact table ++ concatMap extract (factParentNames fact)
  where
    extract fName =
      let pFact      = fromJust . findFact fName $ facts
          pFactTable = fromJust . findTable (factTableName pFact) $ tables
      in extractAllDimensions' tables facts dimPrefix pFact pFactTable

extractFactTable :: [Table] -> [Fact] -> Settings -> Fact -> Either [ValidationError] Table
extractFactTable tables facts Settings {..} fact  =
  withFactValidation tables facts fact $ \table ->
    let intType                  = "integer"
        allDims                  = extractAllDimensions' tables facts settingDimPrefix fact table
        sourceColumnType colName = columnType . fromJust . findColumn colName . tableColumns $ table

        columns = flip concatMap (factColumns fact) $ \col -> case col of
          DimTime cName            -> [ Column (timeUnitColName cName) intType NotNull ]
          NoDimId cName            -> [ fromJust . findColumn cName . tableColumns $ table]
          FactCount cName          -> [ Column cName intType NotNull ]
          FactSum scName cName     -> [ Column cName (sourceColumnType scName) NotNull ]
          FactAverage scName cName -> [ Column (cName <> "_count") intType NotNull
                                      , Column (cName <> "_sum") (sourceColumnType scName) NotNull
                                      ]
          FactCountDistinct cName  -> [ Column (cName <> "_hll") (intType <> "[]") NotNull ]
          _                        -> []

        fks = flip map allDims $ \Table { .. } ->
          let colName     = fromMaybe tableName (Text.stripPrefix settingDimPrefix tableName) <> "_id"
              colNullable = if any ((== Null) . columnNullable) tableColumns then Null else NotNull
          in (Column colName intType colNullable, ForeignKey tableName [(colName, "id")])

        ukColNames =
          (++ map (columnName . fst) fks)
          . flip mapMaybe (factColumns fact) $ \col -> case col of
              DimTime cName -> Just (timeUnitColName cName)
              NoDimId cName -> Just cName
              _             -> Nothing

    in Table { tableName        = settingFactPrefix <> factName fact
             , tableColumns     = columns ++ map fst fks
             , tableConstraints = UniqueKey ukColNames : map snd fks
             }
  where
    timeUnitColName colName = colName <> "_" <> timeUnitName settingTimeUnit <> "_id"

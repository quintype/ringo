module Ringo where

import Ringo.Types
-- import qualified Ringo.Tables as Tables

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Control.Monad.Reader (Reader, asks)
import Data.Maybe  (mapMaybe, fromMaybe, fromJust)
import Data.Monoid ((<>))
import Data.List   (nub, find)

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

validateTable :: Table -> Reader Env [ValidationError]
validateTable table = do
  tables <- asks envTables
  return . concatMap (checkConstraint tables) . tableConstraints $ table
  where
    checkConstraint _ (PrimaryKey colName)    = checkTableForCol table colName
    checkConstraint _ (UniqueKey columnNames) = checkTableForColRefs table columnNames
    checkConstraint tables (ForeignKey oTableName columnNames) =
      case findTable oTableName tables of
        Just oTable -> checkTableForColRefs table (map fst columnNames)
                         ++ checkTableForColRefs oTable (map snd columnNames)
        Nothing     -> [ MissingTable oTableName ]

    checkTableForColRefs tab = concatMap (checkTableForCol tab)

validateFact :: Fact -> Reader Env [ValidationError]
validateFact Fact {..} = do
  tables <- asks envTables
  case findTable factTableName tables of
    Nothing    -> return [ MissingTable factTableName ]
    Just table -> do
      tableVs   <- validateTable table
      parentVs  <- concat <$> mapM checkFactParents factParentNames
      let colVs = concatMap (checkColumn table) factColumns
      return $ tableVs ++ parentVs ++ colVs
  where
    checkFactParents fName = do
      facts  <- asks envFacts
      case findFact fName facts of
        Nothing    -> return [ MissingFact fName ]
        Just pFact -> validateFact pFact

    checkColumn table = maybe [] (checkTableForCol table) . factColumnName

withFactValidation :: Fact -> (Table -> Reader Env a) -> Reader Env (Either [ValidationError] a)
withFactValidation fact func = do
  tables <- asks envTables
  errors <- validateFact fact
  if not $ null errors
    then return $ Left errors
    else fmap Right . func . fromJust . findTable (factTableName fact) $ tables

extractDimensions' :: Fact -> Table -> Reader Env [Table]
extractDimensions' fact Table {..} = do
  tables <- asks envTables
  prefix <- settingDimPrefix <$> asks envSettings
  return $ dimsFromIds tables ++ dimsFromVals prefix
  where
    dimsFromIds tables =
      flip mapMaybe (factColumns fact) $ \fcol -> case fcol of
        DimId d _ -> findTable d tables
        _         -> Nothing

    dimsFromVals prefix =
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

extractDimensions :: Fact -> Reader Env (Either [ValidationError] [Table])
extractDimensions fact = withFactValidation fact $ extractDimensions' fact

extractAllDimensions' :: Fact -> Table -> Reader Env [Table]
extractAllDimensions' fact table = do
  myDims     <- extractDimensions' fact table
  parentDims <- concat <$> mapM extract (factParentNames fact)
  return . nub $ myDims ++ parentDims
  where
    extract fName = do
      tables <- asks envTables
      facts  <- asks envFacts
      let pFact      = fromJust . findFact fName $ facts
          pFactTable = fromJust . findTable (factTableName pFact) $ tables
      extractAllDimensions' pFact pFactTable

extractFactTable ::  Fact -> Reader Env (Either [ValidationError] Table)
extractFactTable fact  =
  withFactValidation fact $ \table -> do
    Settings {..} <- asks envSettings
    allDims       <- extractAllDimensions' fact table

    let intType                  = "integer"
        sourceColumnType colName = columnType . fromJust . findColumn colName . tableColumns $ table

        columns = flip concatMap (factColumns fact) $ \col -> case col of
          DimTime cName            -> [ Column (timeUnitColName cName settingTimeUnit) intType NotNull ]
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
              DimTime cName -> Just (timeUnitColName cName settingTimeUnit)
              NoDimId cName -> Just cName
              _             -> Nothing

    return Table { tableName        = settingFactPrefix <> factName fact
                 , tableColumns     = columns ++ map fst fks
                 , tableConstraints = UniqueKey ukColNames : map snd fks
                 }
  where
    timeUnitColName colName timeUnit = colName <> "_" <> timeUnitName timeUnit <> "_id"

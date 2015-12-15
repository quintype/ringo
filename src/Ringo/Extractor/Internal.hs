module Ringo.Extractor.Internal where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust)
import Data.Monoid          ((<>))
import Data.List            (nub, find)

import Ringo.Types

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

extractDimensions' :: Fact -> Table -> Reader ExtractorEnv [Table]
extractDimensions' fact Table {..} = do
  tables <- asks eeTables
  prefix <- settingDimPrefix <$> asks eeSettings
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
      . Map.mapWithKey (\dim ->
                          map (\col@Column {..} -> col { columnName = dimColumnName dim columnName })
                          . nub)
      . Map.fromListWith (flip (++))
      . mapMaybe (\fcol -> do
                    DimVal d col <- fcol
                    column       <- findColumn col tableColumns
                    return (d, [ column ]))
      . map Just
      . factColumns
      $ fact

dimColumnName :: Text.Text -> ColumnName -> ColumnName
dimColumnName dimName columnName =
  fromMaybe columnName . Text.stripPrefix (dimName <> "_") $ columnName

extractAllDimensions' :: Fact -> Table -> Reader ExtractorEnv [Table]
extractAllDimensions' fact table = do
  myDims     <- extractDimensions' fact table
  parentDims <- concat <$> mapM extract (factParentNames fact)
  return . nub $ myDims ++ parentDims
  where
    extract fName = do
      tables <- asks eeTables
      facts  <- asks eeFacts
      let pFact      = fromJust . findFact fName $ facts
          pFactTable = fromJust . findTable (factTableName pFact) $ tables
      extractAllDimensions' pFact pFactTable

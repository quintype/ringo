module Ringo.Extractor.Internal where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust)
import Data.Monoid          ((<>))
import Data.List            (nub)

import Ringo.Types
import Ringo.Utils

extractDimensions :: Fact -> Table -> Reader Env [Table]
extractDimensions fact Table {..} = do
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

extractAllDimensions :: Fact -> Table -> Reader Env [Table]
extractAllDimensions fact table = do
  myDims     <- extractDimensions fact table
  parentDims <- concat <$> mapM extract (factParentNames fact)
  return . nub $ myDims ++ parentDims
  where
    extract fName = do
      tables <- asks envTables
      facts  <- asks envFacts
      let pFact      = fromJust . findFact fName $ facts
          pFactTable = fromJust . findTable (factTableName pFact) $ tables
      extractAllDimensions pFact pFactTable

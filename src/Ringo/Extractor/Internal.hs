module Ringo.Extractor.Internal where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust)
import Data.Monoid          ((<>))
import Data.List            (nub)

import Ringo.Types
import Ringo.Utils

dimColumnName :: Text.Text -> ColumnName -> ColumnName
dimColumnName dimName columnName =
  fromMaybe columnName . Text.stripPrefix (dimName <> "_") $ columnName

extractDimensionTables :: Fact -> Reader Env [Table]
extractDimensionTables fact = do
  tables    <- asks envTables
  prefix    <- settingDimPrefix <$> asks envSettings
  let table = fromJust . findTable (factTableName fact) $ tables
  return $ dimsFromIds tables ++ dimsFromVals prefix (tableColumns table)
  where
    dimsFromIds tables =
      flip mapMaybe (factColumns fact) $ \fcol -> case fcol of
        DimId d _ -> findTable d tables
        _         -> Nothing

    dimsFromVals prefix tableColumns =
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

extractAllDimensionTables :: Fact -> Reader Env [Table]
extractAllDimensionTables fact = do
  myDims     <- extractDimensionTables fact
  parentDims <- concat <$> mapM extract (factParentNames fact)
  return . nub $ myDims ++ parentDims
  where
    extract fName = do
      facts  <- asks envFacts
      extractAllDimensionTables . fromJust . findFact fName $ facts

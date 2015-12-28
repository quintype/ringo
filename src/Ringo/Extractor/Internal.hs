module Ringo.Extractor.Internal where

import qualified Data.Map  as Map
import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader (Reader, asks)
import Data.Function        (on)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust, catMaybes)
import Data.Monoid          ((<>))
import Data.List            (nub, nubBy)
import Data.Text            (Text)

import Ringo.Types
import Ringo.Utils

dimColumnName :: Text -> ColumnName -> ColumnName
dimColumnName dimName columnName =
  fromMaybe columnName . Text.stripPrefix (dimName <> "_") $ columnName

timeUnitColumnName :: Text -> ColumnName -> TimeUnit -> ColumnName
timeUnitColumnName dimIdColName colName timeUnit =
  colName <> "_" <> timeUnitName timeUnit <> "_" <> dimIdColName

factDimFKIdColumnName :: Text -> Text -> TableName -> ColumnName
factDimFKIdColumnName dimPrefix dimIdColName dimTableName =
  fromMaybe dimTableName (Text.stripPrefix dimPrefix dimTableName) <> "_" <> dimIdColName

extractedFactTableName :: Text -> Text -> TableName -> TimeUnit -> TableName
extractedFactTableName factPrefix factInfix factName timeUnit =
  factPrefix <> factName <> factInfix <> timeUnitName timeUnit

idColTypeToFKIdColType :: Text -> Text
idColTypeToFKIdColType typ = case Text.toLower typ of
  "serial"      -> "integer"
  "smallserial" -> "smallint"
  "bigserial"   -> "bigint"
  _             -> typ

extractDimensionTables :: Fact -> Reader Env [Table]
extractDimensionTables fact = do
  settings  <- asks envSettings
  tables    <- asks envTables
  let table = fromJust . findTable (factTableName fact) $ tables
  return $ dimsFromIds tables ++ dimsFromVals settings (tableColumns table)
  where
    dimsFromIds tables = catMaybes [ findTable d tables | DimId d _ <- factColumns fact ]

    dimsFromVals Settings {..} tableColumns =
      map (\(dim, cols) ->
            Table { tableName        = settingDimPrefix <> dim
                  , tableColumns     =
                      Column settingDimTableIdColumnName settingDimTableIdColumnType NotNull : cols
                  , tableConstraints = [ PrimaryKey settingDimTableIdColumnName
                                       , UniqueKey (map columnName cols)
                                       ]
                  })
      . Map.toList
      . Map.mapWithKey
          (\dim ->  map (\col -> col { columnName     = dimColumnName dim (columnName col)
                                     , columnNullable = NotNull
                                     })
                    . nub)
      . Map.fromListWith (flip (++))
      . mapMaybe (\fcol -> do
                    DimVal d col <- fcol
                    column       <- findColumn col tableColumns
                    return (d, [ column ]))
      . map Just
      . factColumns
      $ fact

extractAllDimensionTables :: Fact -> Reader Env [(Fact, Table)]
extractAllDimensionTables fact = do
  myDims     <- map (fact,) <$> extractDimensionTables fact
  parentDims <- concat <$> mapM extract (factParentNames fact)
  return . nubBy ((==) `on` snd) $ myDims ++ parentDims
  where
    extract fName = do
      facts <- asks envFacts
      extractAllDimensionTables . fromJust . findFact fName $ facts

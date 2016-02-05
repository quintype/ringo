{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
module Ringo.Extractor.Internal where

import qualified Data.Map  as Map
import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader (Reader, asks, withReader)
import Data.Function        (on)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust, catMaybes)
import Data.Monoid          ((<>))
import Data.List            (nub, nubBy, find)
import Data.Text            (Text)

import Ringo.Types

findTable :: TableName -> [Table] -> Maybe Table
findTable tName = find ((== tName) . tableName)

findFact :: TableName -> [Fact] -> Maybe Fact
findFact fName = find ((== fName) . factName)

findColumn :: ColumnName -> [Column] -> Maybe Column
findColumn cName = find ((== cName) . columnName)

dimColumnName :: Text -> ColumnName -> ColumnName
dimColumnName dimName columnName =
  fromMaybe columnName . Text.stripPrefix (dimName <> "_") $ columnName

timeUnitColumnName :: Text -> ColumnName -> TimeUnit -> ColumnName
timeUnitColumnName dimIdColName colName timeUnit =
  colName <> "_" <> timeUnitName timeUnit <> "_" <> dimIdColName

factDimFKIdColumnName :: Text -> Text -> Fact -> Table -> [Table] -> ColumnName
factDimFKIdColumnName dimPrefix dimIdColName dimFact dimTable@Table { .. } tables =
  if dimTable `elem` tables
    then head [ factColTargetColumn
                | FactColumn {factColType = DimId {..}, ..} <- factColumns dimFact
                , factColTargetTable == tableName ]
    else fromMaybe tableName (Text.stripPrefix dimPrefix tableName) <> "_" <> dimIdColName

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
extractDimensionTables fact = withReader envView $ do
  settings  <- asks envSettings
  tables    <- asks envTables
  let table = fromJust . findTable (factTableName fact) $ tables
  return $ dimsFromIds tables ++ dimsFromVals settings (tableColumns table)
  where
    dimsFromIds tables =
      catMaybes [ findTable factColTargetTable tables
                  | FactColumn {factColType = DimId {..}} <- factColumns fact ]

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
                    FactColumn {factColType = DimVal {..}, ..} <- fcol
                    column <- findColumn factColTargetColumn tableColumns
                    return (factColTargetTable, [ column ]))
      . map Just
      . factColumns
      $ fact

extractAllDimensionTables :: Fact -> Reader Env [(Fact, Table)]
extractAllDimensionTables fact = do
  myDims     <- map (fact,) <$> extractDimensionTables fact
  parentDims <- concat <$> mapM extract (factParentNames fact)
  return . nubBy ((==) `on` snd) $ myDims ++ parentDims
  where
    extract fName =
      asks (envFacts . envView) >>= extractAllDimensionTables . fromJust . findFact fName

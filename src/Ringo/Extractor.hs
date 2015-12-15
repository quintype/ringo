module Ringo.Extractor
       ( extractDimensions
       , extractFactTable
       ) where

import qualified Data.Text as Text

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust)
import Data.Monoid          ((<>))

import Ringo.Extractor.Internal
import Ringo.Types
import Ringo.Utils

extractFactTable ::  Fact -> Table -> Reader Env Table
extractFactTable fact table = do
  Settings {..} <- asks envSettings
  allDims       <- extractAllDimensions fact table

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

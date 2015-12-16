module Ringo.Extractor
       ( extractDimensionTables
       , extractAllDimensionTables
       , extractFactTable
       ) where

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (mapMaybe, fromJust)
import Data.Monoid          ((<>))

import Ringo.Extractor.Internal
import Ringo.Types
import Ringo.Utils

extractFactTable ::  Fact -> Reader Env Table
extractFactTable fact = do
  Settings {..} <- asks envSettings
  allDims       <- extractAllDimensionTables fact
  table         <- asks $ fromJust . findTable (factTableName fact) . envTables

  let intType                  = "integer"
      sourceColumnType colName = columnType . fromJust . findColumn colName . tableColumns $ table

      columns = flip concatMap (factColumns fact) $ \col -> case col of
        DimTime cName            -> [ Column (timeUnitColumnName cName settingTimeUnit) intType NotNull ]
        NoDimId cName            -> [ fromJust . findColumn cName . tableColumns $ table]
        FactCount cName          -> [ Column cName intType NotNull ]
        FactSum scName cName     -> [ Column cName (sourceColumnType scName) NotNull ]
        FactAverage scName cName -> [ Column (averageCountColummName cName) intType NotNull
                                    , Column (averageSumColumnName cName) (sourceColumnType scName) NotNull
                                    ]
        FactCountDistinct cName  -> [ Column (countDistinctColumnName cName) (intType <> "[]") NotNull ]
        _                        -> []

      fks = flip map allDims $ \(_, Table {..}) ->
        let colName     = factDimFKIdColumnName settingDimPrefix tableName
            colNullable = if any ((== Null) . columnNullable) tableColumns then Null else NotNull
        in (Column colName intType colNullable, ForeignKey tableName [(colName, "id")])

      ukColNames =
        (++ map (columnName . fst) fks)
        . flip mapMaybe (factColumns fact) $ \col -> case col of
            DimTime cName -> Just (timeUnitColumnName cName settingTimeUnit)
            NoDimId cName -> Just cName
            _             -> Nothing

  return Table { tableName        = extractedFactTableName settingFactPrefix (factName fact) settingTimeUnit
               , tableColumns     = columns ++ map fst fks
               , tableConstraints = UniqueKey ukColNames : map snd fks
               }

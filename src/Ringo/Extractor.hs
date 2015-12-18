module Ringo.Extractor
       ( extractDimensionTables
       , extractAllDimensionTables
       , extractFactTable
       ) where

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (fromJust)
import Data.Monoid          ((<>))

import Ringo.Extractor.Internal
import Ringo.Types
import Ringo.Utils

extractFactTable ::  Fact -> Reader Env Table
extractFactTable fact = do
  Settings {..} <- asks envSettings
  allDims       <- extractAllDimensionTables fact
  table         <- asks $ fromJust . findTable (factTableName fact) . envTables

  let countColType             = settingFactCountColumnType
      dimIdColName             = settingDimTableIdColumnName
      sourceColumnType colName = columnType . fromJust . findColumn colName . tableColumns $ table

      columns = concatFor (factColumns fact) $ \col -> case col of
        DimTime cName            ->
          [ Column (timeUnitColumnName dimIdColName cName settingTimeUnit) "integer" NotNull ]
        NoDimId cName            -> [ fromJust . findColumn cName . tableColumns $ table]
        FactCount cName          -> [ Column cName countColType NotNull ]
        FactSum scName cName     -> [ Column cName (sourceColumnType scName) NotNull ]
        FactAverage scName cName ->
          [ Column (cName <> settingAvgCountColumSuffix) countColType NotNull
          , Column (cName <> settingAvgSumColumnSuffix) (sourceColumnType scName) NotNull
          ]
        FactCountDistinct cName  ->
          [ Column (cName <> settingCountDistinctColumSuffix) (countColType <> "[]") NotNull ]
        _                        -> []

      fks = for allDims $ \(_, Table {..}) ->
        let colName     = factDimFKIdColumnName settingDimPrefix dimIdColName tableName
            colType     = idColTypeToFKIdColType settingDimTableIdColumnType
            colNullable = if any ((== Null) . columnNullable) tableColumns then Null else NotNull
        in ( Column colName colType colNullable , ForeignKey tableName [(colName, dimIdColName)] )

      ukColNames =
        (++ map (columnName . fst) fks)
        . forMaybe (factColumns fact) $ \col -> case col of
            DimTime cName -> Just (timeUnitColumnName dimIdColName cName settingTimeUnit)
            NoDimId cName -> Just cName
            _             -> Nothing

  return Table
         { tableName        =
             extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit
         , tableColumns     = columns ++ map fst fks
         , tableConstraints = UniqueKey ukColNames : map snd fks
         }

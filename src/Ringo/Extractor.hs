{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ringo.Extractor
       ( extractDimensionTables
       , extractAllDimensionTables
       , extractFactTable
       , extractDependencies
       ) where

import qualified Data.Map  as Map
import qualified Data.Tree as Tree

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (fromJust)
import Data.Monoid          ((<>))
import Data.List            (nub)

import Ringo.Extractor.Internal
import Ringo.Types
import Ringo.Utils

extractFactTable ::  Fact -> Reader Env Table
extractFactTable fact = do
  Settings {..} <- asks envSettings
  allDims       <- extractAllDimensionTables fact
  tables        <- asks envTables
  let table     =  fromJust . findTable (factTableName fact) $ tables

  let countColType             = settingFactCountColumnType
      dimIdColName             = settingDimTableIdColumnName
      sourceColumnType colName = columnType . fromJust . findColumn colName . tableColumns $ table

      columns = concatFor (factColumns fact) $ \col -> case col of
        DimTime cName             ->
          [ Column (timeUnitColumnName dimIdColName cName settingTimeUnit) "bigint" NotNull ]
        NoDimId cName             -> let
            col' = fromJust . findColumn cName . tableColumns $ table
          in [ col' { columnNullable = NotNull } ]
        FactCount _ cName         -> [ Column cName countColType NotNull ]
        FactSum scName cName      -> [ Column cName (sourceColumnType scName) NotNull ]
        FactAverage scName cName  ->
          [ Column (cName <> settingAvgCountColumSuffix) countColType NotNull
          , Column (cName <> settingAvgSumColumnSuffix) (sourceColumnType scName) NotNull
          ]
        FactCountDistinct _ cName -> [ Column cName "json" NotNull ]
        _                         -> []

      fkColumns = for allDims $ \(_, Table {..}) ->
        let colName     = factDimFKIdColumnName settingDimPrefix dimIdColName tableName
            colType     = idColTypeToFKIdColType settingDimTableIdColumnType
        in Column colName colType NotNull

      ukColNames =
        (++ map columnName fkColumns)
        . forMaybe (factColumns fact) $ \col -> case col of
            DimTime cName -> Just (timeUnitColumnName dimIdColName cName settingTimeUnit)
            NoDimId cName -> Just cName
            _             -> Nothing

  return Table
         { tableName        =
             extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit
         , tableColumns     = columns ++ fkColumns
         , tableConstraints = [ UniqueKey ukColNames ]
         }

extractDependencies :: Fact -> Reader Env Dependencies
extractDependencies fact = do
  settings@Settings{..} <- asks envSettings
  facts                 <- asks envFacts
  let factSourceDeps =
        nub . Tree.flatten . flip Tree.unfoldTree fact $ \fct ->
          (factTableName fct, parentFacts fct facts)
      factDimDeps    =
        nub . concat . Tree.flatten . flip Tree.unfoldTree fact $ \fct ->
          ( forMaybe (factColumns fct) $ \col -> case col of
              DimVal table _ -> Just $ settingDimPrefix  <> table
              DimId table _  -> Just table
              _              -> Nothing
          , parentFacts fct facts
          )

      dimDeps  = Map.fromList [ (settingDimPrefix <> table, [factTableName fact])
                                | DimVal table _ <- factColumns fact ]

      factDeps = Map.singleton (extractedTable settings) (factSourceDeps ++ factDimDeps)
  return $ Map.union dimDeps factDeps
  where
    extractedTable Settings {..} =
      extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

    parentFacts fct facts = [ fromJust $ findFact pf facts | pf <- factParentNames fct ]

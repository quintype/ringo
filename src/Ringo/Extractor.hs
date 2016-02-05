{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Ringo.Extractor
       ( extractDimensionTables
       , extractAllDimensionTables
       , extractFactTable
       , extractDependencies
       ) where

import qualified Data.Map  as Map
import qualified Data.Tree as Tree

import Control.Monad.Reader (Reader, asks, withReader)
import Data.Maybe           (fromJust)
import Data.Monoid          ((<>))
import Data.List            (nub)

import Ringo.Extractor.Internal
import Ringo.Types
import Ringo.Utils

extractFactTable ::  Fact -> Reader Env Table
extractFactTable fact = do
  allDims <- extractAllDimensionTables fact
  withReader envView $ do
    Settings {..} <- asks envSettings
    tables        <- asks envTables
    let table     =  fromJust . findTable (factTableName fact) $ tables

    let countColType       = settingFactCountColumnType
        dimIdColName       = settingDimTableIdColumnName
        sourceColumn cName = fromJust . findColumn cName . tableColumns $ table
        notNullSourceColumnCopy cName          = (sourceColumn cName) { columnNullable = NotNull }
        notNullSourceColumnRename scName cName = (notNullSourceColumnCopy scName) { columnName = cName }

        columns = concatFor (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
          case factColType of
            DimTime                ->
              [ Column (timeUnitColumnName dimIdColName cName settingTimeUnit) "bigint" NotNull ]
            NoDimId                -> [ notNullSourceColumnCopy cName ]
            TenantId               -> [ notNullSourceColumnCopy cName ]
            FactCount {..}         -> [ Column cName countColType NotNull ]
            FactCountDistinct {..} -> [ Column cName "json" NotNull ]
            FactSum {..}           -> [ notNullSourceColumnRename factColSourceColumn cName ]
            FactMax {..}           -> [ notNullSourceColumnRename factColSourceColumn cName ]
            FactMin {..}           -> [ notNullSourceColumnRename factColSourceColumn cName ]
            FactAverage {..}       ->
              [ Column (cName <> settingAvgCountColumSuffix) countColType NotNull
              , notNullSourceColumnRename factColSourceColumn (cName <> settingAvgSumColumnSuffix)
              ]
            _                      -> []

        fkColumns = for allDims $ \(dimFact, dimTable) ->
          let colName = factDimFKIdColumnName settingDimPrefix dimIdColName dimFact dimTable tables
              colType = idColTypeToFKIdColType settingDimTableIdColumnType
          in Column colName colType NotNull

        ukColNames =
          (++ map columnName fkColumns)
          . forMaybe (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
              case factColType of
                  DimTime  -> Just $ timeUnitColumnName dimIdColName cName settingTimeUnit
                  NoDimId  -> Just cName
                  TenantId -> Just cName
                  _        -> Nothing

    return Table
           { tableName        =
               extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit
           , tableColumns     = columns ++ fkColumns
           , tableConstraints = [ UniqueKey ukColNames ]
           }

extractDependencies :: Fact -> Reader Env Dependencies
extractDependencies fact = withReader envView $ do
  settings@Settings{..} <- asks envSettings
  facts                 <- asks envFacts
  let factSourceDeps =
        nub . Tree.flatten . flip Tree.unfoldTree fact $ \fct ->
          (factTableName fct, parentFacts fct facts)
      factDimDeps    =
        nub . concat . Tree.flatten . flip Tree.unfoldTree fact $ \fct ->
          ( forMaybe (factColumns fct) $ \FactColumn {..} -> case factColType of
              DimVal {..} -> Just $ settingDimPrefix  <> factColTargetTable
              DimId {..}  -> Just factColTargetTable
              _           -> Nothing
          , parentFacts fct facts
          )

      dimDeps  = Map.fromList [ (settingDimPrefix <> table, [factTableName fact])
                                | FactColumn {factColType = DimVal table} <- factColumns fact ]

      factDeps = Map.singleton (extractedTable settings) (factSourceDeps ++ factDimDeps)
  return $ Map.union dimDeps factDeps
  where
    extractedTable Settings {..} =
      extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

    parentFacts fct facts = [ fromJust $ findFact pf facts | pf <- factParentNames fct ]

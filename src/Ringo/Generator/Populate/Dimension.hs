{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Ringo.Generator.Populate.Dimension (dimensionTablePopulateSQL) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Control.Monad.Reader     (Reader, asks, withReader)
import Database.HsSqlPpp.Syntax (Statement, QueryExpr(..), Distinct(..), makeSelect, JoinType(..))
import Data.Maybe               (fromJust)
import Data.Text                (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Generator.Sql
import Ringo.Types

dimensionTablePopulateSQL :: TablePopulationMode -> Fact -> TableName -> Reader Env Text
dimensionTablePopulateSQL popMode fact dimTableName =
  ppStatement <$> dimensionTablePopulateStmt popMode fact dimTableName

dimensionTablePopulateStmt :: TablePopulationMode -> Fact -> TableName -> Reader Env Statement
dimensionTablePopulateStmt popMode fact dimTableName = withReader envView $ do
  Settings {..}    <- asks envSettings
  tables           <- asks envTables
  defaults         <- asks envTypeDefaults
  let factTable    = fromJust $ findTable (factTableName fact) tables
      colMapping   = dimColumnMapping settingDimPrefix fact dimTableName
      selectCols   = [ flip sia (nmc cName) $ coalesceColumn defaults (factTableName fact) col
                       | (_, cName) <- colMapping
                       , let col    = fromJust . findColumn cName $ tableColumns factTable ]
      timeCol      = head ([ cName | DimTimeV cName <- factColumns fact ] :: [ColumnName])
      isNotNullC   = parens . foldBinop "or" . map (postop "isnotnull" . ei . snd) $ colMapping
      selectWhereC = Just . foldBinop "and" $
                       [ isNotNullC, binop "<" (ei timeCol) placeholder ] ++
                         [ binop ">=" (ei timeCol) placeholder | popMode == IncrementalPopulation ]
      selectC      = makeSelect
                     { selDistinct   = Distinct
                     , selSelectList = sl selectCols
                     , selTref       = [tref $ factTableName fact]
                     , selWhere      = selectWhereC
                     }

      iTableName   = suffixTableName popMode settingTableNameSuffixTemplate dimTableName
      insertC      = insert iTableName (map fst colMapping) $ case popMode of
        FullPopulation        -> selectC
        IncrementalPopulation -> let alias = "x" in
          makeSelect
          { selSelectList = sl [si $ qstar alias]
          , selTref       =
              [ tjoin (subtrefa alias selectC) LeftOuter (tref dimTableName) . Just $
                  foldBinop "and" [ binop "=" (eqi dimTableName c1) (eqi alias c2) | (c1, c2) <- colMapping ] ]
          , selWhere      =
              Just . foldBinop "and" . map (postop "isnull" . eqi dimTableName . fst) $ colMapping
          }

  return insertC

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Ringo.Generator.Populate.Fact (factTablePopulateSQL) where

import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Control.Monad.Reader     (Reader, asks, withReader)
import Database.HsSqlPpp.Syntax ( QueryExpr(..), Statement, makeSelect
                                , SelectList(..), SelectItem(..), JoinType(..) )
import Data.List                (nub)
import Data.Maybe               (fromJust, fromMaybe, listToMaybe)
import Data.Monoid              ((<>))
import Data.Text                (Text)
import Text.RawString.QQ        (r)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Generator.Sql
import Ringo.Types
import Ringo.Utils

ilog2FunctionString :: Text
ilog2FunctionString = [r|CREATE OR REPLACE FUNCTION ilog2(v integer)
    RETURNS integer AS
$$
DECLARE
    r integer;
    shift integer;
BEGIN
    IF v > x'FFFF'::integer THEN r := 1 << 4; ELSE r := 0 << 4; END IF;
    v := v >> r;
    IF v > x'FF'::integer THEN shift := 1 << 3; ELSE shift := 0 << 3; END IF;
    v := v >> shift;
    r := r | shift;
    IF v > x'F'::integer THEN shift := 1 << 2; ELSE shift := 0 << 2; END IF;
    v := v >> shift;
    r := r | shift;
    IF v > x'3'::integer THEN shift := 1 << 1; ELSE shift := 0 << 3; END IF;
    v := v >> shift;
    r := r | shift;
    r := r | (v >> 1);
    RETURN r;
END;
$$
LANGUAGE 'plpgsql' IMMUTABLE;
|]

factCountDistinctUpdateStmts :: TablePopulationMode -> Fact -> Text -> QueryExpr -> Reader EnvV [Statement]
factCountDistinctUpdateStmts popMode fact groupByColPrefix expr = case expr of
  Select {selSelectList = SelectList _ origSelectItems, ..} -> do
    Settings {..}         <- asks envSettings
    tables                <- asks envTables
    let fTableName        = factTableName fact
        fTable            = fromJust . findTable fTableName $ tables
        tablePKColName    = head [ cName | PrimaryKey cName <- tableConstraints fTable ]
        extFactTableName  =
          suffixTableName popMode settingTableNameSuffixTemplate
            $ extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

    return $ forMaybe (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
      case factColType of
        FactCountDistinct {factColMaybeSourceColumn = scName} ->
          let unqCol           = cast (eqi fTableName (fromMaybe tablePKColName scName)) "text"

              bucketSelectCols =
                [ sia (binop "&" (app "hashtext" [ unqCol ])
                                 (num . Text.pack . show $ bucketCount settingFactCountDistinctErrorRate - 1))
                      (nmc $ cName <> "_bnum")
                , sia (binop "-"
                         (num "31")
                         (app "ilog2"
                            [ app "min" [ binop "&"
                                            (app "hashtext" [ unqCol ])
                                            (prefop "~" (parens (binop "<<" (num "1") (num "31"))))]]))
                      (nmc $ cName <> "_bhash")
                ]

              groupByCols      = map ppScalarExpr selGroupBy
              selectList       =
                [ i | i@(SelectItem _ _ a) <- origSelectItems , a `elem` map nmc groupByCols ]

              selectStmt       =
                makeSelect
                  { selSelectList = sl $ selectList ++ bucketSelectCols
                  , selTref       = selTref
                  , selWhere      = binop "and" (postop "isnotnull" unqCol) <$> selWhere
                  , selGroupBy    = selGroupBy ++ [ ei $ cName <> "_bnum" ]
                  }

              aggSelectClause  =
                sia (app "json_object_agg" [ ei (cName <> "_bnum"), ei (cName <> "_bhash") ]) (nmc cName)

          in Just $ update extFactTableName
            [ (cName, eqi "xyz" cName) ]
            [ subtrefa "xyz"
                makeSelect
                  { selSelectList = sl $ map (si . ei) groupByCols ++ [ aggSelectClause ]
                  , selTref       = [ subtrefa "zyx" selectStmt ]
                  , selGroupBy    = selGroupBy
                  } ] $
            foldBinop "and"
              [ binop "=" (eqi extFactTableName . fromJust . Text.stripPrefix groupByColPrefix $ col)
                           (eqi "xyz" col)
                | col <- groupByCols ]

        _ -> Nothing

  _ -> return []
  where
    bucketCount :: Double -> Integer
    bucketCount errorRate =
      let power :: Double = fromIntegral (ceiling . logBase 2 $ (1.04 / errorRate) ** 2 :: Integer)
      in ceiling $ 2 ** power

factTablePopulateStmts :: TablePopulationMode -> Fact -> Reader Env [Statement]
factTablePopulateStmts popMode fact = do
  allDims <- extractAllDimensionTables fact
  withReader envView $ do
    Settings {..}       <- asks envSettings
    tables              <- asks envTables
    defaults            <- asks envTypeDefaults
    let fTableName      = factTableName fact
        fTable          = fromJust . findTable fTableName $ tables
        dimIdColName    = settingDimTableIdColumnName

        coalesceFKId ex =
          app "coalesce" [ ex, num . Text.pack . show $ settingForeignKeyIdCoalesceValue ]

        timeUnitColumnInsertSQL cName =
          let colName = timeUnitColumnName dimIdColName cName settingTimeUnit
          in ( colName
             , cast (app "floor" [ binop "/" (extEpoch (eqi fTableName cName))
                                             (num . Text.pack . show . timeUnitToSeconds $ settingTimeUnit) ])
                    "bigint"
             , True
             )
        dimIdColumnInsertSQL cName =
          let sCol = fromJust . findColumn cName $ tableColumns fTable
          in (cName, coalesceColumn defaults fTableName sCol, True)

        app' f cName = app f [ eqi fTableName cName ]

        factColMap = concatFor (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
          case factColType of
            DimTime                -> [ timeUnitColumnInsertSQL cName ]
            NoDimId                -> [ dimIdColumnInsertSQL cName ]
            TenantId               -> [ dimIdColumnInsertSQL cName ]
            FactCount {..}         ->
              [ (cName, app "count" [ maybe star (eqi fTableName) factColMaybeSourceColumn ], False) ]
            FactCountDistinct {..} -> [ (cName, cast (str "{}") "json", False) ]
            FactSum {..}           -> [ (cName, app' "sum" factColSourceColumn, False) ]
            FactMax {..}           -> [ (cName, app' "max" factColSourceColumn, False) ]
            FactMin {..}           -> [ (cName, app' "min" factColSourceColumn, False) ]
            FactAverage {..}       ->
              [ ( cName <> settingAvgCountColumSuffix, app' "count" factColSourceColumn, False )
              , ( cName <> settingAvgSumColumnSuffix , app' "sum" factColSourceColumn  , False)
              ]
            _                      -> []

        dimColMap = for allDims $ \(dimFact, factTable@Table {tableName}) -> let
            dimFKIdColName        =
              factDimFKIdColumnName settingDimPrefix dimIdColName dimFact factTable tables
            factSourceTableName   = factTableName dimFact
            factSourceTable       = fromJust . findTable factSourceTableName $ tables
            dimFKIdColumn         = fromJust . findColumn dimFKIdColName $ tableColumns factSourceTable
            dimLookupWhereClauses = Just . foldBinop "and" $
              [ binop "=" (eqi tableName dimColName) (coalesceColumn defaults factSourceTableName sourceCol)
                | (dimColName, sourceColName) <- dimColumnMapping settingDimPrefix dimFact tableName
                , let sourceCol = fromJust . findColumn sourceColName $ tableColumns factSourceTable ]
            insertExpr = if factTable `elem` tables -- existing dimension table
              then (if columnNullable dimFKIdColumn == Null then coalesceFKId else id)
                     $ eqi factSourceTableName dimFKIdColName
              else coalesceFKId . subQueryExp $
                     makeSelect
                       { selSelectList = sl [ si $ ei dimIdColName ]
                       , selTref       =
                           [ trefa (suffixTableName popMode settingTableNameSuffixTemplate tableName) tableName ]
                       , selWhere      = dimLookupWhereClauses
                       }
          in (dimFKIdColName, insertExpr, True)

        colMap              = [ (cName, (expr, nmc $ groupByColPrefix <> cName), addToGroupBy)
                                | (cName, expr, addToGroupBy) <- factColMap ++ dimColMap ]

        joinClauses         =
          map (tref &&& joinClausePreds fTable)
          . filter (/= fTableName)
          . nub
          . map (factTableName . fst)
          $ allDims

        timeCol             = eqi fTableName $ head [ cName | DimTimeV cName <- factColumns fact ]

        extFactTableName    = suffixTableName popMode settingTableNameSuffixTemplate
          $ extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

        populateSelectExpr  =
          makeSelect
            { selSelectList = sl . map (uncurry sia . snd3) $ colMap
            , selTref       = [ foldl (\tf (t, oc) -> tjoin tf LeftOuter t oc) (tref fTableName) joinClauses ]
            , selWhere      = Just . foldBinop "and" $
                binop "<" timeCol placeholder :
                  [ binop ">=" timeCol placeholder | popMode == IncrementalPopulation ]
            , selGroupBy    = map (ei . (groupByColPrefix <>) . fst3) . filter thd3 $ colMap
            }

        insertIntoStmt      = insert extFactTableName (map fst3 colMap) populateSelectExpr

    updateStmts <- factCountDistinctUpdateStmts popMode fact groupByColPrefix populateSelectExpr
    return $ insertIntoStmt : updateStmts
    where
      groupByColPrefix = "xxff_"

      joinClausePreds table oTableName =
        foldBinop "and"
        . map (\(c1, c2) -> binop "=" (eqi (tableName table) c1) (eqi oTableName c2))
        <$> listToMaybe [ colPairs | ForeignKey tName colPairs <-  tableConstraints table
                                   , tName == oTableName ]

factTablePopulateSQL :: TablePopulationMode -> Fact -> Reader Env [Text]
factTablePopulateSQL popMode fact = do
  stmts <- factTablePopulateStmts popMode fact
  return $ case stmts of
    []   -> []
    [i]  -> [ ppStatement i ]
    i:us -> [ ppStatement i, ilog2FunctionString ] ++ map ppStatement us

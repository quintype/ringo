{-# LANGUAGE QuasiQuotes #-}
module Ringo.Generator.Populate.Fact (factTablePopulateSQL) where

import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader (Reader, asks)
import Data.List            (nub)
import Data.Maybe           (fromJust, fromMaybe, mapMaybe, listToMaybe)
import Data.Monoid          ((<>))
import Data.Text            (Text)
import Text.RawString.QQ    (r)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
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
LANGUAGE 'plpgsql' IMMUTABLE|]

data FactTablePopulateSelectSQL = FactTablePopulateSelectSQL
                                { ftpsSelectCols   :: ![(Text, Text)]
                                , ftpsSelectTable  :: !Text
                                , ftpsJoinClauses  :: ![Text]
                                , ftpsWhereClauses :: ![Text]
                                , ftpsGroupByCols  :: ![Text]
                                } deriving (Show, Eq)

factTableUpdateSQL :: Fact -> Text -> FactTablePopulateSelectSQL -> Reader Env [Text]
factTableUpdateSQL fact groupByColPrefix populateSelectSQL@FactTablePopulateSelectSQL {..} = do
  Settings {..}         <- asks envSettings
  tables                <- asks envTables
  let countDistinctCols = [ col | col@(FactCountDistinct _ _) <- factColumns fact]
      fTableName        = factTableName fact
      fTable            = fromJust . findTable fTableName $ tables
      tablePKColName    = head [ cName | PrimaryKey cName <- tableConstraints fTable ]
      extFactTableName  =
        extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

  return . (\xs -> if null xs then xs else ilog2FunctionString : xs)
    $ for countDistinctCols $ \(FactCountDistinct scName cName) ->
      let unqCol           = fullColumnName fTableName (fromMaybe tablePKColName scName) <> "::text"

          bucketSelectCols =
            [ ( "hashtext(" <> unqCol <> ") & "
                 <> Text.pack (show $ bucketCount settingFactCountDistinctErrorRate - 1)
              , cName <> "_bnum"
              )
            , ( "31 - ilog2(min(hashtext(" <> unqCol <> ") & ~(1 << 31)))"
              , cName <> "_bhash"
              )
            ]

          selectSQL        = toSelectSQL $
            populateSelectSQL
              { ftpsSelectCols   = filter ((`elem` ftpsGroupByCols) . snd) ftpsSelectCols ++ bucketSelectCols
              , ftpsGroupByCols  = ftpsGroupByCols ++ [ cName <> "_bnum" ]
              , ftpsWhereClauses = ftpsWhereClauses ++ [ unqCol <> " IS NOT NULL" ]
              }

          aggSelectClause  =
            "json_object_agg(" <> cName <> "_bnum, " <> cName <> "_bhash) AS " <> cName

      in "UPDATE " <> extFactTableName
           <> "\nSET " <> cName <> " = " <> fullColumnName "xyz" cName
           <> "\nFROM ("
           <> "\nSELECT " <> joinColumnNames (ftpsGroupByCols ++ [aggSelectClause])
           <> "\nFROM (\n" <> selectSQL <> "\n) zyx"
           <> "\nGROUP BY \n" <> joinColumnNames ftpsGroupByCols
           <> "\n) xyz"
           <> "\n WHERE\n"
           <> Text.intercalate "\nAND "
                [ fullColumnName extFactTableName .fromJust . Text.stripPrefix groupByColPrefix $ col
                    <> " = " <> fullColumnName "xyz" col
                  | col <- ftpsGroupByCols ]
  where
    bucketCount :: Double -> Integer
    bucketCount errorRate =
      let power :: Double = fromIntegral (ceiling . logBase 2 $ (1.04 / errorRate) ** 2 :: Integer)
      in ceiling $ 2 ** power

factTablePopulateSQL :: TablePopulationMode -> Fact -> Reader Env [Text]
factTablePopulateSQL popMode fact = do
  Settings {..}      <- asks envSettings
  allDims            <- extractAllDimensionTables fact
  tables             <- asks envTables
  defaults           <- asks envTypeDefaults
  let fTableName     = factTableName fact
      fTable         = fromJust . findTable fTableName $ tables
      dimIdColName   = settingDimTableIdColumnName

      timeUnitColumnInsertSQL cName =
        let colName = timeUnitColumnName dimIdColName cName settingTimeUnit
        in ( colName
           , "extract(epoch from " <> fullColumnName fTableName cName <> ")::bigint/"
                <> Text.pack (show $ timeUnitToSeconds settingTimeUnit)
           , True
           )

      factColMap = concatFor (factColumns fact) $ \col -> case col of
        DimTime cName             -> [ timeUnitColumnInsertSQL cName ]
        NoDimId cName             ->
          let sCol = fromJust . findColumn cName $ tableColumns fTable
          in [ (cName, coalesceColumn defaults fTableName sCol, True) ]
        FactCount scName cName    ->
          [ (cName, "count(" <> maybe "*" (fullColumnName fTableName) scName <> ")", False) ]
        FactSum scName cName      ->
          [ (cName, "sum(" <> fullColumnName fTableName scName <> ")", False) ]
        FactAverage scName cName  ->
          [ ( cName <> settingAvgCountColumSuffix
            , "count(" <> fullColumnName fTableName scName <> ")"
            , False
            )
          , ( cName <> settingAvgSumColumnSuffix
            , "sum(" <> fullColumnName fTableName scName <> ")"
            , False
            )
          ]
        FactCountDistinct _ cName -> [ (cName, "'{}'::json", False)]
        _                        -> []

      dimColMap = for allDims $ \(dimFact, factTable@Table {tableName}) -> let
          dimFKIdColName        = factDimFKIdColumnName settingDimPrefix dimIdColName tableName
          factSourceTableName   = factTableName dimFact
          factSourceTable       = fromJust . findTable factSourceTableName $ tables
          dimFKIdColumn         = fromJust . findColumn dimFKIdColName $ tableColumns factSourceTable
          dimLookupWhereClauses =
            [ fullColumnName tableName dimColName <> " = " <> coalesceColumn defaults factSourceTableName sourceCol
              | (dimColName, sourceColName) <- dimColumnMapping settingDimPrefix dimFact tableName
              , let sourceCol = fromJust . findColumn sourceColName $ tableColumns factSourceTable ]
          insertSQL             = if factTable `elem` tables -- existing dimension table
            then (if columnNullable dimFKIdColumn == Null then coalesceFKId else id)
                   $ fullColumnName factSourceTableName dimFKIdColName
            else "SELECT " <> dimIdColName <> " FROM " <> tableName <> "\nWHERE "
                  <> Text.intercalate "\n AND " dimLookupWhereClauses
        in (dimFKIdColName, coalesceFKId insertSQL, True)

      colMap              = [ (cName, (sql, groupByColPrefix <> cName), addToGroupBy)
                              | (cName, sql, addToGroupBy) <- factColMap ++ dimColMap ]

      joinClauses         =
        mapMaybe (\tName -> (\p -> "LEFT JOIN " <> tName <> "\nON "<> p) <$> joinClausePreds fTable tName)
        . nub
        . map (factTableName . fst)
        $ allDims

      timeCol             = fullColumnName fTableName $ head [ cName | DimTime cName <- factColumns fact ]

      extFactTableName    =
        extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

      populateSelectSQL   =
        FactTablePopulateSelectSQL
          { ftpsSelectCols   = map snd3 colMap
          , ftpsSelectTable  = fTableName
          , ftpsJoinClauses  = joinClauses
          , ftpsWhereClauses = if popMode == IncrementalPopulation
                                then [ timeCol <> " > ?", timeCol <> " <= ?" ]
                                else []
          , ftpsGroupByCols  = map ((groupByColPrefix <>) . fst3) . filter thd3 $ colMap
          }

      insertIntoSQL       = "INSERT INTO " <> extFactTableName
                              <> " (\n" <> Text.intercalate ",\n " (map fst3 colMap) <> "\n)\n"
                              <> toSelectSQL populateSelectSQL

  updateSQLs <- factTableUpdateSQL fact groupByColPrefix populateSelectSQL

  return $ insertIntoSQL : updateSQLs
  where
    groupByColPrefix = "xxff_"

    joinClausePreds table oTableName =
      Text.intercalate " AND "
      . map (\(c1, c2) -> fullColumnName (tableName table) c1 <> " = " <> fullColumnName oTableName c2)
      <$> listToMaybe [ colPairs | ForeignKey tName colPairs <-  tableConstraints table
                                 , tName == oTableName ]

    coalesceFKId col =
      if "coalesce" `Text.isPrefixOf` col
        then col
        else "coalesce((" <> col <> "), -1)" -- TODO extract this out to settings

toSelectSQL :: FactTablePopulateSelectSQL -> Text
toSelectSQL FactTablePopulateSelectSQL {..} =
  "SELECT \n" <> joinColumnNames (map (uncurry asName) ftpsSelectCols)
    <> "\nFROM " <> ftpsSelectTable
    <> (if not . null $ ftpsJoinClauses
          then "\n" <> Text.intercalate "\n" ftpsJoinClauses
          else "")
    <> (if not . null $ ftpsWhereClauses
         then "\nWHERE " <> Text.intercalate "\nAND " ftpsWhereClauses
         else "")
    <> "\nGROUP BY \n"
    <> joinColumnNames ftpsGroupByCols
  where
    asName sql alias = "(" <> sql <> ")" <> " as " <> alias


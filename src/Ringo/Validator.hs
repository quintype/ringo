{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Ringo.Validator (validateEnv) where

import qualified Data.Map as Map
import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Maybe           (isJust, fromJust)
import Data.List            (nub, group, sort)

import Ringo.Extractor.Internal
import Ringo.Types
import Ringo.Types.Internal
import Ringo.Utils

data RawEnv = RawEnv ![Table] ![Fact] !Settings !TypeDefaults deriving (Show)

checkTableForCol :: Table -> ColumnName -> [ValidationError]
checkTableForCol tab colName =
  [ MissingColumn (tableName tab) colName |
      not . any ((colName ==) . columnName) . tableColumns $ tab ]

validateTable :: Table -> Reader RawEnv [ValidationError]
validateTable table = do
  RawEnv tables _ _ _ <- ask
  return . concatMap (checkConstraint tables) . tableConstraints $ table
  where
    checkConstraint _ (PrimaryKey colName)    = checkTableForCol table colName
    checkConstraint _ (UniqueKey columnNames) = checkTableForColRefs table columnNames
    checkConstraint tables (ForeignKey oTableName columnNames) =
      case findTable oTableName tables of
        Just oTable -> checkTableForColRefs table (map fst columnNames)
                         ++ checkTableForColRefs oTable (map snd columnNames)
        Nothing     -> [ MissingTable oTableName ]

    checkTableForColRefs tab = concatMap (checkTableForCol tab)

validateFact :: Fact -> Reader RawEnv [ValidationError]
validateFact Fact {..} = do
  RawEnv tables _ _ typeDefaults <- ask
  let defaults = Map.keys typeDefaults
  case findTable factTableName tables of
    Nothing    -> return [ MissingTable factTableName ]
    Just table -> do
      tableVs           <- validateTable table
      parentVs          <- concat <$> mapM checkFactParents factParentNames
      let colVs         = concatMap (checkColumn tables table) factColumns
          timeVs        = [ MissingTimeColumn factTableName
                            | null ([ cName | DimTimeV cName <- factColumns ] :: [ColumnName]) ]
          notNullVs     = [ MissingNotNullConstraint factTableName cName
                            | DimTimeV cName <- factColumns
                            , let col        = findColumn cName (tableColumns table)
                            , isJust col
                            , columnNullable (fromJust col) == Null ]

          typeDefaultVs =
            [ MissingTypeDefault cType
              | cName   <- [ c | DimValV   c <- factColumns ]
                        ++ [ c | NoDimIdV  c <- factColumns ]
                        ++ [ c | TenantIdV c <- factColumns ]
                        ++ [ c | DimIdV    c <- factColumns ]
              , let col = findColumn cName (tableColumns table)
              , isJust col
              , let cType = columnType $ fromJust col
              , not . any (`Text.isPrefixOf` cType) $ defaults ]

      return $ tableVs ++ parentVs ++ colVs ++ timeVs ++ notNullVs ++ typeDefaultVs
  where
    checkFactParents fName = do
      RawEnv _ facts _ _ <- ask
      case findFact fName facts of
        Nothing    -> return [ MissingFact fName ]
        Just pFact -> validateFact pFact

    checkColumn tables table factCol =
      maybe [] (checkTableForCol table) (factSourceColumnName factCol)
        ++ checkColumnTable tables factCol

    checkColumnTable :: [Table] -> FactColumn -> [ValidationError]
    checkColumnTable tables FactColumn {..} = case factColType of
      DimId {factColTargetTable = tName} -> maybe [ MissingTable tName ] (const []) $ findTable tName tables
      _                                  -> []

validateEnv :: [Table] -> [Fact] -> Settings -> TypeDefaults -> Either [ValidationError] Env
validateEnv tables facts settings typeDefaults =
  flip runReader (RawEnv tables facts settings typeDefaults) $ do
    tableVs <- concat <$> mapM validateTable tables
    factVs  <- concat <$> mapM validateFact facts
    let dupTableVs = [ DuplicateTable table | table <- findDups . map tableName $ tables ]
    let dupFactVs  = [ DuplicateFact fact   | fact  <- findDups . map factName $ facts ]
    let dupColVs   = [ DuplicateColumn tableName col
                       | Table{..} <- tables
                       , col       <- findDups . map columnName $ tableColumns ]
    let vs = nub $ tableVs ++ factVs ++ dupTableVs ++ dupFactVs ++ dupColVs
    if null vs
      then return . Right $ Env tables facts settings typeDefaults
      else return . Left  $ vs
  where
    findDups =
      sort >>> group >>> map (head &&& length) >>> filter (snd >>> (> 1)) >>> map fst

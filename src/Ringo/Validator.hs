{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Ringo.Validator
       ( validateTable
       , validateFact
       ) where

import qualified Data.Map as Map
import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (isJust, fromJust)

import Ringo.Extractor.Internal
import Ringo.Types

checkTableForCol :: Table -> ColumnName -> [ValidationError]
checkTableForCol tab colName =
  [ MissingColumn (tableName tab) colName |
      not . any ((colName ==) . columnName) . tableColumns $ tab ]

validateTable :: Table -> Reader Env [ValidationError]
validateTable table = do
  tables   <- asks envTables
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

validateFact :: Fact -> Reader Env [ValidationError]
validateFact Fact {..} = do
  tables <- asks envTables
  defaults <- Map.keys <$> asks envTypeDefaults
  case findTable factTableName tables of
    Nothing    -> return [ MissingTable factTableName ]
    Just table -> do
      tableVs           <- validateTable table
      parentVs          <- concat <$> mapM checkFactParents factParentNames
      let colVs         = concatMap (checkColumn tables table) factColumns
          timeVs        = [ MissingTimeColumn factTableName
                            | null [ c | DimTime c <- factColumns ] ]
          notNullVs     = [ MissingNotNullConstraint factTableName c
                            | DimTime c <- factColumns
                            , let col   = findColumn c (tableColumns table)
                            , isJust col
                            , columnNullable (fromJust col) == Null ]
          typeDefaultVs =
            [ MissingTypeDefault cType
              | cName   <- [ c | DimVal _ c <- factColumns ] ++ [ c | NoDimId c <- factColumns ]
              , let col = findColumn cName (tableColumns table)
              , isJust col
              , let cType = columnType $ fromJust col
              , not . any (`Text.isPrefixOf` cType) $ defaults ]

      return $ tableVs ++ parentVs ++ colVs ++ timeVs ++ notNullVs ++ typeDefaultVs
  where
    checkFactParents fName = do
      facts <- asks envFacts
      case findFact fName facts of
        Nothing    -> return [ MissingFact fName ]
        Just pFact -> validateFact pFact

    checkColumn tables table factCol =
      maybe [] (checkTableForCol table) (factSourceColumnName factCol)
        ++ checkColumnTable tables factCol

    checkColumnTable tables factCol = case factCol of
      DimId tName _  -> maybe [ MissingTable tName ] (const []) $ findTable tName tables
      _              -> []

module Ringo.Validator where

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust)
import Data.Monoid          ((<>))

import Ringo.Types
import Ringo.Utils

checkTableForCol :: Table -> ColumnName -> [ValidationError]
checkTableForCol tab colName =
  [ MissingColumn (tableName tab) colName |
      not . any ((colName ==) . columnName) . tableColumns $ tab ]

validateTable :: Table -> Reader Env [ValidationError]
validateTable table = do
  tables <- asks envTables
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
  case findTable factTableName tables of
    Nothing    -> return [ MissingTable factTableName ]
    Just table -> do
      tableVs   <- validateTable table
      parentVs  <- concat <$> mapM checkFactParents factParentNames
      let colVs = concatMap (checkColumn table) factColumns
      return $ tableVs ++ parentVs ++ colVs
  where
    checkFactParents fName = do
      facts  <- asks envFacts
      case findFact fName facts of
        Nothing    -> return [ MissingFact fName ]
        Just pFact -> validateFact pFact

    checkColumn table = maybe [] (checkTableForCol table) . factColumnName

withFactValidation :: Fact -> (Table -> Reader Env a)
                   -> Reader Env (Either [ValidationError] a)
withFactValidation fact func = do
  tables <- asks envTables
  errors <- validateFact fact
  if not $ null errors
    then return $ Left errors
    else fmap Right . func . fromJust . findTable (factTableName fact) $ tables

module Main where

import qualified Data.Text as Text

import Data.Char        (toLower)
import Data.List        (nub)
import Data.Monoid      ((<>))
import Control.Monad    (forM_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>), (<.>))
import System.Exit      (exitFailure, exitSuccess)

import Ringo
import Ringo.ArgParser
import Ringo.InputParser

data SQLType = Create | Populate | Update deriving (Eq, Show)

main :: IO ()
main = do
  ProgArgs {..} <- parseArgs
  result        <- parseInput progInputFile
  case result of
    Left err              -> putStrLn err >> exitFailure
    Right (tables, facts) -> do
      let env    = Env tables facts progSettings
      let errors = nub $ concatMap (validateTable env) tables ++ concatMap (validateFact env) facts
      if not $ null errors
        then mapM_ print errors              >> exitFailure
        else writeSQLFiles progOutputDir env >> exitSuccess

writeSQLFiles :: FilePath -> Env -> IO ()
writeSQLFiles outputDir env@Env{..} = forM_ sqls $ \(sqlType, table, sql) -> do
  let dirName  = outputDir </> map toLower (show sqlType)
      fileName = dirName </> Text.unpack table <.> "sql"
  createDirectoryIfMissing True dirName
  writeFile fileName sql
  where
    dimTables  = map (\fact -> (fact, extractDimensionTables env fact)) envFacts
    factTables = map (\fact -> (fact, extractFactTable env fact)) envFacts

    dimTableDefnSQLs   = [ (Create, tableName table, tabDefnSQL table)
                           | (_, tabs)  <- dimTables
                           , table      <- tabs
                           , table `notElem` envTables ]
    factTableDefnSQLs  = [ (Create, tableName table, tabDefnSQL table)
                           | (_, table) <- factTables ]

    dimTableInsertSQLs = [ (Populate
                           , tableName table
                           , sqlStr $ dimensionTableInsertSQL env fact (tableName table))
                           | (fact, tabs) <- dimTables
                           , table        <- tabs
                           , table `notElem` envTables ]

    fctTableInsertSQLs = [ (Populate, tableName table, sqlStr $ factTableInsertSQL env fact)
                           | (fact, table) <- factTables ]

    sqls = concat [ dimTableDefnSQLs
                  , factTableDefnSQLs
                  , dimTableInsertSQLs
                  , fctTableInsertSQLs
                  ]

    sqlStr s   = Text.unpack $ s <> ";\n"
    tabDefnSQL = unlines . map sqlStr . tableDefnSQL

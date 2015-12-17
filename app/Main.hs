module Main where

import qualified Data.Text as Text

import Data.List   (nub)
import System.Exit (exitFailure, exitSuccess)

import Ringo
import Ringo.ArgParser
import Ringo.InputParser

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
        then mapM print errors >> exitFailure
        else do
          let dimTables  = map (\fact -> (fact, extractDimensionTables env fact)) facts
              factTables = map (\fact -> (fact, extractFactTable env fact)) facts

              dimTableDefnSQLs  = [ tabDefnSQL table | (fact, tabs)  <- dimTables
                                                     , table <- tabs
                                                     , table `notElem` tables ]
              factTableDefnSQLs = [ tabDefnSQL table | (fact, table) <- factTables ]

          mapM_ putStrLn dimTableDefnSQLs
          mapM_ putStrLn factTableDefnSQLs

          exitSuccess
  where
    toSQL      = Text.unpack . flip Text.snoc ';'
    tabDefnSQL = unlines . map toSQL . tableDefnSQL


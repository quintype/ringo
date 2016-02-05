{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map             as Map
import qualified Data.Text            as Text

import Data.Aeson       (encode)
import Data.Char        (toLower)
import Control.Monad    (forM_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>), (<.>))
import System.Exit      (exitFailure, exitSuccess)

import Ringo
import Ringo.ArgParser
import Ringo.InputParser

data SQLType = Create | FullRefresh | IncRefresh deriving (Eq, Show)

main :: IO ()
main = do
  ProgArgs {..} <- parseArgs
  result        <- parseInput progInputFile
  case result of
    Left err                        -> putStrLn err >> exitFailure
    Right (tables, facts, defaults) -> do
      case makeEnv tables facts progSettings defaults of
        Left errors -> mapM_ print errors           >> exitFailure
        Right env   -> writeFiles progOutputDir env >> exitSuccess

writeFiles :: FilePath -> Env -> IO ()
writeFiles outputDir env@(envView -> EnvV{..}) = do
  let Settings{..} = envSettings
  forM_ sqls $ \(sqlType, table, sql) -> do
    let dirName = outputDir </> map toLower (show sqlType)
    createDirectoryIfMissing True dirName
    writeFile (dirName </> Text.unpack table <.> "sql") sql

  BS.writeFile (outputDir </> Text.unpack settingDependenciesJSONFileName)
    . encode
    . foldl (\acc -> Map.union acc . extractDependencies env) Map.empty
    $ envFacts

  BS.writeFile (outputDir </> Text.unpack settingDimensionJSONFileName) . encode $
    [ tableName table | (_, tabs) <- dimTables, table <- tabs , table `notElem` envTables ]

  BS.writeFile (outputDir </> Text.unpack settingFactsJSONFileName) . encode $
    [ tableName table | (_, table) <- factTables ]

  where
    dimTables  = [ (fact, extractDimensionTables env fact) | fact <- envFacts ]
    factTables = [ (fact, extractFactTable env fact)       | fact <- envFacts, factTablePersistent fact ]

    dimTableDefnSQLs    = [ (Create, tableName table, unlines . map sqlStr $ dimensionTableDefnSQL env table)
                            | (_, tabs) <- dimTables
                            , table     <- tabs
                            , table `notElem` envTables ]

    factTableDefnSQLs   = [ (Create , tableName table, unlines . map sqlStr $ factTableDefnSQL env fact table)
                            | (fact, table) <- factTables ]

    dimTablePopulateSQLs typ gen  =
      [ (typ , tableName table, sqlStr $ gen env fact (tableName table))
        | (fact, tabs) <- dimTables
        , table        <- tabs
        , table `notElem` envTables ]

    factTablePopulateSQLs typ gen = [ (typ, tableName table, unlines . map sqlStr  $ gen env fact)
                                      | (fact, table) <- factTables ]

    sqls = concat [ dimTableDefnSQLs
                  , factTableDefnSQLs
                  , dimTablePopulateSQLs FullRefresh  $ dimensionTablePopulateSQL FullPopulation
                  , dimTablePopulateSQLs IncRefresh   $ dimensionTablePopulateSQL IncrementalPopulation
                  , factTablePopulateSQLs FullRefresh $ factTablePopulateSQL FullPopulation
                  , factTablePopulateSQLs IncRefresh  $ factTablePopulateSQL IncrementalPopulation
                  ]

    sqlStr = Text.unpack

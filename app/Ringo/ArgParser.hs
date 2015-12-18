module Ringo.ArgParser (ProgArgs(..), parseArgs) where

import qualified Data.Text as Text

import Data.List (intercalate)
import Options.Applicative

import Ringo.Types

data ProgArgs = ProgArgs
                { progSettings  :: Settings
                , progInputFile :: FilePath
                , progOutputDir :: FilePath
                } deriving (Eq, Show)

settingsParser :: Parser Settings
settingsParser = let Settings {..} = defSettings
  in Settings
     <$> (Text.pack <$> strOption (long "dim-prefix"
                                   <> short 'd'
                                   <> value (Text.unpack settingDimPrefix)
                                   <> showDefault
                                   <> help "Prefix for dimension tables"))
     <*> (Text.pack <$> strOption (long "fact-prefix"
                                   <> short 'f'
                                   <> value (Text.unpack settingFactPrefix)
                                   <> showDefault
                                   <> help "Prefix for fact tables"))
     <*> option auto (let timeunits = map show [Second ..]
                      in long "timeunit"
                         <> short 't'
                         <> value settingTimeUnit
                         <> showDefault
                         <> completeWith timeunits
                         <> help ("Time unit granularity for fact tables. Possible values: "
                                    ++ intercalate ", " timeunits))
     <*> minorOption "avg-count-col-suffix"
                     settingAvgCountColumSuffix
                     "Suffix for average count columns"
     <*> minorOption "avg-sum-col-suffix"
                     settingAvgSumColumnSuffix
                     "Suffix for average sum columns"
     <*> minorOption "count-distinct-col-suffix"
                     settingCountDistinctColumSuffix
                     "Suffix for count distinct bucket columns"
     <*> minorOption "dim-id-col-name"
                     settingDimTableIdColumnName
                     "Name of dimension table id columns"
     <*> minorOption "dim-id-col-type"
                     settingDimTableIdColumnType
                     "Type of dimension table id columns"
     <*> minorOption "fact-count-col-type"
                     settingFactCountColumnType
                     "Type of fact table count columns"
     <*> minorOption "fact-infix"
                     settingFactInfix
                     "Infix for fact tables"
  where
    minorOption longDesc defValue helpTxt =
      Text.pack <$> strOption (long longDesc
                               <> hidden
                               <> value (Text.unpack defValue)
                               <> showDefault
                               <> help helpTxt)

progArgsParser :: Parser ProgArgs
progArgsParser =
  ProgArgs
  <$> settingsParser
  <*> argument str (metavar "INPUT"
                    <> action "file"
                    <> help "Input file")
  <*> argument str (metavar "OUTPUT"
                    <> action "directory"
                    <> help "Output directory")

parseArgs :: IO ProgArgs
parseArgs = execParser $
  info (helper <*> progArgsParser)
       (fullDesc
        <> progDesc "Transforms OLTP database schemas to OLAP database star schemas"
        <> header "ringo - OLTP to OLAP schema transformer"
        <> footer "Source: http://github.com/quintype/ringo")

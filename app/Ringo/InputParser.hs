module Ringo.InputParser (parseInput) where

import qualified Data.Text as Text
import qualified Data.Vector as V

import Data.Maybe (fromMaybe)
import Data.Yaml hiding (Null)
import Data.Vector ((!), (!?))
import Ringo.Types

instance FromJSON Nullable where
  parseJSON (String s) = case s of
    "null"    -> pure Null
    "notnull" -> pure NotNull
    _         -> fail $ "Invalid value for nullable: " ++ Text.unpack s
  parseJSON o          = fail $ "Cannot parse nullable: " ++ show o

instance FromJSON Column where
  parseJSON (Array a) = if V.length a < 2
    then fail $ "Column needs at least two elements: name and type"
    else do
      cName <- parseJSON $ a ! 0
      cType <- parseJSON $ a ! 1
      cNull <- parseJSON $ fromMaybe "null" (a !? 2)
      return $ Column cName cType cNull

  parseJSON o         = fail $ "Cannot parse column: " ++ show o

instance FromJSON TableConstraint where
  parseJSON (Object o) = do
    cType <- o .: "type"
    case cType of
      "primary" -> PrimaryKey <$> o .: "column"
      "unique"  -> UniqueKey  <$> o .: "columns"
      "foreign" -> ForeignKey <$> o .: "table" <*> o .: "columns"
      _         -> fail $ "Invalid constraint type: " ++ cType
  parseJSON o          = fail $ "Cannot parse constraint: " ++ show o

instance FromJSON Table where
  parseJSON (Object o) = Table <$> o .: "name" <*> o .: "columns" <*> o .: "constraints"
  parseJSON o          = fail $ "Cannot parse table: " ++ show o

instance FromJSON FactColumn where
  parseJSON (Object o) = do
    cType <- o .: "type"
    case cType of
      "dimtime"           -> DimTime           <$> o .: "column"
      "nodimid"           -> NoDimId           <$> o .: "column"
      "dimid"             -> DimId             <$> o .: "table"        <*> o .: "column"
      "dimval"            -> DimVal            <$> o .: "table"        <*> o .: "column"
      "factcount"         -> FactCount         <$> o .: "column"
      "factsum"           -> FactSum           <$> o .: "sourcecolumn" <*> o .: "column"
      "factaverage"       -> FactAverage       <$> o .: "sourcecolumn" <*> o .: "column"
      "factcountdistinct" -> FactCountDistinct <$> o .: "column"
      _                   -> fail $ "Invalid fact column type: " ++ cType
  parseJSON o          = fail $ "Cannot parse fact column: " ++ show o

instance FromJSON Fact where
  parseJSON (Object o) = Fact <$> o .: "name"
                              <*> o .: "tablename"
                              <*> o .:? "parentfacts" .!= []
                              <*> o .: "columns"
  parseJSON o          = fail $ "Cannot parse fact: " ++ show o

data Input = Input [Table] [Fact] deriving (Eq, Show)

instance FromJSON Input where
  parseJSON (Object o) = Input <$> o .: "tables" <*> o .: "facts"
  parseJSON o          = fail $ "Cannot parse input: " ++ show o

parseInput :: FilePath -> IO (Either String ([Table], [Fact]))
parseInput file = do
  result <- decodeFileEither file
  return $ case result of
    Left pe                    -> Left $ prettyPrintParseException pe
    Right (Input tables facts) -> Right (tables, facts)

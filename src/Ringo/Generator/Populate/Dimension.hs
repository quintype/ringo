module Ringo.Generator.Populate.Dimension (dimensionTablePopulateSQL) where

import qualified Data.Text as Text

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative  ((<$>))
#endif

import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (fromJust)
import Data.Monoid          ((<>))
import Data.Text            (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Types

dimensionTablePopulateSQL :: TablePopulationMode -> Fact -> TableName -> Reader Env Text
dimensionTablePopulateSQL popMode fact dimTableName = do
  dimPrefix           <- settingDimPrefix <$> asks envSettings
  tables              <- asks envTables
  defaults            <- asks envTypeDefaults
  let factTable       = fromJust $ findTable (factTableName fact) tables
      colMapping      = dimColumnMapping dimPrefix fact dimTableName
      selectCols      = [ coalesceColumn defaults (factTableName fact) col <> " AS " <> cName
                          | (_, cName) <- colMapping
                          , let col    = fromJust . findColumn cName $ tableColumns factTable ]
      baseSelectC     = "SELECT DISTINCT\n" <> joinColumnNames selectCols
                          <> "\nFROM " <> factTableName fact
      baseWhereC      = "(\n"
                          <> Text.intercalate "\nOR " [ c <> " IS NOT NULL" | (_, c) <- colMapping ]
                          <> "\n)"
      insertC selectC whereCs =
        "INSERT INTO " <> dimTableName
          <> " (\n" <> joinColumnNames (map fst colMapping) <> "\n) "
          <> "SELECT x.* FROM (\n"
          <> selectC <> "\nWHERE " <> Text.intercalate " AND\n" whereCs
          <> ") x"
      timeCol         = head [ cName | DimTime cName <- factColumns fact ]
  return $ case popMode of
    FullPopulation        -> insertC baseSelectC [baseWhereC]
    IncrementalPopulation ->
      insertC baseSelectC [baseWhereC, timeCol <> " > ?", timeCol <> " <= ?"]
        <> "\nLEFT JOIN " <> dimTableName <> " ON\n"
        <> Text.intercalate " \nAND "
              [ fullColumnName dimTableName c1 <> " = " <> fullColumnName "x" c2
                | (c1, c2) <- colMapping ]
        <> "\nWHERE " <> Text.intercalate " \nAND "
                           [ fullColumnName dimTableName c <> " IS NULL" | (c, _) <- colMapping ]

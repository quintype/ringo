module Ringo
       ( module Ringo.Types
       , extractFactTable
       , extractDimensionTables
       , G.tableDefnSQL
       , dimensionTableInsertSQL
       , factTableInsertSQL
       , validateTable
       , validateFact
       ) where

import Control.Monad.Reader (runReader)
import Data.Text (Text)

import Ringo.Types
import qualified Ringo.Extractor as E
import qualified Ringo.Generator as G
import qualified Ringo.Validator as V

extractFactTable :: Env -> Fact -> Table
extractFactTable env = flip runReader env . E.extractFactTable

extractDimensionTables :: Env -> Fact -> [Table]
extractDimensionTables env = flip runReader env . E.extractDimensionTables

dimensionTableInsertSQL :: Env -> Fact -> TableName -> Text
dimensionTableInsertSQL env fact = flip runReader env . G.dimensionTableInsertSQL fact

factTableInsertSQL :: Env -> Fact -> Text
factTableInsertSQL env = flip runReader env . G.factTableInsertSQL

validateTable :: Env -> Table -> [ValidationError]
validateTable env = flip runReader env . V.validateTable

validateFact :: Env -> Fact -> [ValidationError]
validateFact env = flip runReader env . V.validateFact

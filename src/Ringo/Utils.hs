module Ringo.Utils where

import Data.Maybe (mapMaybe)
import Data.List  (find)

import Ringo.Types

findTable :: TableName -> [Table] -> Maybe Table
findTable tName = find ((== tName) . tableName)

findFact :: TableName -> [Fact] -> Maybe Fact
findFact fName = find ((== fName) . factName)

findColumn :: ColumnName -> [Column] -> Maybe Column
findColumn cName = find ((== cName) . columnName)

for :: [a] -> (a -> b) -> [b]
for = flip map

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

#if MIN_VERSION_base(4,8,0)
concatFor :: (Foldable t) => t a -> (a -> [b]) -> [b]
concatFor = flip concatMap
#else
concatFor :: [a] -> (a -> [b]) -> [b]
concatFor = flip concatMap
#endif

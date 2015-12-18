module Ringo.Utils where

import qualified Control.Arrow as Arrow

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

infixr 3 ***, &&&

first :: (a -> a') -> (a, b) -> (a', b)
first = Arrow.first

second :: (b -> b') -> (a, b) -> (a, b')
second = Arrow.second

(***) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(***) = (Arrow.***)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) = (Arrow.&&&)

dupe :: a -> (a,a)
dupe x = (x, x)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

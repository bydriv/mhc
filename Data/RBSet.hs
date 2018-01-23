-- MHC: Minimal Haskell Compiler © 2018 Kaoru Kawamukai <bydriv@gmail.com>
--
-- MHC is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- MHC is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with MHC.  If not, see <http://www.gnu.org/licenses/>.

module Data.RBSet
  ( RBSet
  , empty
  , singleton
  , foldr
  , foldl
  , foldTree
  , map
  , concatMap
  , all
  , any
  , null
  , member
  , subset
  , insert
  , delete
  , union
  , unions
  , inter
  , diff
  , maximum
  , minimum
  , toList
  , fromList ) where

import           Prelude
  hiding
    ( all
    , any
    , concatMap
    , foldl
    , foldr
    , map
    , maximum
    , minimum
    , null )
import qualified Prelude
import qualified Data.List  as List
import qualified Data.RBMap as RBMap

newtype RBSet a = RBSet (RBMap.RBMap a ())
  deriving (Eq, Ord)

instance (Ord a, Read a) => Read (RBSet a) where
  readsPrec _ s =
    if List.isPrefixOf "fromList" s then
      Prelude.map (\(xs, s') -> (fromList xs, s')) $ reads $ List.drop 8 s
    else
      []

instance Show a => Show (RBSet a) where
  show = ("fromList" ++) . show . toList

unRBSet :: RBSet a -> RBMap.RBMap a ()
unRBSet (RBSet rbMap) = rbMap

empty :: RBSet a
empty = RBSet RBMap.empty

singleton :: a -> RBSet a
singleton = RBSet . flip RBMap.singleton ()

foldr :: (a -> b -> b) -> b -> RBSet a -> b
foldr f z = RBMap.foldri (const . f) z . unRBSet

foldl :: (a -> b -> b) -> b -> RBSet a -> b
foldl f z = RBMap.foldli (const . f) z . unRBSet

foldTree :: (a -> b -> b -> b) -> b -> RBSet a -> b
foldTree f z = RBMap.foldiTree (const . f) z . unRBSet

map :: (Ord a, Ord b) => (a -> b) -> RBSet a -> RBSet b
map f = foldr (insert . f) empty

concatMap :: (Ord a, Ord b) => (a -> RBSet b) -> RBSet a -> RBSet b
concatMap f = foldr (union . f) empty

all :: (a -> Bool) -> RBSet a -> Bool
all f = foldr ((&&) . f) True

any :: (a -> Bool) -> RBSet a -> Bool
any f = foldr ((||) . f) False

null :: RBSet a -> Bool
null = RBMap.null . unRBSet

member :: Ord a => a -> RBSet a -> Bool
member x = RBMap.member x . unRBSet

subset :: Ord a => RBSet a -> RBSet a -> Bool
subset = flip $ all . flip member

insert :: Ord a => a -> RBSet a -> RBSet a
insert x = RBSet . RBMap.insert x () . unRBSet

delete :: Ord a => a -> RBSet a -> RBSet a
delete x = RBSet . RBMap.delete x . unRBSet

union :: Ord a => RBSet a -> RBSet a -> RBSet a
union rbSet = RBSet . RBMap.unionr (unRBSet rbSet) . unRBSet

unions :: Ord a => [RBSet a] -> RBSet a
unions = Prelude.foldr union empty

inter :: Ord a => RBSet a -> RBSet a -> RBSet a
inter rbSet = RBSet . RBMap.interr (unRBSet rbSet) . unRBSet

diff :: Ord a => RBSet a -> RBSet a -> RBSet a
diff rbSet = RBSet . RBMap.diff (unRBSet rbSet) . unRBSet

maximum :: Ord a => RBSet a -> Maybe a
maximum = RBMap.maximumKey . unRBSet

minimum :: Ord a => RBSet a -> Maybe a
minimum = RBMap.minimumKey . unRBSet

toList :: RBSet a -> [a]
toList = foldr (:) []

fromList :: Ord a => [a] -> RBSet a
fromList = Prelude.foldr insert empty

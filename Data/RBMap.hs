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

module Data.RBMap
  ( RBMap
  , empty
  , singleton
  , foldri
  , foldr
  , foldli
  , foldl
  , foldiTree
  , foldTree
  , mapi
  , map
  , gmap
  , filteri
  , filter
  , alli
  , all
  , anyi
  , any
  , lookup
  , insert
  , delete
  , null
  , member
  , insertBy
  , alter
  , update
  , adjust
  , unionBy
  , unionsBy
  , unionl
  , unionr
  , interBy
  , interl
  , interr
  , diffBy
  , diff
  , keys
  , elts
  , maximumKey
  , minimumKey
  , maximum
  , minimum
  , toList
  , fromList ) where

import           Prelude
  hiding
    ( all
    , any
    , filter
    , foldl
    , foldr
    , lookup
    , map
    , maximum
    , minimum
    , null )
import qualified Prelude
import qualified Data.List  as List
import qualified Data.Maybe as Maybe

data Color =
    Red
  | Black
  deriving (Eq, Ord, Read, Show)

data RBMap k a =
    Empty
  | Node Color (RBMap k a) (RBMap k a) k a

instance (Eq k, Eq a) => Eq (RBMap k a) where
  lhs == rhs = toList lhs == toList rhs

instance (Ord k, Ord a) => Ord (RBMap k a) where
  compare lhs rhs = compare (toList lhs) (toList rhs)

instance (Ord k, Read k, Read a) => Read (RBMap k a) where
  readsPrec _ s =
    if List.isPrefixOf "fromList" s then
      List.map (\(alist, s') -> (fromList alist, s')) $ reads $ List.drop 8 s
    else
      []

instance (Show k, Show a) => Show (RBMap k a) where
  show = ("fromList" ++) . show . toList

toRed :: RBMap k a -> RBMap k a
toRed Empty = Empty
toRed (Node _ l r k x) = Node Red l r k x

toBlack :: RBMap k a -> RBMap k a
toBlack Empty = Empty
toBlack (Node _ l r k x) = Node Black l r k x

balance :: RBMap k a -> RBMap k a
balance (Node Black (Node Red (Node Red l2 r2 k2 x2) r1 k1 x1) r0 k0 x0) =
  Node Red (Node Black l2 r2 k2 x2) (Node Black r1 r0 k0 x0) k1 x1
balance (Node Black (Node Red l1 (Node Red l2 r2 k2 x2) k1 x1) r0 k0 x0) =
  Node Red (Node Black l1 l2 k1 x1) (Node Black r2 r0 k0 x0) k2 x2
balance (Node Black l0 (Node Red (Node Red l2 r2 k2 x2) r1 k1 x1) k0 x0) =
  Node Red (Node Black l0 l2 k0 x0) (Node Black r2 r1 k1 x1) k2 x2
balance (Node Black l0 (Node Red l1 (Node Red l2 r2 k2 x2) k1 x1) k0 x0) =
  Node Red (Node Black l0 l1 k0 x0) (Node Black l2 r2 k2 x2) k1 x1
balance rbMap =
  rbMap

empty :: RBMap k a
empty = Empty

singleton :: k -> a -> RBMap k a
singleton = Node Black Empty Empty

foldri :: (k -> a -> b -> b) -> b -> RBMap k a -> b
foldri _ z Empty = z
foldri f z (Node _ l r k x) = foldri f (f k x (foldri f z r)) l

foldr :: (a -> b -> b) -> b -> RBMap k a -> b
foldr = foldri . const

foldli :: (k -> a -> b -> b) -> b -> RBMap k a -> b
foldli _ z Empty = z
foldli f z (Node _ l r k x) = foldli f (f k x (foldli f z l)) r

foldl :: (a -> b -> b) -> b -> RBMap k a -> b
foldl = foldli . const

foldiTree :: (k -> a -> b -> b -> b) -> b -> RBMap k a -> b
foldiTree _ z Empty = z
foldiTree f z (Node _ l r k x) = f k x (foldiTree f z l) (foldiTree f z r)

foldTree :: (a -> b -> b -> b) -> b -> RBMap k a -> b
foldTree = foldiTree . const

mapi :: (k -> a -> b) -> RBMap k a -> RBMap k b
mapi _ Empty = Empty
mapi f (Node color l r k x) = Node color (mapi f l) (mapi f r) k (f k x)

map :: (a -> b) -> RBMap k a -> RBMap k b
map = mapi . const

gmap :: (Ord k, Ord k') => (k -> a -> (k', b)) -> RBMap k a -> RBMap k' b
gmap f = foldri (\k -> uncurry insert . f k) empty

filteri :: Ord k => (k -> a -> Bool) -> RBMap k a -> RBMap k a
filteri f = flip foldri empty $ \k x ->
  if f k x then
    insert k x
  else
    id

filter :: Ord k => (a -> Bool) -> RBMap k a -> RBMap k a
filter = filteri . const

alli :: (k -> a -> Bool) -> RBMap k a -> Bool
alli f = foldri (\k -> (&&) . (f k)) True

all :: (a -> Bool) -> RBMap k a -> Bool
all = alli . const

anyi :: (k -> a -> Bool) -> RBMap k a -> Bool
anyi f = foldri (\k -> (||) . (f k)) False

any :: (a -> Bool) -> RBMap k a -> Bool
any = anyi . const

lookup :: Ord k => k -> RBMap k a -> Maybe a
lookup _ Empty =
  Nothing
lookup k (Node _ l0 r0 k0 x0) =
  case compare k k0 of
    LT -> lookup k l0
    EQ -> Just x0
    GT -> lookup k r0

insert :: Ord k => k -> a -> RBMap k a -> RBMap k a
insert k x = toBlack . insert' where
  insert' Empty =
    Node Red Empty Empty k x
  insert' (Node color l0 r0 k0 x0) =
    case compare k k0 of
      LT -> balance $ Node color (insert' l0) r0 k0 x0
      EQ -> Node color l0 r0 k x
      GT -> balance $ Node color l0 (insert' r0) k0 x0

delete :: Ord k => k -> RBMap k a -> RBMap k a
delete k = toBlack . delete' where
  cat Empty r0 =
    r0
  cat l0 Empty =
    l0
  cat (Node Red l1 r1 k1 x1) (Node Red l2 r2 k2 x2) =
    case cat r1 l2 of
      Node Red l3 r3 k3 x3 ->
        Node Red (Node Red l1 l3 k1 x1) (Node Red r3 r2 k2 x2) k3 x3
      rbMap ->
        Node Red l1 (Node Red rbMap r2 k2 x2) k1 x1
  cat (Node Black l1 r1 k1 x1) (Node Black l2 r2 k2 x2) =
    case cat r1 l2 of
      Node Red l3 r3 k3 x3 ->
        Node Red (Node Black l1 l3 k1 x1) (Node Black r3 r2 k2 x2) k3 x3
      rbMap ->
        case l1 of
          Node Red l4 r4 k4 x4 ->
            Node Red (Node Black l4 r4 k4 x4) (Node Black rbMap r2 k2 x2) k1 x1
          _ ->
            balance (Node Black l1 (Node Red rbMap r2 k2 x2) k1 x1)
  cat l0 (Node Red l2 r2 k2 x2) =
    Node Red (cat l0 l2) r2 k2 x2
  cat (Node Red l1 r1 k1 x1) r0 =
    Node Red l1 (cat r1 r0) k1 x1

  delete' Empty =
    Empty
  delete' (Node _ l0 r0 k0 x0) =
    case compare k k0 of
      LT ->
        case l0 of
          Node Black _ _ _ _ ->
            case delete' l0 of
              Node Red l1 r1 k1 x1 ->
                Node Red (Node Black l1 r1 k1 x1) r0 k0 x0
              l0' ->
                case r0 of
                  Node Black l1 r1 k1 x1 ->
                    balance (Node Black l0' (Node Red l1 r1 k1 x1) k0 x0)
                  Node Red (Node Black l2 r2 k2 x2) r1 k1 x1 ->
                    Node
                      Red
                      (Node Black l0' l2 k0 x0)
                      (balance (Node Black r2 (toRed r1) k1 x1))
                      k2
                      x2
                  _ ->
                    undefined
          _ ->
            Node Red (delete' l0) r0 k0 x0
      EQ ->
        cat l0 r0
      GT ->
        case r0 of
          Node Black _ _ _ _ ->
            case delete' r0 of
              Node Red l1 r1 k1 x1 ->
                Node Red l0 (Node Black l1 r1 k1 x1) k0 x0
              r0' ->
                case l0 of
                  Node Black l1 r1 k1 x1 ->
                    balance (Node Black (Node Red l1 r1 k1 x1) r0' k0 x0)
                  Node Red l1 (Node Black l2 r2 k2 x2) k1 x1 ->
                    Node
                      Red
                      (balance (Node Black (toRed l1) l2 k1 x1))
                      (Node Black r2 r0' k0 x0)
                      k2
                      x2
                  _ ->
                    undefined
          _ ->
            Node Red l0 (delete' r0) k0 x0

null :: RBMap k a -> Bool
null Empty = True
null _ = False

member :: Ord k => k -> RBMap k a -> Bool
member k = Maybe.isJust . lookup k

insertBy :: Ord k => (a -> a -> a) -> k -> a -> RBMap k a -> RBMap k a
insertBy f k x rbMap =
  case lookup k rbMap of
    Nothing ->
      insert k x rbMap
    Just x' ->
      insert k (f x x') rbMap

alter :: Ord k => (Maybe a -> Maybe a) -> k -> RBMap k a -> RBMap k a
alter f k rbMap =
  case f (lookup k rbMap) of
    Nothing ->
      delete k rbMap
    Just x' ->
      insert k x' rbMap

update :: Ord k => (a -> Maybe a) -> k -> RBMap k a -> RBMap k a
update = alter . maybe Nothing

adjust :: Ord k => (a -> a) -> k -> RBMap k a -> RBMap k a
adjust f = update $ Just . f

unionBy :: Ord k => (a -> a -> a) -> RBMap k a -> RBMap k a -> RBMap k a
unionBy = foldri . insertBy . flip

unionsBy :: Ord k => (a -> a -> a) -> [RBMap k a] -> RBMap k a
unionsBy f = Prelude.foldr (unionBy f) empty

unionl :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
unionl = unionBy const

unionr :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
unionr = unionBy $ const id

interBy :: Ord k => (a -> a -> a) -> RBMap k a -> RBMap k a -> RBMap k a
interBy f rbMap = flip foldri empty $ \k x ->
  case lookup k rbMap of
    Nothing ->
      id
    Just x' ->
      insert k $ f x' x

interl :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
interl = interBy const

interr :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
interr = interBy $ const id

diffBy :: Ord k => (a -> a -> Maybe a) -> RBMap k a -> RBMap k a -> RBMap k a
diffBy f = foldri $ flip $ update . flip f

diff :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
diff = diffBy $ \_ _ -> Nothing

keys :: RBMap k a -> [k]
keys = foldri (const . (:)) []

elts :: RBMap k a -> [a]
elts = foldr (:) []

maximumKey :: Ord k => RBMap k a -> Maybe k
maximumKey = maximumKey' Nothing where
  maximumKey' z Empty = z
  maximumKey' _ (Node _ _ r k _) = maximumKey' (Just k) r

minimumKey :: Ord k => RBMap k a -> Maybe k
minimumKey = minimumKey' Nothing where
  minimumKey' z Empty = z
  minimumKey' _ (Node _ l _ k _) = minimumKey' (Just k) l

maximum :: Ord a => RBMap k a -> Maybe a
maximum rbMap =
  let alist = elts rbMap in
    if Prelude.null alist then
      Nothing
    else
      Just $ List.maximum alist

minimum :: Ord a => RBMap k a -> Maybe a
minimum rbMap =
  let alist = elts rbMap in
    if Prelude.null alist then
      Nothing
    else
      Just $ List.minimum alist

toList :: RBMap k a -> [(k, a)]
toList = foldri (\k x -> ((k, x) :)) []

fromList :: Ord k => [(k, a)] -> RBMap k a
fromList = Prelude.foldr (uncurry insert) empty

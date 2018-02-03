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
  , blackHeight
  , length
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
    , length
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
  lhs == rhs =
    if blackHeight lhs /= blackHeight rhs then
      False
    else
      toList lhs == toList rhs

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

{-# INLINE toRed #-}
toRed :: RBMap k a -> RBMap k a
toRed Empty = Empty
toRed (Node _ l r k x) = Node Red l r k x

{-# INLINE toBlack #-}
toBlack :: RBMap k a -> RBMap k a
toBlack Empty = Empty
toBlack (Node _ l r k x) = Node Black l r k x

{-# INLINE balance #-}
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

{-# INLINE empty #-}
empty :: RBMap k a
empty = Empty

{-# INLINE singleton #-}
singleton :: k -> a -> RBMap k a
singleton = Node Black Empty Empty

{-# INLINE blackHeight #-}
blackHeight :: RBMap k a -> Int
blackHeight Empty = 0
blackHeight (Node Red l _ _ _) = blackHeight l
blackHeight (Node Black l _ _ _) = blackHeight l + 1

{-# INLINE length #-}
length :: RBMap k a -> Int
length Empty = 0
length (Node _ l r _ _) = length l + length r + 1

{-# INLINE foldri #-}
foldri :: (k -> a -> b -> b) -> b -> RBMap k a -> b
foldri _ z Empty = z
foldri f z (Node _ l r k x) = foldri f (f k x (foldri f z r)) l

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> RBMap k a -> b
foldr = foldri . const

{-# INLINE foldli #-}
foldli :: (k -> a -> b -> b) -> b -> RBMap k a -> b
foldli _ z Empty = z
foldli f z (Node _ l r k x) = foldli f (f k x (foldli f z l)) r

{-# INLINE foldl #-}
foldl :: (a -> b -> b) -> b -> RBMap k a -> b
foldl = foldli . const

{-# INLINE foldiTree #-}
foldiTree :: (k -> a -> b -> b -> b) -> b -> RBMap k a -> b
foldiTree _ z Empty = z
foldiTree f z (Node _ l r k x) = f k x (foldiTree f z l) (foldiTree f z r)

{-# INLINE foldTree #-}
foldTree :: (a -> b -> b -> b) -> b -> RBMap k a -> b
foldTree = foldiTree . const

{-# INLINE mapi #-}
mapi :: (k -> a -> b) -> RBMap k a -> RBMap k b
mapi _ Empty = Empty
mapi f (Node color l r k x) = Node color (mapi f l) (mapi f r) k (f k x)

{-# INLINE map #-}
map :: (a -> b) -> RBMap k a -> RBMap k b
map = mapi . const

{-# INLINE gmap #-}
gmap :: (Ord k, Ord k') => (k -> a -> (k', b)) -> RBMap k a -> RBMap k' b
gmap f = foldri (\k -> uncurry insert . f k) empty

{-# INLINE filteri #-}
filteri :: Ord k => (k -> a -> Bool) -> RBMap k a -> RBMap k a
filteri f = flip foldri empty $ \k x ->
  if f k x then
    insert k x
  else
    id

{-# INLINE filter #-}
filter :: Ord k => (a -> Bool) -> RBMap k a -> RBMap k a
filter = filteri . const

{-# INLINE alli #-}
alli :: (k -> a -> Bool) -> RBMap k a -> Bool
alli f = foldri (\k -> (&&) . (f k)) True

{-# INLINE all #-}
all :: (a -> Bool) -> RBMap k a -> Bool
all = alli . const

{-# INLINE anyi #-}
anyi :: (k -> a -> Bool) -> RBMap k a -> Bool
anyi f = foldri (\k -> (||) . (f k)) False

{-# INLINE any #-}
any :: (a -> Bool) -> RBMap k a -> Bool
any = anyi . const

{-# INLINE lookup #-}
lookup :: Ord k => k -> RBMap k a -> Maybe a
lookup _ Empty =
  Nothing
lookup k (Node _ l0 r0 k0 x0) =
  case compare k k0 of
    LT -> lookup k l0
    EQ -> Just x0
    GT -> lookup k r0

{-# INLINE insert #-}
insert :: Ord k => k -> a -> RBMap k a -> RBMap k a
insert k x = toBlack . insert' where
  insert' Empty =
    Node Red Empty Empty k x
  insert' (Node color l0 r0 k0 x0) =
    case compare k k0 of
      LT -> balance $ Node color (insert' l0) r0 k0 x0
      EQ -> Node color l0 r0 k x
      GT -> balance $ Node color l0 (insert' r0) k0 x0

{-# INLINE delete #-}
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

{-# INLINE null #-}
null :: RBMap k a -> Bool
null Empty = True
null _ = False

{-# INLINE member #-}
member :: Ord k => k -> RBMap k a -> Bool
member k = Maybe.isJust . lookup k

{-# INLINE insertBy #-}
insertBy :: Ord k => (a -> a -> a) -> k -> a -> RBMap k a -> RBMap k a
insertBy f k x rbMap =
  case lookup k rbMap of
    Nothing ->
      insert k x rbMap
    Just x' ->
      insert k (f x x') rbMap

{-# INLINE alter #-}
alter :: Ord k => (Maybe a -> Maybe a) -> k -> RBMap k a -> RBMap k a
alter f k rbMap =
  case f (lookup k rbMap) of
    Nothing ->
      delete k rbMap
    Just x' ->
      insert k x' rbMap

{-# INLINE update #-}
update :: Ord k => (a -> Maybe a) -> k -> RBMap k a -> RBMap k a
update = alter . maybe Nothing

{-# INLINE adjust #-}
adjust :: Ord k => (a -> a) -> k -> RBMap k a -> RBMap k a
adjust f = update $ Just . f

{-# INLINE unionBy #-}
unionBy :: Ord k => (a -> a -> a) -> RBMap k a -> RBMap k a -> RBMap k a
unionBy = foldri . insertBy . flip

{-# INLINE unionsBy #-}
unionsBy :: Ord k => (a -> a -> a) -> [RBMap k a] -> RBMap k a
unionsBy f = Prelude.foldr (unionBy f) empty

{-# INLINE unionl #-}
unionl :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
unionl = unionBy const

{-# INLINE unionr #-}
unionr :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
unionr = unionBy $ const id

{-# INLINE interBy #-}
interBy :: Ord k => (a -> a -> a) -> RBMap k a -> RBMap k a -> RBMap k a
interBy f rbMap = flip foldri empty $ \k x ->
  case lookup k rbMap of
    Nothing ->
      id
    Just x' ->
      insert k $ f x' x

{-# INLINE interl #-}
interl :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
interl = interBy const

{-# INLINE interr #-}
interr :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
interr = interBy $ const id

{-# INLINE diffBy #-}
diffBy :: Ord k => (a -> a -> Maybe a) -> RBMap k a -> RBMap k a -> RBMap k a
diffBy f = foldri $ flip $ update . flip f

{-# INLINE diff #-}
diff :: Ord k => RBMap k a -> RBMap k a -> RBMap k a
diff = diffBy $ \_ _ -> Nothing

{-# INLINE keys #-}
keys :: RBMap k a -> [k]
keys = foldri (const . (:)) []

{-# INLINE elts #-}
elts :: RBMap k a -> [a]
elts = foldr (:) []

{-# INLINE maximumKey #-}
maximumKey :: Ord k => RBMap k a -> Maybe k
maximumKey = maximumKey' Nothing where
  maximumKey' z Empty = z
  maximumKey' _ (Node _ _ r k _) = maximumKey' (Just k) r

{-# INLINE minimumKey #-}
minimumKey :: Ord k => RBMap k a -> Maybe k
minimumKey = minimumKey' Nothing where
  minimumKey' z Empty = z
  minimumKey' _ (Node _ l _ k _) = minimumKey' (Just k) l

{-# INLINE maximum #-}
maximum :: Ord a => RBMap k a -> Maybe a
maximum rbMap =
  let alist = elts rbMap in
    if Prelude.null alist then
      Nothing
    else
      Just $ List.maximum alist

{-# INLINE minimum #-}
minimum :: Ord a => RBMap k a -> Maybe a
minimum rbMap =
  let alist = elts rbMap in
    if Prelude.null alist then
      Nothing
    else
      Just $ List.minimum alist

{-# INLINE toList #-}
toList :: RBMap k a -> [(k, a)]
toList = foldri (\k x -> ((k, x) :)) []

{-# INLINE fromList #-}
fromList :: Ord k => [(k, a)] -> RBMap k a
fromList =
  fromSortedList
    . List.sortBy (\(k1, _) (k2, _) -> compare k1 k2)
    . List.nubBy (\(k1, _) (k2, _) -> k1 == k2)
  where
    fromSortedList alist =
      let n = Prelude.length alist in
        if even n then
          blackFromEvenSortedList n alist
        else
          blackFromOddSortedList n alist

    blackFromEvenSortedList _ [] =
      empty
    blackFromEvenSortedList n alist =
      let m = n `div` 2 in
        case splitAt m alist of
          (left, (k, x) : right) ->
            if even m then
              let l = redFromEvenSortedList m left in
              let r = redFromOddSortedList (m - 1) right in
                Node Black l r k x
            else
              let l = redFromOddSortedList m left in
              let r = redFromEvenSortedList (m - 1) right in
                Node Black l r k x
          _ ->
            undefined

    redFromEvenSortedList _ [] =
      empty
    redFromEvenSortedList n alist =
      let m = n `div` 2 in
        case splitAt m alist of
          (left, (k, x) : right) ->
            if even m then
              let l = blackFromEvenSortedList m left in
              let r = blackFromOddSortedList (m - 1) right in
                Node Red l r k x
            else
              let l = blackFromOddSortedList m left in
              let r = blackFromEvenSortedList (m - 1) right in
                Node Red l r k x
          _ ->
            undefined

    blackFromOddSortedList _ [] =
      empty
    blackFromOddSortedList n alist =
      let m = n `div` 2 in
        case splitAt m alist of
          (left, (k, x) : right) ->
            if even m then
              let l = redFromEvenSortedList m left in
              let r = redFromEvenSortedList m right in
                Node Black l r k x
            else
              let l = redFromOddSortedList m left in
              let r = redFromOddSortedList m right in
                Node Black l r k x
          _ ->
            undefined

    redFromOddSortedList _ [] =
      empty
    redFromOddSortedList n alist =
      let m = n `div` 2 in
        case splitAt m alist of
          (left, (k, x) : right) ->
            if even m then
              let l = blackFromEvenSortedList m left in
              let r = blackFromEvenSortedList m right in
                Node Red l r k x
            else
              let l = blackFromOddSortedList m left in
              let r = blackFromOddSortedList m right in
                Node Red l r k x
          _ ->
            undefined

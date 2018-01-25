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

module Language.HsYacc where

import qualified Data.Maybe as Maybe
import qualified Data.RBMap as RBMap
import qualified Data.RBSet as RBSet

data Symbol t n = T t | N n deriving (Eq, Ord, Read, Show)
type Production t n = (n, [Symbol t n])
type Grammar t n = [Production t n]

type NullableSet t n = RBSet.RBSet n
type FirstSet t n = RBMap.RBMap (Symbol t n) (RBSet.RBSet t)
type FollowSet t n = RBMap.RBMap (Symbol t n) (RBSet.RBSet (Symbol t n))
type Sets t n = (NullableSet t n, FirstSet t n, FollowSet t n)

type Item t n = (n, [Symbol t n], [Symbol t n], t)

makeSets :: (Ord t, Ord n) => Grammar t n -> Sets t n
makeSets grm =
  let nullable0 = RBSet.empty in
  let first0 =
        RBMap.fromList $ map (\t -> (T t, RBSet.singleton t)) $ concatMap
          (\(_, right) ->
            Maybe.mapMaybe
              (\symbol ->
                case symbol of
                  T t -> Just t
                  N _ -> Nothing)
              right)
          grm in
  let follow0 = RBMap.empty in
    go nullable0 first0 follow0
  where
    go nullable0 first0 follow0 =
      let nullable' =
            foldl
              (\nullable1 (left, right) ->
                let nullable1' =
                      if length right == 0 || all (isNullable nullable1) right then
                        RBSet.insert left nullable1
                      else
                        nullable1 in
                  nullable1')
              nullable0
              grm in
      let (nullable, first, follow) =
            foldl
              (\(nullable1, first1, follow1) (left, right) ->
                foldl
                  (\(nullable2, first2, follow2) i ->
                    let first2' =
                          if i == 0 || all (isNullable nullable2) (take (i - 1) right) then
                            RBMap.insert (N left) (RBSet.union (maybe RBSet.empty id (RBMap.lookup (N left) first2)) (maybe RBSet.empty id (RBMap.lookup (right !! i) first2))) first2
                          else
                            first2 in
                    let follow2' =
                          if i + 1 == length right || all (isNullable nullable2) (drop (i + 1) right) then
                            RBMap.insert (right !! i) (RBSet.union (maybe RBSet.empty id (RBMap.lookup (N left) follow2)) (maybe RBSet.empty id (RBMap.lookup (right !! i) follow2))) follow2
                          else
                            follow2 in
                    foldl
                      (\(nullable3, first3, follow3) j ->
                        let follow4 =
                              if i + 1 == j || all (isNullable nullable3) (take (j - i - 1) (drop (i + 1) right)) then
                                RBMap.insert (right !! i) (RBSet.union (maybe RBSet.empty id (RBMap.lookup (right !! i) follow3)) (maybe RBSet.empty (RBSet.map T) (RBMap.lookup (right !! j) first3))) follow3
                              else
                                follow3 in
                          (nullable3, first3, follow4))
                      (nullable2, first2', follow2')
                      [i + 1 .. length right - 1])
                  (nullable1, first1, follow1)
                  [0 .. length right - 1])
              (nullable', first0, follow0)
              grm in
      if nullable == nullable0 && first == first0 && follow == follow0 then
        (nullable, first, follow)
      else
        go nullable first follow

    isNullable _ (T _) = False
    isNullable nullable (N n) = RBSet.member n nullable

firstSet :: (Ord t, Ord n) => [Symbol t n] -> Sets t n -> RBSet.RBSet t
firstSet [] _ = RBSet.empty
firstSet (T t : ss) (_, first, _) = maybe RBSet.empty id $ RBMap.lookup (T t) first
firstSet (N n : ss) sets @ (nullable, first, _) =
  if RBSet.member n nullable then
    RBSet.union (maybe RBSet.empty id $ RBMap.lookup (N n) first) (firstSet ss sets)
  else
    maybe RBSet.empty id $ RBMap.lookup (N n) first

closure :: (Ord t, Ord n) => RBSet.RBSet (Item t n) -> Sets t n -> Grammar t n -> RBSet.RBSet (Item t n)
closure items sets @ (_, first, _) grm =
  let items' =
        RBSet.union
          items
          (RBSet.fromList
            (concatMap
              (\(left, middle, right, lookahead) ->
                case right of
                  [] ->
                    []
                  T _ : _ ->
                    []
                  N n : ss ->
                    concatMap
                      (\(left', right') ->
                        map (\t -> (left', [], right', t)) (RBSet.toList (firstSet (ss ++ [T lookahead]) sets)))
                      (filter (\(left', _) -> left' == n) grm))
              (RBSet.toList items))) in
  if items' == items then
    items'
  else
    closure items' sets grm

goto :: (Ord t, Ord n) => RBSet.RBSet (Item t n) -> Symbol t n -> Sets t n -> Grammar t n -> RBSet.RBSet (Item t n)
goto items symbol sets grm =
  let items' =
        foldl
          (\items'' (left, middle, right, lookahead) ->
            case right of
              [] ->
                items''
              symbol' : ss ->
                if symbol' == symbol then
                  RBSet.insert (left, symbol' : middle, ss, lookahead) items''
                else
                  items'')
          RBSet.empty
          (RBSet.toList items) in
    closure items' sets grm

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

import qualified Control.Monad                as Monad
import qualified Control.Monad.CodeGenerating as CodeGenerating
import qualified Data.Char                    as Char
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.RBMap                   as RBMap
import qualified Data.RBSet                   as RBSet

data Symbol t n = T t | N n deriving (Eq, Ord, Read, Show)
type Production t n = (n, [Symbol t n])
type Grammar t n = [Production t n]

type NullableSet t n = RBSet.RBSet n
type FirstSet t n = RBMap.RBMap (Symbol t n) (RBSet.RBSet t)
type FollowSet t n = RBMap.RBMap (Symbol t n) (RBSet.RBSet (Symbol t n))
type Sets t n = (NullableSet t n, FirstSet t n, FollowSet t n)

data InternalTerminalSymbol t =
    Dollar
  | Question
  | UserTerminal t
  deriving (Eq, Ord, Read, Show)

data InternalNonterminalSymbol n =
    Start
  | UserNonterminal n
  deriving (Eq, Ord, Read, Show)

type Item t n = (n, [Symbol t n], [Symbol t n], t)

type State = Int

data Action =
    Shift State
  | Reduce State
  | Accept
  deriving (Eq, Ord, Read, Show)

type ActionTable t n = RBMap.RBMap (State, InternalTerminalSymbol t) Action
type GotoTable t n = RBMap.RBMap (State, InternalNonterminalSymbol n) State

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

makeTable :: (Ord t, Ord n) => n -> Grammar t n -> Maybe (ActionTable t n, GotoTable t n)
makeTable start grm0 =
  let grm =
        (Start, [N (UserNonterminal start), T Dollar]) : map
          (\(left, right) ->
            ( UserNonterminal left
            , map
                (\symbol ->
                  case symbol of
                    T t -> T (UserTerminal t)
                    N n -> N (UserNonterminal n))
                right ))
          grm0 in
  let sets = makeSets grm in
  let states0 = RBSet.fromList [closure (RBSet.fromList [(Start, [], [N (UserNonterminal start), T Dollar], Question)]) sets grm] in
  let edges0 = RBSet.empty in
  let (states, edges) = makeStatesAndEdges states0 edges0 sets grm in
  let reduces0 = RBSet.empty in
  let reduces = makeReduces reduces0 states in
  let statesIndex = RBMap.fromList $ zip (RBSet.toList states) [0..] in
  let actionTable0 =
        RBMap.fromList $ Maybe.mapMaybe
          (\items ->
            let p = maybe undefined id $ RBMap.lookup items statesIndex in
              if RBSet.member (Start, [N (UserNonterminal start)], [T Dollar], Question) items then
                Just ((p, Dollar), Accept)
              else
                Nothing)
          (RBSet.toList states) in
  let actionTable1 =
        RBMap.fromList $ Maybe.mapMaybe
          (\(items, symbol, items') ->
            let p = maybe undefined id $ RBMap.lookup items statesIndex in
            let q = maybe undefined id $ RBMap.lookup items' statesIndex in
              case symbol of
                N _ ->
                  Nothing
                T t ->
                  Just ((p, t), Shift q))
          (RBSet.toList edges) in
  let gotoTable =
        RBMap.fromList $ Maybe.mapMaybe
          (\(items, symbol, items') ->
            let p = maybe undefined id $ RBMap.lookup items statesIndex in
            let q = maybe undefined id $ RBMap.lookup items' statesIndex in
              case symbol of
                N n ->
                  Just ((p, n), q)
                T _ ->
                  Nothing)
          (RBSet.toList edges) in
  let actionTable2list =
        map
          (\(items, lookahead, left, right) ->
            let p = maybe undefined id $ RBMap.lookup items statesIndex in
            let q = maybe undefined fst $ List.find (\(i, (left', right')) -> left' == left && right' == right) $ zip [0..] grm in
              [((p, lookahead), Reduce (q - 1))])
          (RBSet.toList reduces) in
  let actionTable2 = RBMap.fromList $ concat actionTable2list in
    if
        -- shift/reduce conflict check.
        RBSet.null (RBSet.inter (RBSet.fromList (RBMap.keys actionTable1)) (RBSet.fromList (RBMap.keys actionTable2)))
      &&
        -- TODO: reduce/reduce conflict check.
        True
    then
      Just (RBMap.unionl (RBMap.unionl actionTable0 actionTable1) actionTable2, gotoTable)
    else
      Nothing -- shift/reduce or reduce/reduce conflict.
  where
    makeStatesAndEdges states edges sets grm =
      let (states', edges') =
            foldl
              (\(states0, edges0) items ->
                foldl
                  (\(states1, edges1) (left, middle, right, lookahead) ->
                    case right of
                      [] ->
                        (states1, edges1)
                      symbol : ss ->
                        case symbol of
                          T Dollar ->
                            (states1, edges1)
                          _ ->
                            let items' = goto items symbol sets grm in
                            let states2 = RBSet.insert items' states1 in
                            let edges2 = RBSet.insert (items, symbol, items') edges1 in
                              (states2, edges2))
                  (states0, edges0)
                  (RBSet.toList items))
              (states, edges)
              (RBSet.toList states) in
        if states' == states && edges' == edges then
          (states', edges')
        else
          makeStatesAndEdges states' edges' sets grm

    makeReduces reduces states =
      foldl
        (\reduces0 items ->
          foldl
            (\reduces1 (left, middle, right, lookahead) ->
              case right of
                [] ->
                  RBSet.insert (items, lookahead, left, reverse middle) reduces1
                _ ->
                  reduces1)
            reduces0
            (RBSet.toList items))
        reduces
        (RBSet.toList states)

generateParser :: String -> String -> Grammar String String -> Maybe String
generateParser modid start0 grm0 = do
  let terminals =
        List.nub $ concatMap
          (\(_, right) ->
            Maybe.mapMaybe
              (\symbol ->
                case symbol of
                  T t -> Just t
                  N _ -> Nothing)
              right)
          grm0

  let nonterminals =
        List.nub $ concatMap
          (\(left, right) ->
            left : Maybe.mapMaybe
              (\symbol ->
                case symbol of
                  T _ -> Nothing
                  N n -> Just n)
              right)
          grm0

  let terminalIndex = RBMap.fromList $ zip terminals [(0 :: Int)..]
  let indexTerminal = RBMap.fromList $ zip [(0 :: Int)..] terminals
  let nonterminalIndex = RBMap.fromList $ zip nonterminals [(0 :: Int)..]

  let start = maybe undefined id $ RBMap.lookup start0 nonterminalIndex

  let grm =
        map
          (\(left, right) ->
            ( maybe undefined id $ RBMap.lookup left nonterminalIndex
            , map
                (\symbol ->
                  case symbol of
                    T t -> T (maybe undefined id $ RBMap.lookup t terminalIndex)
                    N n -> N (maybe undefined id $ RBMap.lookup n nonterminalIndex))
                right ))
          grm0

  (actionTable, gotoTable) <- makeTable start grm

  let actionTable_list = RBMap.toList actionTable
  let gotoTable_list = RBMap.toList gotoTable

  return $ CodeGenerating.generate $ do
    CodeGenerating.write "import qualified Control.Monad as Monad\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "data Token =\n"

    Monad.forM_ (zip (RBMap.toList terminalIndex) [(0 :: Int)..]) $ \((t, _), i) -> do
      if i == 0 then
        CodeGenerating.write "    "
      else
        CodeGenerating.write "  | "

      CodeGenerating.write t
      CodeGenerating.write "\n"

    CodeGenerating.write "  deriving (Eq, Ord, Read, Show)\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "data Action = Shift Int | Reduce Int Int | Accept\n"
    CodeGenerating.write "type ActionState = Int\n"
    CodeGenerating.write "data ActionSymbol = Token Token | EOF\n"
    CodeGenerating.write "type GotoState = Int\n"
    CodeGenerating.write "type GotoSymbol = Int\n"
    CodeGenerating.write "\n"

    CodeGenerating.write "data StackValue =\n"
    CodeGenerating.write "    StackValueToken\n"

    Monad.forM_ nonterminals $ \n -> do
      CodeGenerating.write "  | StackValue_"
      CodeGenerating.write n
      CodeGenerating.write " "
      case n of
        "" ->
          CodeGenerating.write ""
        (c : cs) ->
          CodeGenerating.write $ Char.toUpper c : cs
      CodeGenerating.write "\n"

    CodeGenerating.write "\n"

    CodeGenerating.write "data SemanticActions m = SemanticActions\n"
    Monad.forM_ (zip grm0 [0..]) $ \((left, right), i) -> do
      if i == 0 then
        CodeGenerating.write "  { "
      else
        CodeGenerating.write "  , "

      CodeGenerating.write left
      CodeGenerating.write "_implies"

      Monad.forM_ right $ \symbol -> do
        CodeGenerating.write "_"

        case symbol of
          T t ->
            CodeGenerating.write t
          N n ->
            CodeGenerating.write n

      CodeGenerating.write " :: "

      Monad.forM_ right $ \symbol -> do
        case symbol of
          T _ ->
            return ()
          N "" ->
            CodeGenerating.write " -> "
          N (c : cs) -> do
            CodeGenerating.write (Char.toUpper c : cs)
            CodeGenerating.write " -> "

      case left of
        "" ->
          CodeGenerating.write "m ()"
        (c : cs) -> do
          CodeGenerating.write "m "
          CodeGenerating.write (Char.toUpper c : cs)

      if i == length grm - 1 then
        CodeGenerating.write " }\n\n"
      else
        CodeGenerating.write "\n"

    CodeGenerating.write "dfaActionTransition :: ActionState -> ActionSymbol -> Action\n"
    CodeGenerating.write "dfaActionTransition q s =\n"
    CodeGenerating.write "  case (q, s) of\n"

    Monad.forM_ actionTable_list $ \((p, t), q) -> do
      CodeGenerating.write "    "
      CodeGenerating.write "("
      CodeGenerating.write $ show p
      CodeGenerating.write ", "

      case t of
        UserTerminal t' -> do
          CodeGenerating.write "Token "
          CodeGenerating.write $ maybe undefined id $ RBMap.lookup t' indexTerminal
        Dollar ->
          CodeGenerating.write "EOF"
        Question ->
          undefined

      CodeGenerating.write ")"
      CodeGenerating.write " -> "

      case q of
        Shift n -> do
          CodeGenerating.write "Shift "
          CodeGenerating.write $ show n
        Reduce k -> do
          let (_, right) = grm !! k

          CodeGenerating.write "Reduce "
          CodeGenerating.write $ show $ length right
          CodeGenerating.write " "
          CodeGenerating.write $ show k
        Accept -> do
          CodeGenerating.write "Accept"

      CodeGenerating.write "\n"

    CodeGenerating.write "\n"

    CodeGenerating.write "production :: Int -> Int\n"
    Monad.forM_ (zip grm [(0 :: Int)..]) $ \((left, right), i) -> do
      CodeGenerating.write "production "
      CodeGenerating.write $ show i
      CodeGenerating.write " = "
      CodeGenerating.write $ show left
      CodeGenerating.write "\n"
    CodeGenerating.write "\n"

    CodeGenerating.write "dfaGotoTransition :: GotoState -> GotoSymbol -> GotoState\n"
    CodeGenerating.write "dfaGotoTransition q s =\n"
    CodeGenerating.write "  case (q, production s) of\n"

    Monad.forM_ gotoTable_list $ \((p, n), q) -> do
      CodeGenerating.write "    "
      CodeGenerating.write "("
      CodeGenerating.write $ show p
      CodeGenerating.write ", "

      case n of
        UserNonterminal n' ->
          CodeGenerating.write $ show n'
        Start ->
          undefined

      CodeGenerating.write ")"
      CodeGenerating.write " -> "
      CodeGenerating.write $ show q
      CodeGenerating.write "\n"

    CodeGenerating.write "parse :: Monad m => SemanticActions m -> [Token] -> m (Maybe ("
    case start0 of
      "" ->
        CodeGenerating.write ""
      (c : cs) ->
        CodeGenerating.write $ Char.toUpper c : cs
    CodeGenerating.write ", [Token]))\n"
    CodeGenerating.write "parse actions = parse' [] where\n"
    CodeGenerating.write "  parse' stack tokens =\n"
    CodeGenerating.write "    let p =\n"
    CodeGenerating.write "          case stack of\n"
    CodeGenerating.write "            [] -> 0\n"
    CodeGenerating.write "            ((q, _) : _) -> q in\n"
    CodeGenerating.write "    let symbol =\n"
    CodeGenerating.write "          case tokens of\n"
    CodeGenerating.write "            [] -> EOF\n"
    CodeGenerating.write "            (token : _) -> Token token in\n"
    CodeGenerating.write "      case dfaActionTransition p symbol of\n"
    CodeGenerating.write "        Shift n ->\n"
    CodeGenerating.write "          parse' ((n, StackValueToken) : stack) (tail tokens)\n"
    CodeGenerating.write "        Reduce n m ->\n"
    CodeGenerating.write "          let (pop, stack') = splitAt n stack in\n"
    CodeGenerating.write "          let q =\n"
    CodeGenerating.write "                case stack' of\n"
    CodeGenerating.write "                  [] -> dfaGotoTransition 0 m\n"
    CodeGenerating.write "                  ((q', _) : _) -> dfaGotoTransition q' m in do\n"
    CodeGenerating.write "            value <-\n"
    CodeGenerating.write "              case m of\n"

    Monad.forM_ (zip grm0 [(0 :: Int)..]) $ \((left, right), i) -> do
      CodeGenerating.write "                "
      CodeGenerating.write $ show i
      CodeGenerating.write " ->\n"
      CodeGenerating.write "                  Monad.liftM StackValue_"
      CodeGenerating.write left
      CodeGenerating.write " $ "
      CodeGenerating.write left
      CodeGenerating.write "_implies"

      Monad.forM_ right $ \symbol -> do
        CodeGenerating.write "_"

        case symbol of
          T t ->
            CodeGenerating.write t
          N n ->
            CodeGenerating.write n

      CodeGenerating.write " actions"

      Monad.forM_ (zip right [(0 :: Int)..]) $ \(symbol, j) -> do
        case symbol of
          T _ ->
            return ()
          N n -> do
            CodeGenerating.write " (case snd (pop !! "
            CodeGenerating.write $ show $ length right - j - 1
            CodeGenerating.write ") of { StackValue"
            CodeGenerating.write "_"
            CodeGenerating.write n
            CodeGenerating.write " value -> value; _ -> undefined })"
      CodeGenerating.write "\n"

    CodeGenerating.write "            parse' ((q, value) : stack') tokens\n"
    CodeGenerating.write "        Accept ->\n"
    CodeGenerating.write "          case stack of { [(_, StackValue_"
    CodeGenerating.write start0
    CodeGenerating.write " value)] -> return $ Just (value, tokens); _ -> return Nothing }\n"

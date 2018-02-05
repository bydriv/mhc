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
import qualified Control.Monad.Identity       as Identity
import qualified Control.Monad.Trans          as MonadTrans
import qualified Control.Monad.Trans.State    as StateT
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

type MemoT k v m = StateT.StateT (RBMap.RBMap k v) m

memo :: (Ord s, Monad m) => (s -> m a) -> s -> MemoT s a m a
memo f s = do
  m <- StateT.get
  case RBMap.lookup s m of
    Nothing -> do
      x <- MonadTrans.lift $ f s
      let m' = RBMap.insert s x m
      StateT.put m'
      return x
    Just x ->
      return x

makeNullableSet :: (Ord t, Ord n) => Grammar t n -> NullableSet t n
makeNullableSet grm =
  let nullable0 = RBSet.empty in
    go nullable0
  where
    go nullable0 =
      let nullable =
            foldl
              (\nullable1 (left, right) ->
                let nullable2 =
                      if length right == 0 || all (isNullable nullable1) right then
                        RBSet.insert left nullable1
                      else
                        nullable1 in
                  nullable2)
              nullable0
              grm in
      if nullable == nullable0 then
        nullable
      else
        go nullable

makeFirstSet :: (Ord t, Ord n) => NullableSet t n -> Grammar t n -> FirstSet t n
makeFirstSet nullable grm =
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
    go first0
  where
    go first0 =
      let first =
            foldl
              (\first1 (left, right) ->
                foldl
                  (\first2 i ->
                    let first3 =
                          if i == 0 || all (isNullable nullable) (take (i - 1) right) then
                            RBMap.insert (N left) (RBSet.union (maybe RBSet.empty id (RBMap.lookup (N left) first2)) (maybe RBSet.empty id (RBMap.lookup (right !! i) first2))) first2
                          else
                            first2 in
                      first3)
                  first1
                  [0 .. length right - 1])
              first0
              grm in
      if first == first0 then
        first
      else
        go first

makeFollowSet :: (Ord t, Ord n) => NullableSet t n -> FirstSet t n -> Grammar t n -> FollowSet t n
makeFollowSet nullable first grm =
  let follow0 = RBMap.empty in
    go follow0
  where
    go follow0 =
      let follow =
            foldl
              (\follow1 (left, right) ->
                foldl
                  (\follow2 i ->
                    let follow3 =
                          if i + 1 == length right || all (isNullable nullable) (drop (i + 1) right) then
                            RBMap.insert (right !! i) (RBSet.union (maybe RBSet.empty id (RBMap.lookup (N left) follow2)) (maybe RBSet.empty id (RBMap.lookup (right !! i) follow2))) follow2
                          else
                            follow2 in
                    foldl
                      (\follow4 j ->
                        let follow5 =
                              if i + 1 == j || all (isNullable nullable) (take (j - i - 1) (drop (i + 1) right)) then
                                RBMap.insert (right !! i) (RBSet.union (maybe RBSet.empty id (RBMap.lookup (right !! i) follow4)) (maybe RBSet.empty (RBSet.map T) (RBMap.lookup (right !! j) first))) follow4
                              else
                                follow4 in
                          follow5)
                      follow3
                      [i + 1 .. length right - 1])
                  follow1
                  [0 .. length right - 1])
              follow0
              grm in
      if follow == follow0 then
        follow
      else
        go follow

makeSets :: (Ord t, Ord n) => Grammar t n -> Sets t n
makeSets grm =
  let nullable = makeNullableSet grm in
  let first = makeFirstSet nullable grm in
  let follow = makeFollowSet nullable first grm in
    (nullable, first, follow)

isNullable :: (Ord t, Ord n) => NullableSet t n -> Symbol t n -> Bool
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

goto
  :: (Ord t, Ord n, Monad m)
  => Sets t n
  -> Grammar t n
  -> (RBSet.RBSet (Item t n), Symbol t n)
  -> MemoT
      (RBSet.RBSet (Item t n), Symbol t n)
      (RBSet.RBSet (Item t n))
      (MemoT (RBSet.RBSet (Item t n)) (RBSet.RBSet (Item t n)) m)
      (RBSet.RBSet (Item t n))
goto sets grm = memo $ \(items, symbol) ->
  let items' =
        RBSet.foldl
          (\(left, middle, right, lookahead) items'' ->
            case right of
              [] ->
                items''
              symbol' : ss ->
                if symbol' == symbol then
                  RBSet.insert (left, symbol' : middle, ss, lookahead) items''
                else
                  items'')
          RBSet.empty
          items in
    closure sets grm items'

closure
  :: (Ord t, Ord n, Monad m)
  => Sets t n
  -> Grammar t n
  -> RBSet.RBSet (Item t n)
  -> MemoT
      (RBSet.RBSet (Item t n))
      (RBSet.RBSet (Item t n))
      m
      (RBSet.RBSet (Item t n))
closure sets @ (_, first, _) grm = memo $ \items ->
    return $ closure' items RBSet.empty
  where
    closure' items1 items2 =
      let items3 =
            RBSet.fromList
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
                (RBSet.toList items1)) in
      let items4 = RBSet.union items1 items2 in
        if RBSet.subset items3 items4 then
          items4
        else
          closure' (RBSet.diff items3 items4) items4

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
  let states0 = RBSet.fromList [Identity.runIdentity (StateT.evalStateT (closure sets grm (RBSet.fromList [(Start, [], [N (UserNonterminal start), T Dollar], Question)])) RBMap.empty)] in
  let edges0 = RBSet.empty in
  let (states1, edges1) = makeStatesAndEdges states0 RBSet.empty edges0 RBSet.empty sets grm (RBMap.empty, RBMap.empty) in
  let (states, edges) = makeLALR1 states1 edges1 in
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
              ((p, lookahead), Reduce (q - 1)))
          (RBSet.toList reduces) in
  let actionTable2 = RBMap.fromList $ actionTable2list in
    if
        -- shift/reduce conflict check.
        RBSet.null (RBSet.inter (RBSet.fromList (RBMap.keys actionTable1)) (RBSet.fromList (RBMap.keys actionTable2)))
      &&
        -- reduce/reduce conflict check.
        length (List.nub (map fst (List.nub actionTable2list))) == length (List.nub actionTable2list)
    then
      Just (RBMap.unionl (RBMap.unionl actionTable0 actionTable1) actionTable2, gotoTable)
    else
      Nothing -- shift/reduce or reduce/reduce conflict.
  where
    makeStatesAndEdges states accStates edges accEdges sets grm mem =
      let (states', edges', mem') =
            RBSet.foldl
              (\items (states0, edges0, mem0) ->
                RBSet.foldl
                  (\(left, middle, right, lookahead) (states1, edges1, mem1) ->
                    case right of
                      [] ->
                        (states1, edges1, mem1)
                      symbol : ss ->
                        case symbol of
                          T Dollar ->
                            (states1, edges1, mem1)
                          _ ->
                            let ((items', closmem), gotomem) =
                                  Identity.runIdentity
                                    (StateT.runStateT
                                      (StateT.runStateT
                                        (goto sets grm (items, symbol))
                                        (fst mem1))
                                      (snd mem1)) in
                            let states2 = RBSet.insert items' states1 in
                            let edges2 = RBSet.insert (items, symbol, items') edges1 in
                              (states2, edges2, (closmem, gotomem)))
                  (states0, edges0, mem0)
                  items)
              (RBSet.empty, RBSet.empty, mem)
              states in
        let states'' = RBSet.union states accStates in
        let edges'' = RBSet.union edges accEdges in
          if RBSet.subset states' states'' && RBSet.subset edges' edges'' then
            (states'', edges'')
          else
            makeStatesAndEdges (RBSet.diff states' states'') states'' (RBSet.diff edges' edges'') edges'' sets grm mem'

    makeLALR1 states0 edges0 =
      let states = RBSet.fromList $ makeLALR1States $ RBSet.toList states0 in
      let edges = RBSet.fromList $ map (\(items, symbol, items') -> (maybe undefined id (List.find (lalrEqItems items) (RBSet.toList states)), symbol, maybe undefined id (List.find (lalrEqItems items') (RBSet.toList states)))) (RBSet.toList edges0) in
        (states, edges)
      where
        makeLALR1States [] = []
        makeLALR1States (items : states1) =
          let states = makeLALR1States states1 in
            case List.find (lalrEqItems items) states of
              Nothing ->
                items : states
              Just items' ->
                RBSet.union items items' : filter (/= items') states

        lalrEqItems items0 items0' =
          let items = RBSet.fromList $ List.nubBy lalrEqItem $ RBSet.toList items0 in
          let items' = RBSet.fromList $ List.nubBy lalrEqItem $ RBSet.toList items0' in
            length (RBSet.toList items) == length (RBSet.toList items')
          &&
            all (uncurry lalrEqItem) (zip (RBSet.toList items) (RBSet.toList items'))

        lalrEqItem (left, middle, right, _) (left', middle', right', _) =
          left == left' && middle == middle' && right == right'

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

generateParser :: Bool -> String -> String -> String -> String -> Grammar String String -> Maybe String
generateParser trivial modid start0 header footer grm0 = do
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
  --let indexTerminal = RBMap.fromList $ zip [(0 :: Int)..] terminals
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

  --let actionTable_list = RBMap.toList actionTable
  --let gotoTable_list = RBMap.toList gotoTable

  return $ CodeGenerating.generate $ do
    CodeGenerating.write "module "
    CodeGenerating.write modid
    CodeGenerating.write " where\n"
    CodeGenerating.write "import qualified Control.Monad as Monad\n"
    CodeGenerating.write "\n"
    CodeGenerating.write header
    CodeGenerating.write "\n"
    CodeGenerating.write "data Token =\n"

    Monad.forM_ (zip (RBMap.toList terminalIndex) [(0 :: Int)..]) $ \((t, _), i) -> do
      if i == 0 then
        CodeGenerating.write "    "
      else
        CodeGenerating.write "  | "

      CodeGenerating.write t
      CodeGenerating.write " "
      CodeGenerating.write t
      CodeGenerating.write "\n"

    CodeGenerating.write "  deriving (Eq, Ord, Read, Show)\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "data Action = Shift Int | Reduce Int Int | Accept\n"
    CodeGenerating.write "type ActionState = Int\n"
    CodeGenerating.write "data ActionSymbol = Token Token | EOF\n"
    CodeGenerating.write "  deriving (Eq, Ord, Read, Show)\n"
    CodeGenerating.write "type GotoState = Int\n"
    CodeGenerating.write "type GotoSymbol = Int\n"
    CodeGenerating.write "\n"

    Monad.when trivial $ do
      Monad.forM_ nonterminals $ \n -> do
        CodeGenerating.write "data "
        case n of
          [] ->
            return ()
          (c : cs) ->
            CodeGenerating.write $ Char.toUpper c : cs
        CodeGenerating.write " =\n"

        Monad.forM_ (zip (filter (\(left, _) -> left == n) grm0) [(0 :: Int)..]) $ \((left, right), i) -> do
          if i == 0 then
            CodeGenerating.write "    "
          else
            CodeGenerating.write "  | "

          case left of
            [] ->
              return ()
            (c : cs) ->
              CodeGenerating.write $ Char.toUpper c : cs

          CodeGenerating.write "_implies"

          Monad.forM_ right $ \symbol -> do
            CodeGenerating.write "_"

            case symbol of
              T t ->
                CodeGenerating.write t
              N n' ->
                CodeGenerating.write n'

          Monad.forM_ right $ \symbol -> do
            case symbol of
              T t -> do
                CodeGenerating.write " "
                CodeGenerating.write t
              N n' -> do
                CodeGenerating.write " "
                case n' of
                  [] ->
                    return ()
                  (c : cs) ->
                    CodeGenerating.write $ Char.toUpper c : cs

          CodeGenerating.write "\n"

        CodeGenerating.write "  deriving (Eq, Ord, Read, Show)\n"
        CodeGenerating.write "\n"

    CodeGenerating.write "data StackValue =\n"
    CodeGenerating.write "    StackValue_EOF\n"

    Monad.forM_ terminals $ \t -> do
      CodeGenerating.write "  | StackValue_"
      CodeGenerating.write t
      CodeGenerating.write " "
      CodeGenerating.write t
      CodeGenerating.write "\n"

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
          T t -> do
            CodeGenerating.write t
            CodeGenerating.write " -> "
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

    CodeGenerating.write "dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action\n"
    CodeGenerating.write "dfaActionTransition q s =\n"
    CodeGenerating.write "  let s' =\n"
    CodeGenerating.write "        case s of\n"
    CodeGenerating.write "          EOF -> -1\n"

    Monad.forM_ (RBMap.toList terminalIndex) $ \(t, i) -> do
      CodeGenerating.write "          Token ("
      CodeGenerating.write t
      CodeGenerating.write " _) -> "
      CodeGenerating.write $ show i
      CodeGenerating.write "\n"
    CodeGenerating.write "  in\n"

    RBMap.foldiTree
      (\(p, t) q lm rm -> do
        CodeGenerating.write "    case compare (q, s') "
        CodeGenerating.write "("
        CodeGenerating.write $ show p
        CodeGenerating.write ", "

        case t of
          UserTerminal t' -> do
            CodeGenerating.write $ show t'
          Dollar ->
            CodeGenerating.write "-1"
          Question ->
            undefined

        CodeGenerating.write ") of {\n"
        CodeGenerating.write "      LT ->\n"
        lm
        CodeGenerating.write ";\n"
        CodeGenerating.write "      EQ ->\n"
        CodeGenerating.write "        Just ("

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
        CodeGenerating.write ");\n"
        CodeGenerating.write "      GT ->\n"
        rm
        CodeGenerating.write "\n"
        CodeGenerating.write "    }")
      (CodeGenerating.write "        Nothing")
      actionTable

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

    CodeGenerating.write "dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState\n"
    CodeGenerating.write "dfaGotoTransition q s =\n"
    CodeGenerating.write "  let s' = production s in\n"

    RBMap.foldiTree
      (\(p, n) q lm rm -> do
        CodeGenerating.write "    case compare (q, s') "
        CodeGenerating.write "("
        CodeGenerating.write $ show p
        CodeGenerating.write ", "

        case n of
          UserNonterminal n' ->
            CodeGenerating.write $ show n'
          Start ->
            undefined

        CodeGenerating.write ") of {\n"
        CodeGenerating.write "      LT ->\n"
        lm
        CodeGenerating.write ";\n"
        CodeGenerating.write "      EQ ->\n"
        CodeGenerating.write "        Just "
        CodeGenerating.write $ show q
        CodeGenerating.write ";\n"
        CodeGenerating.write "      GT ->\n"
        rm
        CodeGenerating.write "\n"
        CodeGenerating.write "    }")
      (CodeGenerating.write "        Nothing")
      gotoTable

    CodeGenerating.write "\n"
    CodeGenerating.write "\n"

    CodeGenerating.write "parse :: Monad m => SemanticActions m -> [Token] -> m (Either (Maybe Token) ("
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
    CodeGenerating.write "            (token : _) -> Token token in do\n"
    CodeGenerating.write "      case dfaActionTransition p symbol of\n"
    CodeGenerating.write "        Nothing ->\n"
    CodeGenerating.write "          case tokens of\n"
    CodeGenerating.write "            [] -> return $ Left $ Nothing\n"
    CodeGenerating.write "            (token : _) -> return $ Left $ Just token\n"
    CodeGenerating.write "        Just (Shift n) ->\n"
    CodeGenerating.write "          let value =\n"
    CodeGenerating.write "                case symbol of\n"
    CodeGenerating.write "                  EOF ->\n"
    CodeGenerating.write "                    StackValue_EOF\n"

    Monad.forM_ terminals $ \t -> do
      CodeGenerating.write "                  Token ("
      CodeGenerating.write t
      CodeGenerating.write " semanticValue) ->\n"
      CodeGenerating.write "                    StackValue_"
      CodeGenerating.write t
      CodeGenerating.write " semanticValue\n"

    CodeGenerating.write "          in parse' ((n, value) : stack) (tail tokens)\n"
    CodeGenerating.write "        Just (Reduce n m) ->\n"
    CodeGenerating.write "          let (pop, stack') = splitAt n stack in\n"
    CodeGenerating.write "            case\n"
    CodeGenerating.write "              case stack' of\n"
    CodeGenerating.write "                [] -> dfaGotoTransition 0 m\n"
    CodeGenerating.write "                ((q', _) : _) -> dfaGotoTransition q' m of\n"
    CodeGenerating.write "              Nothing ->\n"
    CodeGenerating.write "                case tokens of\n"
    CodeGenerating.write "                  [] -> return $ Left $ Nothing\n"
    CodeGenerating.write "                  (token : _) -> return $ Left $ Just token\n"
    CodeGenerating.write "              Just q -> do\n"
    CodeGenerating.write "                value <-\n"
    CodeGenerating.write "                  case m of\n"

    Monad.forM_ (zip grm0 [(0 :: Int)..]) $ \((left, right), i) -> do
      CodeGenerating.write "                    "
      CodeGenerating.write $ show i
      CodeGenerating.write " ->\n"
      CodeGenerating.write "                      Monad.liftM StackValue_"
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
          T t -> do
            CodeGenerating.write " (case snd (pop !! "
            CodeGenerating.write $ show $ length right - j - 1
            CodeGenerating.write ") of { StackValue"
            CodeGenerating.write "_"
            CodeGenerating.write t
            CodeGenerating.write " value -> value; _ -> undefined })"
          N n -> do
            CodeGenerating.write " (case snd (pop !! "
            CodeGenerating.write $ show $ length right - j - 1
            CodeGenerating.write ") of { StackValue"
            CodeGenerating.write "_"
            CodeGenerating.write n
            CodeGenerating.write " value -> value; _ -> undefined })"
      CodeGenerating.write "\n"

    CodeGenerating.write "                parse' ((q, value) : stack') tokens\n"
    CodeGenerating.write "        Just Accept ->\n"
    CodeGenerating.write "          case stack of { [(_, StackValue_"
    CodeGenerating.write start0
    CodeGenerating.write " value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}\n"
    CodeGenerating.write "\n"
    CodeGenerating.write footer

    Monad.when trivial $ do
      CodeGenerating.write "semanticActions :: Monad m => SemanticActions m\n"
      CodeGenerating.write "semanticActions = SemanticActions\n"
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

        if (not (null right)) then do
          CodeGenerating.write " = \\"

          Monad.forM_ (zip right [(0 :: Int)..]) $ \(symbol, j) -> do
            case symbol of
              T "" -> do
                CodeGenerating.write " "
              T (c : cs) -> do
                CodeGenerating.write $ (Char.toLower c : cs) ++ show j
                CodeGenerating.write " "
              N n -> do
                CodeGenerating.write $ n ++ show j
                CodeGenerating.write " "

          CodeGenerating.write "->\n"
        else
          CodeGenerating.write " =\n"

        CodeGenerating.write "      return $ "

        case left of
          [] ->
            return ()
          (c : cs) ->
            CodeGenerating.write $ Char.toUpper c : cs

        CodeGenerating.write "_implies"

        Monad.forM_ right $ \symbol -> do
          CodeGenerating.write "_"

          case symbol of
            T t ->
              CodeGenerating.write t
            N n ->
              CodeGenerating.write n

        Monad.forM_ (zip right [(0 :: Int)..]) $ \(symbol, j) -> do
          case symbol of
            T "" -> do
              CodeGenerating.write " "
            T (c : cs) -> do
              CodeGenerating.write " "
              CodeGenerating.write $ (Char.toLower c : cs) ++ show j
            N n -> do
              CodeGenerating.write " "
              CodeGenerating.write $ n ++ show j

        if i == length grm - 1 then
          CodeGenerating.write " }\n"
        else
          CodeGenerating.write "\n"

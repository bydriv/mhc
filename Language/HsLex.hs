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

module Language.HsLex
  ( Regexp
  , empty
  , epsilon
  , to
  , generalCategory
  , (>>>)
  , (|||)
  , many
  , some
  , optional
  , any
  , char
  , string
  , oneOf
  , generateLexer ) where

import           Prelude
  hiding (any, lex, not)
import qualified Prelude
import qualified Control.Monad                as Monad
import qualified Control.Monad.CodeGenerating as CodeGenerating
import qualified Control.Monad.State          as State
import qualified Data.Char                    as Char
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.RBMap                   as RBMap
import qualified Data.RBSet                   as RBSet
import qualified Data.Word                    as Word

infix 5 `to`
infixr 4 :::, >>>
infixr 2 :|:, |||

type Range = (Char, Char)

data Regexp =
    Empty
  | Epsilon
  | Ranges [Range]
  | Regexp ::: Regexp
  | Regexp :|: Regexp
  | Many Regexp
  deriving (Eq, Ord, Read, Show)

type NFAState = Word.Word64

data NFALabel =
    NFAEpsilon
  | NFARanges [Range]
  deriving (Eq, Ord, Read, Show)

type NFATransition = RBMap.RBMap (NFAState, NFALabel) (RBSet.RBSet NFAState)

data NFA = NFA
  { nfaTransition :: NFATransition
  , nfaInitialState :: NFAState
  , nfaFinalStates :: RBSet.RBSet NFAState }
  deriving (Eq, Ord, Read, Show)

type DFAState = Word.Word64

data DFALabel =
    DFARanges [Range]
  deriving (Eq, Ord, Read, Show)

type DFATransition = RBMap.RBMap (DFAState, DFALabel) DFAState

data DFA = DFA
  { dfaInitialState :: DFAState
  , dfaFinalStates :: RBSet.RBSet DFAState
  , dfaTransition :: DFATransition }
  deriving (Eq, Ord, Read, Show)

data RegexpToNFAState = RegexpToNFAState
  { nfaFreshState :: NFAState }
  deriving (Eq, Ord, Read, Show)

type RegexpToNFA = State.State RegexpToNFAState

data NFAToDFAState = NFAToDFAState
  { dfaStates :: RBMap.RBMap DFAState (RBSet.RBSet NFAState)
  , dfaFreshState :: DFAState
  , dfaCurrentState :: DFAState
  , dfaTrans :: DFATransition }
  deriving (Eq, Ord, Read, Show)

type NFAToDFA = State.State NFAToDFAState

nullRange :: Range -> Bool
nullRange (x, y) = x > y

subsetRange :: Range -> Range -> Bool
subsetRange (x, y) (x', y') = x' <= x && y <= y'

empty :: Regexp
empty = Empty

epsilon :: Regexp
epsilon = Epsilon

to :: Char -> Char -> Regexp
c1 `to` c2 = Ranges [(c1, c2)]

generalCategory :: Char.GeneralCategory -> Regexp
generalCategory = maybe Empty Ranges . flip RBMap.lookup generalCategoryInfo

(>>>) :: Regexp -> Regexp -> Regexp
(>>>) = (:::)

(|||) :: Regexp -> Regexp -> Regexp
(|||) = (:|:)

many :: Regexp -> Regexp
many = Many

some :: Regexp -> Regexp
some regexp = regexp ::: Many regexp

optional :: Regexp -> Regexp
optional = (:|: Epsilon)

any :: Regexp
any = Ranges [(minBound, maxBound)]

char :: Char -> Regexp
char c = Ranges [(c, c)]

string :: String -> Regexp
string = foldr (:::) Epsilon . map char

oneOf :: [Char] -> Regexp
oneOf = Ranges . minimizeRanges . map (\c -> (c, c)) . List.nub . List.sort
  where
    minimizeRanges [] = []
    minimizeRanges [r] = [r]
    minimizeRanges (r1 : r2 : rs) =
      let (c1, c2) = r1 in
      let (c3, c4) = r2 in
        if c2 == maxBound then
          [(c1, c2)]
        else if succ c2 == c3 then
          minimizeRanges ((c1, c4) : rs)
        else
          r1 : minimizeRanges (r2 : rs)

groupByGeneralCategory :: Range -> [(Char.GeneralCategory, Range)]
groupByGeneralCategory =
  map (\(gcat, cs) -> (gcat, (head cs, last cs))) .
    map (\tmp -> (fst (head tmp), map snd tmp)) .
      List.groupBy (\(gcat, _) (gcat', _) -> gcat == gcat') .
        map (\c -> (Char.generalCategory c, c)) .
          uncurry enumFromTo

generalCategoryInfo :: RBMap.RBMap Char.GeneralCategory [Range]
generalCategoryInfo =
  RBMap.fromList $
    map (\tmp -> (fst (head tmp), map snd tmp)) $
      List.groupBy (\(gcat, _) (gcat', _) -> gcat == gcat') $
        List.sortBy (\(gcat, _) (gcat', _) -> compare gcat gcat') $
          groupByGeneralCategory (minBound, maxBound)

runRegexpToNFA :: RegexpToNFA a -> a
runRegexpToNFA = flip State.evalState $ RegexpToNFAState { nfaFreshState = 1 }

generateFreshNFAState :: RegexpToNFA NFAState
generateFreshNFAState = do
  nfaFreshState' <- State.gets nfaFreshState
  State.modify $ \s -> s { nfaFreshState = nfaFreshState s + 1 }
  return nfaFreshState'

convertRegexpToNFA :: Regexp -> RegexpToNFA NFA
convertRegexpToNFA Empty = do
  nfaInitialState' <- generateFreshNFAState
  return $ NFA
    { nfaInitialState = nfaInitialState'
    , nfaFinalStates = RBSet.empty
    , nfaTransition = RBMap.empty }
convertRegexpToNFA Epsilon = do
  nfaInitialState' <- generateFreshNFAState
  let nfaFinalStates' = RBSet.singleton nfaInitialState'
  return $ NFA
    { nfaInitialState = nfaInitialState'
    , nfaFinalStates = nfaFinalStates'
    , nfaTransition = RBMap.empty }
convertRegexpToNFA (Ranges ranges) = do
  nfaInitialState' <- generateFreshNFAState
  nfaFinalStates' <- fmap RBSet.singleton generateFreshNFAState
  return $ NFA
    { nfaInitialState = nfaInitialState'
    , nfaFinalStates = nfaFinalStates'
    , nfaTransition =
        RBMap.singleton (nfaInitialState', NFARanges ranges) nfaFinalStates' }
convertRegexpToNFA (regexp1 ::: regexp2) = do
  nfa1 <- convertRegexpToNFA regexp1
  nfa2 <- convertRegexpToNFA regexp2
  return $ NFA
    { nfaInitialState = nfaInitialState nfa1
    , nfaFinalStates = nfaFinalStates nfa2
    , nfaTransition =
        RBMap.unionsBy RBSet.union
          [ nfaTransition nfa1
          , nfaTransition nfa2
          , RBMap.fromList $ map
              (\nfaMiddleState ->
                ( (nfaMiddleState, NFAEpsilon)
                , RBSet.singleton $ nfaInitialState nfa2 ))
              (RBSet.toList (nfaFinalStates nfa1)) ] }
convertRegexpToNFA (regexp1 :|: regexp2) = do
  nfaInitialState' <- generateFreshNFAState
  nfa1 <- convertRegexpToNFA regexp1
  nfa2 <- convertRegexpToNFA regexp2
  return $ NFA
    { nfaInitialState = nfaInitialState'
    , nfaFinalStates = RBSet.union (nfaFinalStates nfa1) (nfaFinalStates nfa2)
    , nfaTransition =
        RBMap.unionsBy RBSet.union
          [ nfaTransition nfa1
          , nfaTransition nfa2
          , RBMap.singleton
              (nfaInitialState', NFAEpsilon)
              (RBSet.fromList [nfaInitialState nfa1, nfaInitialState nfa2]) ] }
convertRegexpToNFA (Many regexp) = do
  nfa <- convertRegexpToNFA regexp
  return $ NFA
    { nfaInitialState = nfaInitialState nfa
    , nfaFinalStates = RBSet.singleton $ nfaInitialState nfa -- $ nfaFinalStates nfa
    , nfaTransition =
        RBMap.unionBy RBSet.union (nfaTransition nfa) $ RBMap.fromList $ map
          (\nfaLoopState ->
            ( (nfaLoopState, NFAEpsilon)
            , RBSet.singleton $ nfaInitialState nfa ))
          (RBSet.toList (nfaFinalStates nfa)) }

runNFAToDFA :: NFAToDFA a -> a
runNFAToDFA =
  flip State.evalState $ NFAToDFAState
    { dfaStates = RBMap.empty
    , dfaFreshState = 1
    , dfaCurrentState = 0
    , dfaTrans = RBMap.empty }

convertNFAToDFA :: NFA -> NFAToDFA DFA
convertNFAToDFA nfa = do
  isEmpty <- State.gets (RBMap.null . dfaStates)
  Monad.when isEmpty $
    State.modify $ \s ->
      s { dfaStates =
            RBMap.fromList
              [ (0, RBSet.empty)
              , (1, closure (RBSet.singleton (nfaInitialState nfa))) ] }
  makeDFA
  states <- State.gets dfaStates
  dfaTransition' <- State.gets dfaTrans
  let dfaFinalStates'' =
        RBSet.fromList $ RBMap.keys $ RBMap.filter
          (RBSet.any (flip RBSet.member (nfaFinalStates nfa)))
          states
  return $ DFA
    { dfaInitialState = 1
    , dfaFinalStates = dfaFinalStates''
    , dfaTransition = dfaTransition' }
  where
    closure states =
      let nfaTransition' =
            flip RBMap.filteri (nfaTransition nfa) $ \(s, c) _ ->
              RBSet.member s states && c == NFAEpsilon in
      let states' = RBSet.unions $ states : RBMap.elts nfaTransition' in
        if states == states' then
          states
        else
          closure states'

    dfaLabel NFAEpsilon =
      Nothing
    dfaLabel (NFARanges ranges) =
      Just $ DFARanges ranges

    splitRange range1 range2 =
      let (c1, c2) = range1 in
      let (c3, c4) = range2 in
        if range1 == range2 then
          []
        else if c1 > c4 || c2 < c3 then
          [range2]
        else if subsetRange range1 range2 then
          if c1 == minBound && c2 == maxBound then
            []
          else if c1 == minBound then
            [(succ c2, c4)]
          else if c2 == maxBound then
            [(c3, pred c1)]
          else
            [(c3, pred c1), (succ c2, c4)]
        else if subsetRange range2 range1 then
          []
        else
          if c1 > c3 then
            [(c3, pred c1)]
          else if c2 < c4 then
            [(succ c2, c4)]
          else
            []

    splitRanges = concatMap . splitRange

    splitRanges' ranges =
      map (\r -> foldr splitRanges r $ concat $ filter (\r'' -> Prelude.not (all (\r' -> Prelude.any (subsetRange r') r'') r)) ranges) ranges

    dfaRanges =
      RBSet.toList $ RBSet.fromList $ map (filter (Prelude.not . nullRange)) $ splitRanges' $ [(minBound, maxBound)] : (Maybe.mapMaybe (\(_, c) ->
        case c of
          NFAEpsilon -> Nothing
          NFARanges ranges -> Just ranges
          ) $
        RBMap.keys $ nfaTransition nfa)

    dfaLabels =
      map DFARanges dfaRanges

    subset (DFARanges ranges1) (DFARanges ranges2) =
      all (\r -> Prelude.any (subsetRange r) ranges2) ranges1

    makeDFA = do
      continue <- State.gets (\(NFAToDFAState _ p j _) -> j <= p)
      if Prelude.not continue then
        return ()
      else do
        Monad.forM_ dfaLabels $ \c -> do
          e <-
            State.gets $ \(NFAToDFAState states _ j _) ->
              case RBMap.lookup j states of
                Nothing ->
                  undefined
                Just states' ->
                  closure $ RBSet.unions $ RBMap.elts $
                    flip RBMap.filteri (nfaTransition nfa) $ \(s, c') _ ->
                      case dfaLabel c' of
                        Nothing ->
                          False
                        Just c'' ->
                          RBSet.member s states' && subset c c''
          maybeI <-
            State.gets $ \(NFAToDFAState states _ _ _) ->
              fmap fst $ List.find (\(_, states') -> states' == e) $
                RBMap.toList states
          case maybeI of
            Nothing ->
              State.modify $ \(NFAToDFAState states p j trans) ->
                let p' = p + 1 in
                let states' = RBMap.insert p' e states in
                let trans' = RBMap.insert (j, c) p' trans in
                  (NFAToDFAState states' p' j trans')
            Just i ->
              State.modify $ \(NFAToDFAState states p j trans) ->
                let trans' = RBMap.insert (j, c) i trans in
                  (NFAToDFAState states p j trans')
        State.modify $ \(NFAToDFAState states p j trans) ->
          let j' = j + 1 in
            (NFAToDFAState states p j' trans)
        makeDFA

convertRegexpToDFA :: Regexp -> DFA
convertRegexpToDFA = runNFAToDFA . convertNFAToDFA . runRegexpToNFA . convertRegexpToNFA

fillTable :: DFA -> RBSet.RBSet (DFAState, DFAState)
fillTable dfa = fillTable' RBSet.empty initialTable where
  states =
    RBSet.insert (dfaInitialState dfa) $
      RBSet.union (dfaFinalStates dfa) $
      RBSet.fromList $ concatMap (\((s, _), s') -> [s, s']) $
        RBMap.toList $ dfaTransition dfa

  initialTable =
    RBSet.fromList
      (concatMap
        (\p ->
          concatMap (\q -> [(min p q, max p q)]) (RBSet.toList (dfaFinalStates dfa)))
        (RBSet.toList (RBSet.diff states (dfaFinalStates dfa))))

  m = RBMap.fromList $
    map (\l -> (fst (head l), RBSet.toList $ RBSet.fromList $ map snd l)) $
    List.groupBy (\lhs rhs -> fst lhs == fst rhs) $
    List.sortBy (\lhs rhs -> compare (fst lhs) (fst rhs)) $
    map (\((p, c), q) -> (q, (p, c))) $ RBMap.toList $ dfaTransition dfa

  fillTable' table table' =
    let table'' =
          RBSet.fromList $
            concatMap
              (\(p, q) ->
                case RBMap.lookup p m of
                  Nothing ->
                    []
                  Just l ->
                    case RBMap.lookup q m of
                      Nothing ->
                        []
                      Just l' ->
                        concatMap (\(p', c) -> Maybe.mapMaybe (\(q', c') -> if p' /= q' && c == c' then Just (min p' q', max p' q') else Nothing) l) l')
              (RBSet.toList table') in
      if RBSet.union table table' == RBSet.unions [table, table', table''] then
        RBSet.union table table'
      else
        fillTable' (RBSet.union table table') table''

minimize :: DFA -> DFA
minimize dfa = DFA
  { dfaTransition = RBMap.fromList $ map (\((p, c), q) -> ((mapState p, c), mapState q)) $ RBMap.toList $ dfaTransition dfa
  , dfaInitialState = mapState $ dfaInitialState dfa
  , dfaFinalStates = RBSet.fromList $ map mapState $ RBSet.toList $ dfaFinalStates dfa } where
    states =
      RBSet.insert (dfaInitialState dfa) $
        RBSet.union (dfaFinalStates dfa) $
        RBSet.fromList $ concatMap (\((s, _), s') -> [s, s']) $
          RBMap.toList $ dfaTransition dfa

    table = fillTable dfa

    p /== q =
      RBSet.member (min p q, max p q) table

    p === q =
      Prelude.not $ p /== q

    eqStates =
      RBSet.fromList $ map RBSet.fromList $ foldr
        (\q s ->
          let s' =
                map
                  (\qs ->
                    if Maybe.isJust (List.find (\q' -> q === q') qs) then
                      q : qs
                    else
                      qs)
                  s in
            if s == s' then
              [q] : s
            else
              s')
        []
        (RBSet.toList states)

    mapState q =
      case List.find (\(_, qs) -> RBSet.member q qs) $ zip [0..] $ RBSet.toList eqStates of
        Nothing ->
          undefined
        Just (i, _) ->
          i

generateDFATransition ::
  String -> DFATransition -> CodeGenerating.CodeGenerating Char ()
generateDFATransition prefix dfaTransition' =
  let rangess =
        List.nub $
          map (\((_, DFARanges ranges), _) -> ranges) $
          RBMap.toList dfaTransition' in
  let indices = RBMap.fromList $ zip rangess [0 :: Int ..] in
  let isSingleton [(c1, c2)] = c1 == c2
      isSingleton _ = False in
  let singletons = filter (isSingleton . fst) (RBMap.toList indices) in
  let notSingletons =
        filter (Prelude.not . isSingleton . fst) (RBMap.toList indices) in do
    CodeGenerating.write prefix
    CodeGenerating.write "Transition :: Int -> Char -> Int\n"
    CodeGenerating.write prefix
    CodeGenerating.write "Transition q c =\n"
    CodeGenerating.write "  let c' :: Int\n"
    CodeGenerating.write "      c' =\n"
    CodeGenerating.write "        case Char.ord c of\n"
    Monad.forM_ singletons $ \(ranges, i) ->
      case ranges of
        [singleton] ->
          let (c, _) = singleton in do
            CodeGenerating.write "          "
            CodeGenerating.write $ show $ Char.ord c
            CodeGenerating.write " -> "
            CodeGenerating.write $ show i
            CodeGenerating.write "\n"
        _ ->
          return ()
    CodeGenerating.write "          c'' ->\n"
    CodeGenerating.write "            "
    Monad.forM_ (filter ((/= 0) . snd) notSingletons) $ \(ranges, i) ->
      case ranges of
        [r] ->
          let (c1, c2) = r in do
            CodeGenerating.write "if "
            CodeGenerating.write $ show $ Char.ord c1
            CodeGenerating.write " <= c'' && c'' <= "
            CodeGenerating.write $ show $ Char.ord c2
            CodeGenerating.write " then "
            CodeGenerating.write $ show i
            CodeGenerating.write "\n"
            CodeGenerating.write "            else "
        _ -> do
          CodeGenerating.write "if any (\\(c1, c2) -> c1 <= c'' && c'' <= c2) "
          CodeGenerating.write $ show $
            map (\(c1, c2) -> (Char.ord c1, Char.ord c2)) ranges
          CodeGenerating.write " then "
          CodeGenerating.write $ show i
          CodeGenerating.write "\n"
          CodeGenerating.write "            else "
    CodeGenerating.write "0 in\n"
    CodeGenerating.write "    case (q, c') of\n"
    Monad.forM_ (filter ((/= 0) . snd) (RBMap.toList dfaTransition')) $ \((p, DFARanges ranges), q) -> do
      CodeGenerating.write "      ("
      CodeGenerating.write $ show p
      CodeGenerating.write ", "
      CodeGenerating.write $ maybe "-1" show $ RBMap.lookup ranges indices
      CodeGenerating.write ") -> "
      CodeGenerating.write $ show q
      CodeGenerating.write "\n"
    CodeGenerating.write "      _ -> 0\n"

generateDFA :: String -> DFA -> CodeGenerating.CodeGenerating Char ()
generateDFA prefix dfa = do
  CodeGenerating.write prefix
  CodeGenerating.write "InitialState :: Int\n"
  CodeGenerating.write prefix
  CodeGenerating.write "InitialState = "
  CodeGenerating.write $ show $ dfaInitialState dfa
  CodeGenerating.write "\n"
  CodeGenerating.write "\n"
  CodeGenerating.write prefix
  CodeGenerating.write "FinalStates :: [Int]\n"
  CodeGenerating.write prefix
  CodeGenerating.write "FinalStates = "
  CodeGenerating.write $ show $ RBSet.toList $ dfaFinalStates dfa
  CodeGenerating.write "\n"
  CodeGenerating.write "\n"
  generateDFATransition prefix $ dfaTransition dfa

generateLexer :: String -> [(String, Regexp, String)] -> String
generateLexer modid rules = CodeGenerating.generate $
  let states = List.nub $ "Initial" : map (\(state, _, _) -> state) rules in
  let actions = List.nub $ map (\(_, _, action) -> action) rules in do
    CodeGenerating.write "module "
    CodeGenerating.write modid
    CodeGenerating.write " (Lexing, LexingState(..), SemanticActions(..), runLexing, lex, yybegin) where\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "import           Prelude\n"
    CodeGenerating.write "  hiding (lex)\n"
    CodeGenerating.write "import qualified Control.Applicative as Applicative\n"
    CodeGenerating.write "import qualified Control.Monad       as Monad\n"
    CodeGenerating.write "import qualified Control.Monad.Trans as MonadTrans\n"
    CodeGenerating.write "import qualified Data.Char           as Char\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "data LexingState ="
    Monad.forM_ (zip [0 :: Int ..] states) $ \(i, state) -> do
      if i == 0 then
        CodeGenerating.write "\n    "
      else
        CodeGenerating.write "\n  | "
      CodeGenerating.write state
    CodeGenerating.write "\n  deriving (Eq, Ord, Read, Show)\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "data SemanticActions m a = SemanticActions"
    Monad.forM_ (zip [0 :: Int ..] actions) $ \(i, action) -> do
      if i == 0 then
        CodeGenerating.write "\n  { "
      else
        CodeGenerating.write "\n  , "
      CodeGenerating.write action
      CodeGenerating.write " :: String -> Lexing m a"
    CodeGenerating.write " }\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "instance Monad m => Functor (Lexing m) where\n"
    CodeGenerating.write "  fmap = Monad.liftM\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "instance Monad m => Applicative.Applicative (Lexing m) where\n"
    CodeGenerating.write "  pure = return\n"
    CodeGenerating.write "  (<*>) = Monad.ap\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "instance Monad m => Monad (Lexing m) where\n"
    CodeGenerating.write "  return x = Lexing $ \\s -> return (x, s)\n"
    CodeGenerating.write "  Lexing f >>= k = Lexing $ \\s -> do\n"
    CodeGenerating.write "    (x, s') <- f s\n"
    CodeGenerating.write "    unLexing (k x) s'\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "instance MonadTrans.MonadTrans Lexing where\n"
    CodeGenerating.write "  lift m = Lexing $ \\s -> do { x <- m; return (x, s) }\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "runLexing :: Monad m => Lexing m a -> m a\n"
    CodeGenerating.write "runLexing = Monad.liftM fst . flip unLexing Initial\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "yybegin :: Monad m => LexingState -> Lexing m ()\n"
    CodeGenerating.write "yybegin s = Lexing $ const $ return ((), s)\n"
    CodeGenerating.write "\n"
    Monad.forM_ states $ \state ->
      let regexp =
            foldr (:|:) Empty $
              map (\(_, regexp', _) -> regexp') $
              filter (\(state', _, _) -> state' == state) rules in
      let dfa = minimize $ convertRegexpToDFA regexp in do
        generateDFA ("dfa" ++ state) dfa
        CodeGenerating.write "\n"
    Monad.forM_ (zip [0 :: Int ..] rules) $ \(i, (_, regexp, _)) ->
      let dfa = minimize $ convertRegexpToDFA regexp in do
        generateDFA ("dfa" ++ show i) dfa
        CodeGenerating.write "\n"
    CodeGenerating.write "match :: Int -> [Int] -> (Int -> Char -> Int) -> String -> Maybe Int\n"
    CodeGenerating.write "match initialState finalStates transition = match' 0 Nothing initialState\n"
    CodeGenerating.write "  where\n"
    CodeGenerating.write "    match' :: Int -> Maybe Int -> Int -> String -> Maybe Int\n"
    CodeGenerating.write "    match' i r q s = \n"
    CodeGenerating.write "      let r' =\n"
    CodeGenerating.write "            if q `elem` finalStates then\n"
    CodeGenerating.write "              Just i\n"
    CodeGenerating.write "            else\n"
    CodeGenerating.write "              r in\n"
    CodeGenerating.write "        case s of\n"
    CodeGenerating.write "          [] ->\n"
    CodeGenerating.write "            r'\n"
    CodeGenerating.write "          (c : s') ->\n"
    CodeGenerating.write "            let q' = transition q c in\n"
    CodeGenerating.write "              match' (i + 1) r' q' s'\n"
    CodeGenerating.write "\n"
    CodeGenerating.write "lex :: Monad m => SemanticActions m a -> String -> Lexing m ([a], String)\n"
    CodeGenerating.write "lex actions = lex' where\n"
    CodeGenerating.write "  lex' s = do\n"
    CodeGenerating.write "    p <- Lexing $ \\p -> return (p, p)\n"
    CodeGenerating.write "   "
    Monad.forM_ states $ \state -> do
      CodeGenerating.write " if p == "
      CodeGenerating.write state
      CodeGenerating.write " then\n"
      CodeGenerating.write "      case match dfa"
      CodeGenerating.write state
      CodeGenerating.write "InitialState dfa"
      CodeGenerating.write state
      CodeGenerating.write "FinalStates dfa"
      CodeGenerating.write state
      CodeGenerating.write "Transition s of\n"
      CodeGenerating.write "        Nothing ->\n"
      CodeGenerating.write "          return ([], s)\n"
      CodeGenerating.write "        Just 0 ->\n"
      CodeGenerating.write "          return ([], s)\n"
      CodeGenerating.write "        Just i ->\n"
      let rules' = filter (\(_, (state', _, _)) -> state' == state) $ zip [0 :: Int ..] rules
      let actions' = map (\(i, (_, _, action)) -> (i, action)) rules'
      CodeGenerating.write "          let (yytext, s') = splitAt i s in\n"
      CodeGenerating.write "           "
      Monad.forM_  actions' $ \(i, action) -> do
        CodeGenerating.write " if match dfa"
        CodeGenerating.write $ show i
        CodeGenerating.write "InitialState dfa"
        CodeGenerating.write $ show i
        CodeGenerating.write "FinalStates dfa"
        CodeGenerating.write $ show i
        CodeGenerating.write "Transition s == Just i then do\n"
        CodeGenerating.write "              x <- "
        CodeGenerating.write action
        CodeGenerating.write " actions yytext\n"
        CodeGenerating.write "              (xs, s'') <- lex' s'\n"
        CodeGenerating.write "              return (x : xs, s'')\n"
        CodeGenerating.write "            else"
      CodeGenerating.write "\n"
      CodeGenerating.write "              return ([], s)\n"
      CodeGenerating.write "    else"
    CodeGenerating.write "\n      return ([], s)\n"

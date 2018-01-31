module  Lexing  where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char


import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as MonadTrans
import qualified Parsing

newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }

data LexingState =
    Initial
  deriving (Eq, Ord, Read, Show)

data SemanticActions m a = SemanticActions
  { saLambda :: String -> Lexing m a
  , saDot :: String -> Lexing m a
  , saLParen :: String -> Lexing m a
  , saRParen :: String -> Lexing m a
  , saId :: String -> Lexing m a
  , saSpace :: String -> Lexing m a }

instance Monad m => Functor (Lexing m) where
  fmap = Monad.liftM

instance Monad m => Applicative.Applicative (Lexing m) where
  pure = return
  (<*>) = Monad.ap

instance Monad m => Monad (Lexing m) where
  return x = Lexing $ \s -> return (x, s)
  Lexing f >>= k = Lexing $ \s -> do
    (x, s') <- f s
    unLexing (k x) s'

instance MonadTrans.MonadTrans Lexing where
  lift m = Lexing $ \s -> do { x <- m; return (x, s) }

runLexing :: Monad m => Lexing m a -> m a
runLexing = Monad.liftM fst . flip unLexing Initial

yybegin :: Monad m => LexingState -> Lexing m ()
yybegin s = Lexing $ const $ return ((), s)

dfaInitialInitialState :: Int
dfaInitialInitialState = 1

dfaInitialFinalStates :: [Int]
dfaInitialFinalStates = [2,3]

dfaInitialTransition :: Int -> Char -> Int
dfaInitialTransition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          40 -> 2
          41 -> 3
          46 -> 4
          95 -> 7
          955 -> 9
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(9,13),(32,32)] then 1
            else if 48 <= c'' && c'' <= 57 then 5
            else if 65 <= c'' && c'' <= 90 then 6
            else if 97 <= c'' && c'' <= 122 then 8
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 3) -> 2
      (1, 4) -> 2
      (1, 6) -> 3
      (1, 7) -> 3
      (1, 8) -> 3
      (1, 9) -> 2
      (3, 5) -> 3
      (3, 6) -> 3
      (3, 7) -> 3
      (3, 8) -> 3
      _ -> 0

dfa0InitialState :: Int
dfa0InitialState = 1

dfa0FinalStates :: [Int]
dfa0FinalStates = [2]

dfa0Transition :: Int -> Char -> Int
dfa0Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          955 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa1InitialState :: Int
dfa1InitialState = 1

dfa1FinalStates :: [Int]
dfa1FinalStates = [2]

dfa1Transition :: Int -> Char -> Int
dfa1Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          46 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa2InitialState :: Int
dfa2InitialState = 1

dfa2FinalStates :: [Int]
dfa2FinalStates = [2]

dfa2Transition :: Int -> Char -> Int
dfa2Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          40 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa3InitialState :: Int
dfa3InitialState = 1

dfa3FinalStates :: [Int]
dfa3FinalStates = [2]

dfa3Transition :: Int -> Char -> Int
dfa3Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          41 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa4InitialState :: Int
dfa4InitialState = 1

dfa4FinalStates :: [Int]
dfa4FinalStates = [2]

dfa4Transition :: Int -> Char -> Int
dfa4Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          95 -> 3
          c'' ->
            if 48 <= c'' && c'' <= 57 then 1
            else if 65 <= c'' && c'' <= 90 then 2
            else if 97 <= c'' && c'' <= 122 then 4
            else 0 in
    case (q, c') of
      (1, 2) -> 2
      (1, 3) -> 2
      (1, 4) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      _ -> 0

dfa5InitialState :: Int
dfa5InitialState = 1

dfa5FinalStates :: [Int]
dfa5FinalStates = [2]

dfa5Transition :: Int -> Char -> Int
dfa5Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(9,13),(32,32)] then 1
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

match :: Int -> [Int] -> (Int -> Char -> Int) -> String -> Maybe Int
match initialState finalStates transition = match' 0 Nothing initialState
  where
    match' :: Int -> Maybe Int -> Int -> String -> Maybe Int
    match' i r q s =
      let r' =
            if q `elem` finalStates then
              Just i
            else
              r in
        case s of
          [] ->
            r'
          (c : s') ->
            let q' = transition q c in
              match' (i + 1) r' q' s'

lex :: Monad m => SemanticActions m a -> String -> Lexing m ([a], String)
lex actions = lex' where
  lex' s = do
    p <- Lexing $ \p -> return (p, p)
    if p == Initial then
      case match dfaInitialInitialState dfaInitialFinalStates dfaInitialTransition s of
        Nothing ->
          return ([], s)
        Just 0 ->
          return ([], s)
        Just i ->
          let (yytext, s') = splitAt i s in
            if match dfa0InitialState dfa0FinalStates dfa0Transition s == Just i then do
              x <- saLambda actions yytext
              (xs, s'') <- lex' s'
              return (x : xs, s'')
            else if match dfa1InitialState dfa1FinalStates dfa1Transition s == Just i then do
              x <- saDot actions yytext
              (xs, s'') <- lex' s'
              return (x : xs, s'')
            else if match dfa2InitialState dfa2FinalStates dfa2Transition s == Just i then do
              x <- saLParen actions yytext
              (xs, s'') <- lex' s'
              return (x : xs, s'')
            else if match dfa3InitialState dfa3FinalStates dfa3Transition s == Just i then do
              x <- saRParen actions yytext
              (xs, s'') <- lex' s'
              return (x : xs, s'')
            else if match dfa4InitialState dfa4FinalStates dfa4Transition s == Just i then do
              x <- saId actions yytext
              (xs, s'') <- lex' s'
              return (x : xs, s'')
            else if match dfa5InitialState dfa5FinalStates dfa5Transition s == Just i then do
              x <- saSpace actions yytext
              (xs, s'') <- lex' s'
              return (x : xs, s'')
            else
              return ([], s)
    else
      return ([], s)



withPosition :: (Int -> String -> Maybe Parsing.Token) -> String -> Lexing (State.State Int) (Maybe Parsing.Token)
withPosition f yytext = do
  let n = length yytext
  pos <- MonadTrans.lift State.get
  MonadTrans.lift $ State.put $ pos + n
  return $ f pos yytext

semanticActions :: SemanticActions (State.State Int) (Maybe Parsing.Token)
semanticActions = SemanticActions
  { saLambda = withPosition $ const . Just . Parsing.LAMBDA
  , saDot = withPosition $ const . Just . Parsing.DOT
  , saLParen = withPosition $ const . Just . Parsing.LPAREN
  , saRParen = withPosition $ const . Just . Parsing.RPAREN
  , saId = withPosition $ curry $ Just . Parsing.ID
  , saSpace = withPosition $ const $ const Nothing }


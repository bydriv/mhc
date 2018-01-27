module MHC.Lexing (Lexing, LexingState(..), SemanticActions(..), runLexing, lex, yybegin) where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char

newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }

data LexingState =
    Initial
  deriving (Eq, Ord, Read, Show)

data SemanticActions m a = SemanticActions
  { space :: String -> Lexing m a }

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
          10 -> 1
          12 -> 2
          13 -> 3
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 3) -> 3
      (3, 1) -> 2
      _ -> 0

dfa0InitialState :: Int
dfa0InitialState = 1

dfa0FinalStates :: [Int]
dfa0FinalStates = [2,3]

dfa0Transition :: Int -> Char -> Int
dfa0Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 1
          12 -> 2
          13 -> 3
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 3) -> 3
      (3, 1) -> 2
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
              x <- space actions yytext
              (xs, s'') <- lex' s'
              return (x : xs, s'')
            else
              return ([], s)
    else
      return ([], s)


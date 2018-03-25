module  Data.JSON.Lexing  where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char


import qualified Data.Char         as Char
import qualified Data.JSON.Parsing as Parsing

newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }

data LexingState =
    Initial
  deriving (Eq, Ord, Read, Show)

data SemanticActions m a = SemanticActions
  { saWS :: String -> Lexing m a
  , saFalse :: String -> Lexing m a
  , saNull :: String -> Lexing m a
  , saTrue :: String -> Lexing m a
  , saLBrace :: String -> Lexing m a
  , saRBrace :: String -> Lexing m a
  , saComma :: String -> Lexing m a
  , saColon :: String -> Lexing m a
  , saLBracket :: String -> Lexing m a
  , saRBracket :: String -> Lexing m a
  , saNumber :: String -> Lexing m a
  , saString :: String -> Lexing m a }

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

dfa0InitialState :: Int
dfa0InitialState = 1

dfa0FinalStates :: [Int]
dfa0FinalStates = [2]

dfa0Transition :: Int -> Char -> Int
dfa0Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(9,10),(12,13),(32,32)] then 1
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa1InitialState :: Int
dfa1InitialState = 1

dfa1FinalStates :: [Int]
dfa1FinalStates = [6]

dfa1Transition :: Int -> Char -> Int
dfa1Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          97 -> 1
          101 -> 2
          102 -> 3
          108 -> 4
          115 -> 5
          _ ->
            0 in
    case (q, c') of
      (1, 3) -> 2
      (2, 1) -> 3
      (3, 4) -> 4
      (4, 5) -> 5
      (5, 2) -> 6
      _ -> 0

dfa2InitialState :: Int
dfa2InitialState = 1

dfa2FinalStates :: [Int]
dfa2FinalStates = [5]

dfa2Transition :: Int -> Char -> Int
dfa2Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          108 -> 1
          110 -> 2
          117 -> 3
          _ ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 3) -> 3
      (3, 1) -> 4
      (4, 1) -> 5
      _ -> 0

dfa3InitialState :: Int
dfa3InitialState = 1

dfa3FinalStates :: [Int]
dfa3FinalStates = [5]

dfa3Transition :: Int -> Char -> Int
dfa3Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          114 -> 2
          116 -> 3
          117 -> 4
          _ ->
            0 in
    case (q, c') of
      (1, 3) -> 2
      (2, 2) -> 3
      (3, 4) -> 4
      (4, 1) -> 5
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
          123 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
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
          125 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa6InitialState :: Int
dfa6InitialState = 1

dfa6FinalStates :: [Int]
dfa6FinalStates = [2]

dfa6Transition :: Int -> Char -> Int
dfa6Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          44 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa7InitialState :: Int
dfa7InitialState = 1

dfa7FinalStates :: [Int]
dfa7FinalStates = [2]

dfa7Transition :: Int -> Char -> Int
dfa7Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          58 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa8InitialState :: Int
dfa8InitialState = 1

dfa8FinalStates :: [Int]
dfa8FinalStates = [2]

dfa8Transition :: Int -> Char -> Int
dfa8Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          91 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa9InitialState :: Int
dfa9InitialState = 1

dfa9FinalStates :: [Int]
dfa9FinalStates = [2]

dfa9Transition :: Int -> Char -> Int
dfa9Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          93 -> 1
          _ ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa10InitialState :: Int
dfa10InitialState = 1

dfa10FinalStates :: [Int]
dfa10FinalStates = [2,4,5,10,11,12]

dfa10Transition :: Int -> Char -> Int
dfa10Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          43 -> 2
          45 -> 3
          46 -> 4
          48 -> 5
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,42),(44,44),(47,47),(58,68),(70,100),(102,1114111)] then 1
            else if 49 <= c'' && c'' <= 57 then 6
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(69,69),(101,101)] then 7
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 3) -> 3
      (1, 5) -> 4
      (1, 6) -> 2
      (2, 0) -> 5
      (2, 4) -> 6
      (2, 5) -> 2
      (2, 6) -> 2
      (2, 7) -> 7
      (3, 0) -> 2
      (3, 5) -> 4
      (3, 6) -> 2
      (4, 0) -> 8
      (4, 4) -> 6
      (4, 7) -> 7
      (5, 0) -> 5
      (5, 2) -> 9
      (5, 3) -> 9
      (5, 4) -> 6
      (5, 5) -> 2
      (5, 6) -> 2
      (5, 7) -> 7
      (6, 0) -> 10
      (6, 5) -> 10
      (6, 6) -> 10
      (7, 0) -> 11
      (7, 2) -> 9
      (7, 3) -> 9
      (7, 5) -> 11
      (7, 6) -> 11
      (8, 0) -> 10
      (8, 2) -> 9
      (8, 3) -> 9
      (8, 5) -> 10
      (8, 6) -> 10
      (9, 0) -> 11
      (9, 5) -> 11
      (9, 6) -> 11
      (10, 0) -> 12
      (10, 5) -> 10
      (10, 6) -> 10
      (10, 7) -> 7
      (11, 0) -> 11
      (11, 5) -> 11
      (11, 6) -> 11
      (12, 0) -> 12
      (12, 2) -> 9
      (12, 3) -> 9
      (12, 5) -> 10
      (12, 6) -> 10
      (12, 7) -> 7
      _ -> 0

dfa11InitialState :: Int
dfa11InitialState = 1

dfa11FinalStates :: [Int]
dfa11FinalStates = [3,4,6]

dfa11Transition :: Int -> Char -> Int
dfa11Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          34 -> 2
          92 -> 6
          117 -> 9
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,33),(35,46),(58,64),(71,91),(93,96),(103,109),(111,113),(115,115),(118,1114111)] then 1
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(47,47),(110,110),(114,114),(116,116)] then 3
            else if 48 <= c'' && c'' <= 57 then 4
            else if 65 <= c'' && c'' <= 70 then 5
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,97),(99,101)] then 7
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(98,98),(102,102)] then 8
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 2) -> 2
      (2, 0) -> 3
      (2, 1) -> 2
      (2, 2) -> 4
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 5
      (2, 7) -> 2
      (2, 8) -> 2
      (2, 9) -> 2
      (3, 0) -> 3
      (3, 1) -> 2
      (3, 2) -> 6
      (3, 3) -> 2
      (3, 4) -> 2
      (3, 5) -> 2
      (3, 6) -> 7
      (3, 7) -> 2
      (3, 8) -> 2
      (3, 9) -> 2
      (5, 0) -> 2
      (5, 2) -> 2
      (5, 3) -> 2
      (5, 6) -> 2
      (5, 8) -> 2
      (5, 9) -> 8
      (6, 0) -> 3
      (6, 1) -> 2
      (6, 2) -> 4
      (6, 3) -> 2
      (6, 4) -> 2
      (6, 5) -> 2
      (6, 6) -> 5
      (6, 7) -> 2
      (6, 8) -> 2
      (6, 9) -> 2
      (7, 0) -> 3
      (7, 1) -> 2
      (7, 2) -> 6
      (7, 3) -> 2
      (7, 4) -> 2
      (7, 5) -> 2
      (7, 6) -> 7
      (7, 7) -> 2
      (7, 8) -> 2
      (7, 9) -> 2
      (8, 0) -> 9
      (8, 4) -> 9
      (8, 5) -> 9
      (8, 7) -> 9
      (8, 8) -> 9
      (9, 0) -> 10
      (9, 4) -> 10
      (9, 5) -> 10
      (9, 7) -> 10
      (9, 8) -> 10
      (10, 0) -> 11
      (10, 4) -> 11
      (10, 5) -> 11
      (10, 7) -> 11
      (10, 8) -> 11
      (11, 0) -> 2
      (11, 4) -> 2
      (11, 5) -> 2
      (11, 7) -> 2
      (11, 8) -> 2
      _ -> 0

match :: Int -> [Int] -> (Int -> Char -> Int) -> String -> Maybe Int
match initialState finalStates transition = match' 0 Nothing initialState
  where
    match' :: Int -> Maybe Int -> Int -> String -> Maybe Int
    match' _ r 0 _ =
      r
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
      case max (match dfa0InitialState dfa0FinalStates dfa0Transition s, -0) $ max (match dfa1InitialState dfa1FinalStates dfa1Transition s, -1) $ max (match dfa2InitialState dfa2FinalStates dfa2Transition s, -2) $ max (match dfa3InitialState dfa3FinalStates dfa3Transition s, -3) $ max (match dfa4InitialState dfa4FinalStates dfa4Transition s, -4) $ max (match dfa5InitialState dfa5FinalStates dfa5Transition s, -5) $ max (match dfa6InitialState dfa6FinalStates dfa6Transition s, -6) $ max (match dfa7InitialState dfa7FinalStates dfa7Transition s, -7) $ max (match dfa8InitialState dfa8FinalStates dfa8Transition s, -8) $ max (match dfa9InitialState dfa9FinalStates dfa9Transition s, -9) $ max (match dfa10InitialState dfa10FinalStates dfa10Transition s, -10) $ max (match dfa11InitialState dfa11FinalStates dfa11Transition s, -11) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -0 -> do
                x <- saWS actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -1 -> do
                x <- saFalse actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -2 -> do
                x <- saNull actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -3 -> do
                x <- saTrue actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -4 -> do
                x <- saLBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -5 -> do
                x <- saRBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -6 -> do
                x <- saComma actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -7 -> do
                x <- saColon actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -8 -> do
                x <- saLBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -9 -> do
                x <- saRBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -10 -> do
                x <- saNumber actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -11 -> do
                x <- saString actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else
      return ([], s)



semanticActions :: Monad m => SemanticActions m (Maybe Parsing.Token)
semanticActions = SemanticActions
  { saWS = const $ return Nothing
  , saFalse = const $ return $ Just $ Parsing.FALSE ()
  , saNull = const $ return $ Just $ Parsing.NULL ()
  , saTrue = const $ return $ Just $ Parsing.TRUE ()
  , saLBrace = const $ return $ Just $ Parsing.LBRACE ()
  , saRBrace = const $ return $ Just $ Parsing.RBRACE ()
  , saComma = const $ return $ Just $ Parsing.COMMA ()
  , saColon = const $ return $ Just $ Parsing.COLON ()
  , saLBracket = const $ return $ Just $ Parsing.LBRACKET ()
  , saRBracket = const $ return $ Just $ Parsing.RBRACKET ()
  , saNumber = return . Just . Parsing.NUMBER . read
  , saString = return . Just . Parsing.STRING . unescape . init . tail }
  where
    unescape [] = []
    unescape ('\\' : '"' : s) =
      '"' : unescape s
    unescape ('\\' : '\\' : s) =
      '\\' : unescape s
    unescape ('\\' : '/' : s) =
      '/' : unescape s
    unescape ('\\' : 'b' : s) =
      '\b' : unescape s
    unescape ('\\' : 'f' : s) =
      '\f' : unescape s
    unescape ('\\' : 'n' : s) =
      '\n' : unescape s
    unescape ('\\' : 'r' : s) =
      '\r' : unescape s
    unescape ('\\' : 't' : s) =
      '\t' : unescape s
    unescape ('\\' : 'u' : x1 : x2 : x3 : x4 : s) =
      Char.chr (read ['0', 'x', x1, x2, x3, x4]) : unescape s
    unescape (c : s) =
      c : unescape s


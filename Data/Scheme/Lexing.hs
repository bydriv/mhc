module  Data.Scheme.Lexing  where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char


import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Scheme.Parsing as Parsing

newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }

data LexingState =
    Initial
  | NestedComment
  deriving (Eq, Ord, Read, Show)

data SemanticActions m a = SemanticActions
  { saWhitespace :: String -> Lexing m a
  , saComment :: String -> Lexing m a
  , saOpenNestedComment :: String -> Lexing m a
  , saCloseNestedComment :: String -> Lexing m a
  , saExprComment :: String -> Lexing m a
  , saEqual :: String -> Lexing m a
  , saSharp :: String -> Lexing m a
  , saLParen :: String -> Lexing m a
  , saRParen :: String -> Lexing m a
  , saDot :: String -> Lexing m a
  , saQuote :: String -> Lexing m a
  , saBackquote :: String -> Lexing m a
  , saComma :: String -> Lexing m a
  , saCommaAt :: String -> Lexing m a
  , saTrue :: String -> Lexing m a
  , saFalse :: String -> Lexing m a
  , saIdentifier :: String -> Lexing m a
  , saNestedComment :: String -> Lexing m a }

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
dfa0FinalStates = [2,3]

dfa0Transition :: Int -> Char -> Int
dfa0Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          9 -> 1
          10 -> 2
          13 -> 3
          32 -> 4
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 3) -> 3
      (1, 4) -> 2
      (3, 2) -> 2
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
          59 -> 2
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(10,10),(13,13)] then 1
            else 0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 0) -> 2
      (2, 2) -> 2
      _ -> 0

dfa2InitialState :: Int
dfa2InitialState = 1

dfa2FinalStates :: [Int]
dfa2FinalStates = [3]

dfa2Transition :: Int -> Char -> Int
dfa2Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          35 -> 1
          124 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa3InitialState :: Int
dfa3InitialState = 1

dfa3FinalStates :: [Int]
dfa3FinalStates = [3]

dfa3Transition :: Int -> Char -> Int
dfa3Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          35 -> 1
          124 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      _ -> 0

dfa4InitialState :: Int
dfa4InitialState = 1

dfa4FinalStates :: [Int]
dfa4FinalStates = [3]

dfa4Transition :: Int -> Char -> Int
dfa4Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          35 -> 1
          59 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          61 -> 1
          c'' ->
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
          35 -> 1
          c'' ->
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
          40 -> 1
          c'' ->
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
          41 -> 1
          c'' ->
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
          46 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa10InitialState :: Int
dfa10InitialState = 1

dfa10FinalStates :: [Int]
dfa10FinalStates = [2]

dfa10Transition :: Int -> Char -> Int
dfa10Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          39 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa11InitialState :: Int
dfa11InitialState = 1

dfa11FinalStates :: [Int]
dfa11FinalStates = [2]

dfa11Transition :: Int -> Char -> Int
dfa11Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          96 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa12InitialState :: Int
dfa12InitialState = 1

dfa12FinalStates :: [Int]
dfa12FinalStates = [2]

dfa12Transition :: Int -> Char -> Int
dfa12Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          44 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa13InitialState :: Int
dfa13InitialState = 1

dfa13FinalStates :: [Int]
dfa13FinalStates = [3]

dfa13Transition :: Int -> Char -> Int
dfa13Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          44 -> 1
          64 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa14InitialState :: Int
dfa14InitialState = 1

dfa14FinalStates :: [Int]
dfa14FinalStates = [3,6]

dfa14Transition :: Int -> Char -> Int
dfa14Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          35 -> 1
          101 -> 2
          114 -> 3
          116 -> 4
          117 -> 5
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 4) -> 3
      (3, 3) -> 4
      (4, 5) -> 5
      (5, 2) -> 6
      _ -> 0

dfa15InitialState :: Int
dfa15InitialState = 1

dfa15FinalStates :: [Int]
dfa15FinalStates = [3,7]

dfa15Transition :: Int -> Char -> Int
dfa15Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          35 -> 1
          97 -> 2
          101 -> 3
          102 -> 4
          108 -> 5
          115 -> 6
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 4) -> 3
      (3, 2) -> 4
      (4, 5) -> 5
      (5, 6) -> 6
      (6, 3) -> 7
      _ -> 0

dfa16InitialState :: Int
dfa16InitialState = 1

dfa16FinalStates :: [Int]
dfa16FinalStates = [2]

dfa16Transition :: Int -> Char -> Int
dfa16Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          33 -> 1
          36 -> 2
          37 -> 3
          38 -> 4
          42 -> 5
          43 -> 6
          45 -> 7
          46 -> 8
          47 -> 9
          58 -> 11
          60 -> 12
          61 -> 13
          62 -> 14
          63 -> 15
          64 -> 16
          94 -> 18
          95 -> 19
          126 -> 21
          c'' ->
            if 48 <= c'' && c'' <= 57 then 10
            else if 65 <= c'' && c'' <= 90 then 17
            else if 97 <= c'' && c'' <= 122 then 20
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 3) -> 2
      (1, 4) -> 2
      (1, 5) -> 2
      (1, 9) -> 2
      (1, 11) -> 2
      (1, 12) -> 2
      (1, 13) -> 2
      (1, 14) -> 2
      (1, 15) -> 2
      (1, 17) -> 2
      (1, 18) -> 2
      (1, 19) -> 2
      (1, 20) -> 2
      (1, 21) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
      (2, 7) -> 2
      (2, 8) -> 2
      (2, 9) -> 2
      (2, 10) -> 2
      (2, 11) -> 2
      (2, 12) -> 2
      (2, 13) -> 2
      (2, 14) -> 2
      (2, 15) -> 2
      (2, 16) -> 2
      (2, 17) -> 2
      (2, 18) -> 2
      (2, 19) -> 2
      (2, 20) -> 2
      (2, 21) -> 2
      _ -> 0

dfa17InitialState :: Int
dfa17InitialState = 1

dfa17FinalStates :: [Int]
dfa17FinalStates = [3]

dfa17Transition :: Int -> Char -> Int
dfa17Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          35 -> 1
          124 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa18InitialState :: Int
dfa18InitialState = 1

dfa18FinalStates :: [Int]
dfa18FinalStates = [3]

dfa18Transition :: Int -> Char -> Int
dfa18Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          35 -> 1
          124 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      _ -> 0

dfa19InitialState :: Int
dfa19InitialState = 1

dfa19FinalStates :: [Int]
dfa19FinalStates = [2]

dfa19Transition :: Int -> Char -> Int
dfa19Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          c'' ->
            0 in
    case (q, c') of
      (1, 0) -> 2
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
      case max (match dfa0InitialState dfa0FinalStates dfa0Transition s, -0) $ max (match dfa1InitialState dfa1FinalStates dfa1Transition s, -1) $ max (match dfa2InitialState dfa2FinalStates dfa2Transition s, -2) $ max (match dfa3InitialState dfa3FinalStates dfa3Transition s, -3) $ max (match dfa4InitialState dfa4FinalStates dfa4Transition s, -4) $ max (match dfa5InitialState dfa5FinalStates dfa5Transition s, -5) $ max (match dfa6InitialState dfa6FinalStates dfa6Transition s, -6) $ max (match dfa7InitialState dfa7FinalStates dfa7Transition s, -7) $ max (match dfa8InitialState dfa8FinalStates dfa8Transition s, -8) $ max (match dfa9InitialState dfa9FinalStates dfa9Transition s, -9) $ max (match dfa10InitialState dfa10FinalStates dfa10Transition s, -10) $ max (match dfa11InitialState dfa11FinalStates dfa11Transition s, -11) $ max (match dfa12InitialState dfa12FinalStates dfa12Transition s, -12) $ max (match dfa13InitialState dfa13FinalStates dfa13Transition s, -13) $ max (match dfa14InitialState dfa14FinalStates dfa14Transition s, -14) $ max (match dfa15InitialState dfa15FinalStates dfa15Transition s, -15) $ max (match dfa16InitialState dfa16FinalStates dfa16Transition s, -16) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -0 -> do
                x <- saWhitespace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -1 -> do
                x <- saComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -2 -> do
                x <- saOpenNestedComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -3 -> do
                x <- saCloseNestedComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -4 -> do
                x <- saExprComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -5 -> do
                x <- saEqual actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -6 -> do
                x <- saSharp actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -7 -> do
                x <- saLParen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -8 -> do
                x <- saRParen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -9 -> do
                x <- saDot actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -10 -> do
                x <- saQuote actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -11 -> do
                x <- saBackquote actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -12 -> do
                x <- saComma actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -13 -> do
                x <- saCommaAt actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -14 -> do
                x <- saTrue actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -15 -> do
                x <- saFalse actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -16 -> do
                x <- saIdentifier actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == NestedComment then
      case max (match dfa17InitialState dfa17FinalStates dfa17Transition s, -17) $ max (match dfa18InitialState dfa18FinalStates dfa18Transition s, -18) $ max (match dfa19InitialState dfa19FinalStates dfa19Transition s, -19) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -17 -> do
                x <- saOpenNestedComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -18 -> do
                x <- saCloseNestedComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -19 -> do
                x <- saNestedComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else
      return ([], s)



semanticActions :: SemanticActions (State.State Int) (Maybe Parsing.Token)
semanticActions = SemanticActions
  { saWhitespace =
      const $ return Nothing
  , saComment =
      const $ return Nothing

  , saOpenNestedComment = const $ do
      MonadTrans.lift $ State.modify succ
      yybegin NestedComment
      return Nothing

  , saCloseNestedComment = const $ do
      i <- MonadTrans.lift State.get

      if i <= 1 then do
        MonadTrans.lift $ State.put 0
        yybegin Initial
      else
        MonadTrans.lift $ State.modify pred

      return Nothing

  , saExprComment =
      const $ return $ Just $ Parsing.SHARP_SEMICOLON ()
  , saEqual =
      const $ return $ Just $ Parsing.EQUAL ()
  , saSharp =
      const $ return $ Just $ Parsing.SHARP ()
  , saLParen =
      const $ return $ Just $ Parsing.LPAREN ()
  , saRParen =
      const $ return $ Just $ Parsing.RPAREN ()
  , saDot =
      const $ return $ Just $ Parsing.DOT ()
  , saQuote =
      const $ return $ Just $ Parsing.QUOTE ()
  , saBackquote =
      const $ return $ Just $ Parsing.BACKQUOTE ()
  , saComma =
      const $ return $ Just $ Parsing.COMMA ()
  , saCommaAt =
      const $ return $ Just $ Parsing.COMMAAT ()
  , saTrue =
      const $ return $ Just $ Parsing.BOOLEAN True
  , saFalse =
      const $ return $ Just $ Parsing.BOOLEAN False
  , saIdentifier =
      return . Just . Parsing.IDENTIFIER
  , saNestedComment =
      const $ return Nothing }


module  Data.TOML.Lexing  where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char


import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char
import qualified Data.TOML.Parsing   as Parsing

newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }

data LexingState =
    Initial
  deriving (Eq, Ord, Read, Show)

data SemanticActions m a = SemanticActions
  { saWs :: String -> Lexing m a
  , saBasicString :: String -> Lexing m a
  , saMLBasicString :: String -> Lexing m a
  , saMLLiteralString :: String -> Lexing m a
  , saBool :: String -> Lexing m a
  , saComma :: String -> Lexing m a
  , saDot :: String -> Lexing m a
  , saEqual :: String -> Lexing m a
  , saFloat :: String -> Lexing m a
  , saInteger :: String -> Lexing m a
  , saLBracket :: String -> Lexing m a
  , saLBrace :: String -> Lexing m a
  , saLiteralString :: String -> Lexing m a
  , saNewline :: String -> Lexing m a
  , saRBracket :: String -> Lexing m a
  , saRBrace :: String -> Lexing m a
  , saUnquotedKey :: String -> Lexing m a }

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
          9 -> 1
          32 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
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
          9 -> 1
          35 -> 3
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(32,34),(36,1114111)] then 2
            else 0 in
    case (q, c') of
      (1, 3) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
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
          34 -> 2
          47 -> 4
          85 -> 7
          92 -> 8
          98 -> 11
          102 -> 12
          110 -> 13
          114 -> 14
          116 -> 15
          117 -> 16
          c'' ->
            if 32 <= c'' && c'' <= 33 then 1
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(35,46),(58,64),(71,84),(86,91)] then 3
            else if 48 <= c'' && c'' <= 57 then 5
            else if 65 <= c'' && c'' <= 70 then 6
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(93,96),(103,109),(111,113),(115,115),(118,126)] then 9
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,97),(99,101)] then 10
            else if 128 <= c'' && c'' <= 1114111 then 17
            else 0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 2
      (2, 2) -> 3
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
      (2, 7) -> 2
      (2, 8) -> 4
      (2, 9) -> 2
      (2, 10) -> 2
      (2, 11) -> 2
      (2, 12) -> 2
      (2, 13) -> 2
      (2, 14) -> 2
      (2, 15) -> 2
      (2, 16) -> 2
      (2, 17) -> 2
      (4, 2) -> 2
      (4, 4) -> 2
      (4, 7) -> 5
      (4, 8) -> 2
      (4, 11) -> 2
      (4, 12) -> 2
      (4, 13) -> 2
      (4, 14) -> 2
      (4, 15) -> 2
      (4, 16) -> 6
      (5, 5) -> 7
      (5, 6) -> 7
      (5, 10) -> 7
      (5, 11) -> 7
      (5, 12) -> 7
      (6, 5) -> 8
      (6, 6) -> 8
      (6, 10) -> 8
      (6, 11) -> 8
      (6, 12) -> 8
      (7, 5) -> 9
      (7, 6) -> 9
      (7, 10) -> 9
      (7, 11) -> 9
      (7, 12) -> 9
      (8, 5) -> 10
      (8, 6) -> 10
      (8, 10) -> 10
      (8, 11) -> 10
      (8, 12) -> 10
      (9, 5) -> 11
      (9, 6) -> 11
      (9, 10) -> 11
      (9, 11) -> 11
      (9, 12) -> 11
      (10, 5) -> 12
      (10, 6) -> 12
      (10, 10) -> 12
      (10, 11) -> 12
      (10, 12) -> 12
      (11, 5) -> 6
      (11, 6) -> 6
      (11, 10) -> 6
      (11, 11) -> 6
      (11, 12) -> 6
      (12, 5) -> 2
      (12, 6) -> 2
      (12, 10) -> 2
      (12, 11) -> 2
      (12, 12) -> 2
      _ -> 0

dfa3InitialState :: Int
dfa3InitialState = 1

dfa3FinalStates :: [Int]
dfa3FinalStates = [11]

dfa3Transition :: Int -> Char -> Int
dfa3Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 1
          13 -> 2
          34 -> 4
          47 -> 5
          85 -> 8
          92 -> 9
          98 -> 12
          102 -> 13
          110 -> 14
          114 -> 15
          116 -> 16
          117 -> 17
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(32,33),(35,46),(58,64),(71,84),(86,91)] then 3
            else if 48 <= c'' && c'' <= 57 then 6
            else if 65 <= c'' && c'' <= 70 then 7
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(93,96),(103,109),(111,113),(115,115),(118,126)] then 10
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,97),(99,101)] then 11
            else if 128 <= c'' && c'' <= 1114111 then 18
            else 0 in
    case (q, c') of
      (1, 4) -> 2
      (2, 4) -> 3
      (3, 4) -> 4
      (4, 1) -> 4
      (4, 2) -> 5
      (4, 3) -> 4
      (4, 4) -> 6
      (4, 5) -> 4
      (4, 6) -> 4
      (4, 7) -> 4
      (4, 8) -> 4
      (4, 9) -> 7
      (4, 10) -> 4
      (4, 11) -> 4
      (4, 12) -> 4
      (4, 13) -> 4
      (4, 14) -> 4
      (4, 15) -> 4
      (4, 16) -> 4
      (4, 17) -> 4
      (4, 18) -> 4
      (5, 1) -> 4
      (6, 1) -> 4
      (6, 2) -> 5
      (6, 3) -> 4
      (6, 4) -> 8
      (6, 5) -> 4
      (6, 6) -> 4
      (6, 7) -> 4
      (6, 8) -> 4
      (6, 9) -> 7
      (6, 10) -> 4
      (6, 11) -> 4
      (6, 12) -> 4
      (6, 13) -> 4
      (6, 14) -> 4
      (6, 15) -> 4
      (6, 16) -> 4
      (6, 17) -> 4
      (6, 18) -> 4
      (7, 1) -> 4
      (7, 2) -> 5
      (7, 4) -> 4
      (7, 5) -> 4
      (7, 8) -> 9
      (7, 9) -> 4
      (7, 12) -> 4
      (7, 13) -> 4
      (7, 14) -> 4
      (7, 15) -> 4
      (7, 16) -> 4
      (7, 17) -> 10
      (8, 1) -> 4
      (8, 2) -> 5
      (8, 3) -> 4
      (8, 4) -> 11
      (8, 5) -> 4
      (8, 6) -> 4
      (8, 7) -> 4
      (8, 8) -> 4
      (8, 9) -> 7
      (8, 10) -> 4
      (8, 11) -> 4
      (8, 12) -> 4
      (8, 13) -> 4
      (8, 14) -> 4
      (8, 15) -> 4
      (8, 16) -> 4
      (8, 17) -> 4
      (8, 18) -> 4
      (9, 6) -> 12
      (9, 7) -> 12
      (9, 11) -> 12
      (9, 12) -> 12
      (9, 13) -> 12
      (10, 6) -> 13
      (10, 7) -> 13
      (10, 11) -> 13
      (10, 12) -> 13
      (10, 13) -> 13
      (11, 1) -> 4
      (11, 2) -> 5
      (11, 3) -> 4
      (11, 4) -> 11
      (11, 5) -> 4
      (11, 6) -> 4
      (11, 7) -> 4
      (11, 8) -> 4
      (11, 9) -> 7
      (11, 10) -> 4
      (11, 11) -> 4
      (11, 12) -> 4
      (11, 13) -> 4
      (11, 14) -> 4
      (11, 15) -> 4
      (11, 16) -> 4
      (11, 17) -> 4
      (11, 18) -> 4
      (12, 6) -> 14
      (12, 7) -> 14
      (12, 11) -> 14
      (12, 12) -> 14
      (12, 13) -> 14
      (13, 6) -> 15
      (13, 7) -> 15
      (13, 11) -> 15
      (13, 12) -> 15
      (13, 13) -> 15
      (14, 6) -> 16
      (14, 7) -> 16
      (14, 11) -> 16
      (14, 12) -> 16
      (14, 13) -> 16
      (15, 6) -> 17
      (15, 7) -> 17
      (15, 11) -> 17
      (15, 12) -> 17
      (15, 13) -> 17
      (16, 6) -> 10
      (16, 7) -> 10
      (16, 11) -> 10
      (16, 12) -> 10
      (16, 13) -> 10
      (17, 6) -> 4
      (17, 7) -> 4
      (17, 11) -> 4
      (17, 12) -> 4
      (17, 13) -> 4
      _ -> 0

dfa4InitialState :: Int
dfa4InitialState = 1

dfa4FinalStates :: [Int]
dfa4FinalStates = [8]

dfa4Transition :: Int -> Char -> Int
dfa4Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          9 -> 1
          10 -> 2
          13 -> 3
          39 -> 5
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(32,38),(40,1114111)] then 4
            else 0 in
    case (q, c') of
      (1, 5) -> 2
      (2, 5) -> 3
      (3, 5) -> 4
      (4, 1) -> 4
      (4, 2) -> 4
      (4, 3) -> 5
      (4, 4) -> 4
      (4, 5) -> 6
      (5, 2) -> 4
      (6, 1) -> 4
      (6, 2) -> 4
      (6, 3) -> 5
      (6, 4) -> 4
      (6, 5) -> 7
      (7, 1) -> 4
      (7, 2) -> 4
      (7, 3) -> 5
      (7, 4) -> 4
      (7, 5) -> 8
      (8, 1) -> 4
      (8, 2) -> 4
      (8, 3) -> 5
      (8, 4) -> 4
      (8, 5) -> 8
      _ -> 0

dfa5InitialState :: Int
dfa5InitialState = 1

dfa5FinalStates :: [Int]
dfa5FinalStates = [8]

dfa5Transition :: Int -> Char -> Int
dfa5Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          97 -> 1
          101 -> 2
          102 -> 3
          108 -> 4
          114 -> 5
          115 -> 6
          116 -> 7
          117 -> 8
          c'' ->
            0 in
    case (q, c') of
      (1, 3) -> 2
      (1, 7) -> 3
      (2, 1) -> 4
      (3, 5) -> 5
      (4, 4) -> 6
      (5, 8) -> 7
      (6, 6) -> 7
      (7, 2) -> 8
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
          46 -> 1
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
          61 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa9InitialState :: Int
dfa9InitialState = 1

dfa9FinalStates :: [Int]
dfa9FinalStates = [13,15,16]

dfa9Transition :: Int -> Char -> Int
dfa9Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          43 -> 1
          45 -> 2
          46 -> 3
          48 -> 4
          95 -> 6
          97 -> 7
          101 -> 8
          102 -> 9
          105 -> 10
          110 -> 11
          c'' ->
            if 49 <= c'' && c'' <= 57 then 5
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 4) -> 3
      (1, 5) -> 4
      (1, 10) -> 5
      (1, 11) -> 6
      (2, 4) -> 3
      (2, 5) -> 4
      (2, 10) -> 5
      (2, 11) -> 6
      (3, 3) -> 7
      (3, 8) -> 8
      (4, 3) -> 7
      (4, 4) -> 4
      (4, 5) -> 4
      (4, 6) -> 9
      (4, 8) -> 8
      (5, 11) -> 10
      (6, 7) -> 11
      (7, 1) -> 12
      (7, 2) -> 12
      (7, 4) -> 13
      (7, 5) -> 13
      (8, 1) -> 14
      (8, 2) -> 14
      (8, 4) -> 15
      (8, 5) -> 16
      (9, 4) -> 4
      (9, 5) -> 4
      (10, 9) -> 15
      (11, 11) -> 15
      (12, 4) -> 13
      (12, 5) -> 13
      (13, 4) -> 13
      (13, 5) -> 13
      (13, 6) -> 12
      (13, 8) -> 8
      (14, 4) -> 15
      (14, 5) -> 16
      (16, 4) -> 16
      (16, 5) -> 16
      (16, 6) -> 17
      (17, 4) -> 16
      (17, 5) -> 16
      _ -> 0

dfa10InitialState :: Int
dfa10InitialState = 1

dfa10FinalStates :: [Int]
dfa10FinalStates = [2,4,5,6,11,12,13,14]

dfa10Transition :: Int -> Char -> Int
dfa10Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          43 -> 2
          45 -> 3
          48 -> 4
          49 -> 5
          95 -> 9
          98 -> 11
          111 -> 12
          120 -> 13
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,42),(44,44),(46,47),(58,64),(71,94),(96,96),(103,110),(112,119),(121,1114111)] then 1
            else if 50 <= c'' && c'' <= 55 then 6
            else if 56 <= c'' && c'' <= 57 then 7
            else if 65 <= c'' && c'' <= 70 then 8
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,97),(99,102)] then 10
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 2) -> 3
      (1, 3) -> 3
      (1, 4) -> 4
      (1, 5) -> 5
      (1, 6) -> 5
      (1, 7) -> 5
      (2, 0) -> 6
      (2, 4) -> 5
      (2, 5) -> 5
      (2, 6) -> 5
      (2, 7) -> 5
      (2, 9) -> 7
      (2, 11) -> 8
      (2, 12) -> 9
      (2, 13) -> 10
      (3, 0) -> 5
      (3, 4) -> 11
      (3, 5) -> 5
      (3, 6) -> 5
      (3, 7) -> 5
      (4, 0) -> 10
      (4, 11) -> 8
      (4, 12) -> 9
      (4, 13) -> 10
      (5, 0) -> 5
      (5, 4) -> 5
      (5, 5) -> 5
      (5, 6) -> 5
      (5, 7) -> 5
      (5, 9) -> 7
      (6, 0) -> 12
      (6, 4) -> 12
      (6, 5) -> 12
      (6, 6) -> 12
      (6, 7) -> 12
      (6, 8) -> 12
      (6, 9) -> 7
      (6, 10) -> 12
      (6, 11) -> 12
      (7, 0) -> 5
      (7, 4) -> 5
      (7, 5) -> 5
      (7, 6) -> 5
      (7, 7) -> 5
      (8, 0) -> 13
      (8, 4) -> 13
      (8, 5) -> 13
      (9, 0) -> 14
      (9, 4) -> 14
      (9, 5) -> 14
      (9, 6) -> 14
      (10, 0) -> 12
      (10, 4) -> 12
      (10, 5) -> 12
      (10, 6) -> 12
      (10, 7) -> 12
      (10, 8) -> 12
      (10, 10) -> 12
      (10, 11) -> 12
      (12, 0) -> 12
      (12, 4) -> 12
      (12, 5) -> 12
      (12, 6) -> 12
      (12, 7) -> 12
      (12, 8) -> 12
      (12, 9) -> 10
      (12, 10) -> 12
      (12, 11) -> 12
      (13, 0) -> 13
      (13, 4) -> 13
      (13, 5) -> 13
      (13, 9) -> 8
      (14, 0) -> 14
      (14, 4) -> 14
      (14, 5) -> 14
      (14, 6) -> 14
      (14, 9) -> 9
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
          91 -> 1
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
          123 -> 1
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
          9 -> 1
          39 -> 3
          c'' ->
            if 32 <= c'' && c'' <= 38 then 2
            else if 40 <= c'' && c'' <= 1114111 then 4
            else 0 in
    case (q, c') of
      (1, 3) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 3
      (2, 4) -> 2
      _ -> 0

dfa14InitialState :: Int
dfa14InitialState = 1

dfa14FinalStates :: [Int]
dfa14FinalStates = [2]

dfa14Transition :: Int -> Char -> Int
dfa14Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          10 -> 1
          13 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 3
      (3, 1) -> 2
      _ -> 0

dfa15InitialState :: Int
dfa15InitialState = 1

dfa15FinalStates :: [Int]
dfa15FinalStates = [2]

dfa15Transition :: Int -> Char -> Int
dfa15Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          93 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
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
          125 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa17InitialState :: Int
dfa17InitialState = 1

dfa17FinalStates :: [Int]
dfa17FinalStates = [2]

dfa17Transition :: Int -> Char -> Int
dfa17Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          45 -> 1
          95 -> 4
          c'' ->
            if 48 <= c'' && c'' <= 57 then 2
            else if 65 <= c'' && c'' <= 90 then 3
            else if 97 <= c'' && c'' <= 122 then 5
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 3) -> 2
      (1, 4) -> 2
      (1, 5) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
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
      case max (match dfa0InitialState dfa0FinalStates dfa0Transition s, -0) $ max (match dfa1InitialState dfa1FinalStates dfa1Transition s, -1) $ max (match dfa2InitialState dfa2FinalStates dfa2Transition s, -2) $ max (match dfa3InitialState dfa3FinalStates dfa3Transition s, -3) $ max (match dfa4InitialState dfa4FinalStates dfa4Transition s, -4) $ max (match dfa5InitialState dfa5FinalStates dfa5Transition s, -5) $ max (match dfa6InitialState dfa6FinalStates dfa6Transition s, -6) $ max (match dfa7InitialState dfa7FinalStates dfa7Transition s, -7) $ max (match dfa8InitialState dfa8FinalStates dfa8Transition s, -8) $ max (match dfa9InitialState dfa9FinalStates dfa9Transition s, -9) $ max (match dfa10InitialState dfa10FinalStates dfa10Transition s, -10) $ max (match dfa11InitialState dfa11FinalStates dfa11Transition s, -11) $ max (match dfa12InitialState dfa12FinalStates dfa12Transition s, -12) $ max (match dfa13InitialState dfa13FinalStates dfa13Transition s, -13) $ max (match dfa14InitialState dfa14FinalStates dfa14Transition s, -14) $ max (match dfa15InitialState dfa15FinalStates dfa15Transition s, -15) $ max (match dfa16InitialState dfa16FinalStates dfa16Transition s, -16) $ max (match dfa17InitialState dfa17FinalStates dfa17Transition s, -17) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -0 -> do
                x <- saWs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -1 -> do
                x <- saWs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -2 -> do
                x <- saBasicString actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -3 -> do
                x <- saMLBasicString actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -4 -> do
                x <- saMLLiteralString actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -5 -> do
                x <- saBool actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -6 -> do
                x <- saComma actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -7 -> do
                x <- saDot actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -8 -> do
                x <- saEqual actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -9 -> do
                x <- saFloat actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -10 -> do
                x <- saInteger actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -11 -> do
                x <- saLBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -12 -> do
                x <- saLBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -13 -> do
                x <- saLiteralString actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -14 -> do
                x <- saNewline actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -15 -> do
                x <- saRBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -16 -> do
                x <- saRBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -17 -> do
                x <- saUnquotedKey actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else
      return ([], s)



inc :: Lexing (State.State (Int, Int, Int)) ()
inc =
  MonadTrans.lift $ State.modify $ \(i, pos, nest) ->
    (i + 1, pos, nest)

dec :: Lexing (State.State (Int, Int, Int)) ()
dec =
  MonadTrans.lift $ State.modify $ \(i, pos, nest) ->
    (max 0 (i - 1), pos, nest)

withPosition :: (Int -> Int -> String -> Lexing (State.State (Int, Int, Int)) (Maybe Parsing.Token)) -> String -> Lexing (State.State (Int, Int, Int)) (Maybe Parsing.Token)
withPosition f yytext = do
  let n = length yytext
  (i, pos, nest) <- MonadTrans.lift State.get
  MonadTrans.lift $ State.put (i, pos + n, nest)
  f pos n yytext

semanticActions :: SemanticActions (State.State (Int, Int, Int)) (Maybe Parsing.Token)
semanticActions = SemanticActions
  { saWs = withPosition $ \_ _ _ -> return Nothing
  , saNewline = withPosition $ \pos n _ -> do
      (i, _, _) <- MonadTrans.lift State.get
      if i == 0 then
        return $ Just $ Parsing.NEWLINE (pos, n)
      else
        return Nothing
  , saBasicString = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.BASIC_STRING ((pos, n), unescape $ init $ tail $ yytext)
  , saMLBasicString = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.ML_BASIC_STRING ((pos, n), unescape $ take (length yytext - 6) $ drop 3 $ yytext)
  , saBool = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.BOOLEAN ((pos, n), yytext == "true")
  , saComma = withPosition $ \pos n _ -> return $ Just $ Parsing.COMMA (pos, n)
  , saDot = withPosition $ \pos n _ -> return $ Just $ Parsing.DOT (pos, n)
  , saEqual = withPosition $ \pos n _ -> return $ Just $ Parsing.EQUAL (pos, n)
  , saFloat = withPosition $ \pos n yytext ->
      let val =
            case yytext of
              "inf" -> (1.0 / 0.0)
              "+inf" -> (1.0 / 0.0)
              "-inf" -> -(1.0 / 0.0)
              "nan" -> -(0.0 / 0.0)
              "+nan" -> -(0.0 / 0.0)
              "-nan" -> (0.0 / 0.0)
              _ -> read $ toHsInt yytext in
       return $ Just $ Parsing.FLOAT ((pos, n), val)
  , saInteger = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.INTEGER ((pos, n), read $ toHsInt yytext)
  , saLBracket = withPosition $ \pos n _ -> do { inc; return $ Just $ Parsing.LBRACKET (pos, n) }
  , saLBrace = withPosition $ \pos n _ -> do { inc; return $ Just $ Parsing.LBRACE (pos, n) }
  , saRBracket = withPosition $ \pos n _ -> do { dec; return $ Just $ Parsing.RBRACKET (pos, n) }
  , saRBrace = withPosition $ \pos n _ -> do { dec; return $ Just $ Parsing.RBRACE (pos, n) }
  , saUnquotedKey = withPosition $ \pos n yytext -> return $ Just $ Parsing.UNQUOTED_KEY ((pos, n), yytext)
  , saLiteralString = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.LITERAL_STRING ((pos, n), take (length yytext - 6) $ drop 3 $ yytext)
  , saMLLiteralString = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.ML_LITERAL_STRING ((pos, n), init $ tail $ yytext)}
  where
    unescape "" = ""
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
    unescape ('\\' : 'U' : x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : s) =
      Char.chr (read ['0', 'x', x1, x2, x3, x4, x5, x6, x7, x8]) : unescape s
    unescape (c : s) =
      c : unescape s

    toHsInt "" = ""
    toHsInt ('_' : s) = toHsInt s
    toHsInt ('+' : s) = toHsInt s
    toHsInt (c : s) = c : toHsInt s


module  Language.Haskell2010.Lexing  where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char


import qualified Control.Monad.State          as State
import qualified Control.Monad.Trans          as MonadTrans
import qualified Language.Haskell2010.Parsing as Parsing

newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }

data LexingState =
    Initial
  | Dashes
  | Nested
  deriving (Eq, Ord, Read, Show)

data SemanticActions m a = SemanticActions
  { saWhitechar :: String -> Lexing m a
  , saOpenDashes :: String -> Lexing m a
  , saOpenNested :: String -> Lexing m a
  , saAs :: String -> Lexing m a
  , saBackquote :: String -> Lexing m a
  , saCase :: String -> Lexing m a
  , saClass :: String -> Lexing m a
  , saColonColon :: String -> Lexing m a
  , saComma :: String -> Lexing m a
  , saDArrow :: String -> Lexing m a
  , saData :: String -> Lexing m a
  , saDefault :: String -> Lexing m a
  , saDeriving :: String -> Lexing m a
  , saDo :: String -> Lexing m a
  , saDotDot :: String -> Lexing m a
  , saElse :: String -> Lexing m a
  , saEqual :: String -> Lexing m a
  , saExcl :: String -> Lexing m a
  , saExport :: String -> Lexing m a
  , saForeign :: String -> Lexing m a
  , saHiding :: String -> Lexing m a
  , saImport :: String -> Lexing m a
  , saIf :: String -> Lexing m a
  , saIn :: String -> Lexing m a
  , saInfix :: String -> Lexing m a
  , saInfixL :: String -> Lexing m a
  , saInfixR :: String -> Lexing m a
  , saInstance :: String -> Lexing m a
  , saInteger :: String -> Lexing m a
  , saLambda :: String -> Lexing m a
  , saLArrow :: String -> Lexing m a
  , saLBrace :: String -> Lexing m a
  , saLBracket :: String -> Lexing m a
  , saLet :: String -> Lexing m a
  , saLParen :: String -> Lexing m a
  , saMinus :: String -> Lexing m a
  , saModule :: String -> Lexing m a
  , saNewtype :: String -> Lexing m a
  , saOf :: String -> Lexing m a
  , saPipe :: String -> Lexing m a
  , saQualified :: String -> Lexing m a
  , saRArrow :: String -> Lexing m a
  , saRBrace :: String -> Lexing m a
  , saRBracket :: String -> Lexing m a
  , saRParen :: String -> Lexing m a
  , saSemicolon :: String -> Lexing m a
  , saString :: String -> Lexing m a
  , saThen :: String -> Lexing m a
  , saType :: String -> Lexing m a
  , saWhere :: String -> Lexing m a
  , saVarId :: String -> Lexing m a
  , saConId :: String -> Lexing m a
  , saVarSym :: String -> Lexing m a
  , saConSym :: String -> Lexing m a
  , saCloseDashes :: String -> Lexing m a
  , saComment :: String -> Lexing m a
  , saCloseNested :: String -> Lexing m a }

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
          11 -> 3
          12 -> 4
          13 -> 5
          32 -> 6
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(160,160),(5760,5760),(8192,8202),(8239,8239),(8287,8287),(12288,12288)] then 7
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 3) -> 2
      (1, 4) -> 2
      (1, 5) -> 3
      (1, 6) -> 2
      (1, 7) -> 2
      (3, 2) -> 2
      _ -> 0

dfa1InitialState :: Int
dfa1InitialState = 1

dfa1FinalStates :: [Int]
dfa1FinalStates = [3]

dfa1Transition :: Int -> Char -> Int
dfa1Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          45 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 1) -> 3
      (3, 1) -> 3
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
          45 -> 1
          123 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
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
          97 -> 1
          115 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          96 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa5InitialState :: Int
dfa5InitialState = 1

dfa5FinalStates :: [Int]
dfa5FinalStates = [5]

dfa5Transition :: Int -> Char -> Int
dfa5Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          97 -> 1
          99 -> 2
          101 -> 3
          115 -> 4
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      (3, 4) -> 4
      (4, 3) -> 5
      _ -> 0

dfa6InitialState :: Int
dfa6InitialState = 1

dfa6FinalStates :: [Int]
dfa6FinalStates = [6]

dfa6Transition :: Int -> Char -> Int
dfa6Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          97 -> 1
          99 -> 2
          108 -> 3
          115 -> 4
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 3) -> 3
      (3, 1) -> 4
      (4, 4) -> 5
      (5, 4) -> 6
      _ -> 0

dfa7InitialState :: Int
dfa7InitialState = 1

dfa7FinalStates :: [Int]
dfa7FinalStates = [3]

dfa7Transition :: Int -> Char -> Int
dfa7Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          58 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 1) -> 3
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
          44 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa9InitialState :: Int
dfa9InitialState = 1

dfa9FinalStates :: [Int]
dfa9FinalStates = [3]

dfa9Transition :: Int -> Char -> Int
dfa9Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          61 -> 1
          62 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa10InitialState :: Int
dfa10InitialState = 1

dfa10FinalStates :: [Int]
dfa10FinalStates = [5]

dfa10Transition :: Int -> Char -> Int
dfa10Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          97 -> 1
          100 -> 2
          116 -> 3
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      (3, 3) -> 4
      (4, 1) -> 5
      _ -> 0

dfa11InitialState :: Int
dfa11InitialState = 1

dfa11FinalStates :: [Int]
dfa11FinalStates = [8]

dfa11Transition :: Int -> Char -> Int
dfa11Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          97 -> 1
          100 -> 2
          101 -> 3
          102 -> 4
          108 -> 5
          116 -> 6
          117 -> 7
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 3) -> 3
      (3, 4) -> 4
      (4, 1) -> 5
      (5, 7) -> 6
      (6, 5) -> 7
      (7, 6) -> 8
      _ -> 0

dfa12InitialState :: Int
dfa12InitialState = 1

dfa12FinalStates :: [Int]
dfa12FinalStates = [9]

dfa12Transition :: Int -> Char -> Int
dfa12Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          100 -> 1
          101 -> 2
          103 -> 3
          105 -> 4
          110 -> 5
          114 -> 6
          118 -> 7
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      (3, 6) -> 4
      (4, 4) -> 5
      (5, 7) -> 6
      (6, 4) -> 7
      (7, 5) -> 8
      (8, 3) -> 9
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
          100 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa14InitialState :: Int
dfa14InitialState = 1

dfa14FinalStates :: [Int]
dfa14FinalStates = [3]

dfa14Transition :: Int -> Char -> Int
dfa14Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          46 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 1) -> 3
      _ -> 0

dfa15InitialState :: Int
dfa15InitialState = 1

dfa15FinalStates :: [Int]
dfa15FinalStates = [5]

dfa15Transition :: Int -> Char -> Int
dfa15Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          108 -> 2
          115 -> 3
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      (3, 3) -> 4
      (4, 1) -> 5
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
          61 -> 1
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
          33 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa18InitialState :: Int
dfa18InitialState = 1

dfa18FinalStates :: [Int]
dfa18FinalStates = [7]

dfa18Transition :: Int -> Char -> Int
dfa18Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          111 -> 2
          112 -> 3
          114 -> 4
          116 -> 5
          120 -> 6
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 6) -> 3
      (3, 3) -> 4
      (4, 2) -> 5
      (5, 4) -> 6
      (6, 5) -> 7
      _ -> 0

dfa19InitialState :: Int
dfa19InitialState = 1

dfa19FinalStates :: [Int]
dfa19FinalStates = [8]

dfa19Transition :: Int -> Char -> Int
dfa19Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          102 -> 2
          103 -> 3
          105 -> 4
          110 -> 5
          111 -> 6
          114 -> 7
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 6) -> 3
      (3, 7) -> 4
      (4, 1) -> 5
      (5, 4) -> 6
      (6, 3) -> 7
      (7, 5) -> 8
      _ -> 0

dfa20InitialState :: Int
dfa20InitialState = 1

dfa20FinalStates :: [Int]
dfa20FinalStates = [7]

dfa20Transition :: Int -> Char -> Int
dfa20Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          100 -> 1
          103 -> 2
          104 -> 3
          105 -> 4
          110 -> 5
          c'' ->
            0 in
    case (q, c') of
      (1, 3) -> 2
      (2, 4) -> 3
      (3, 1) -> 4
      (4, 4) -> 5
      (5, 5) -> 6
      (6, 2) -> 7
      _ -> 0

dfa21InitialState :: Int
dfa21InitialState = 1

dfa21FinalStates :: [Int]
dfa21FinalStates = [7]

dfa21Transition :: Int -> Char -> Int
dfa21Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          105 -> 1
          109 -> 2
          111 -> 3
          112 -> 4
          114 -> 5
          116 -> 6
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      (3, 4) -> 4
      (4, 3) -> 5
      (5, 5) -> 6
      (6, 6) -> 7
      _ -> 0

dfa22InitialState :: Int
dfa22InitialState = 1

dfa22FinalStates :: [Int]
dfa22FinalStates = [3]

dfa22Transition :: Int -> Char -> Int
dfa22Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          102 -> 1
          105 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      _ -> 0

dfa23InitialState :: Int
dfa23InitialState = 1

dfa23FinalStates :: [Int]
dfa23FinalStates = [3]

dfa23Transition :: Int -> Char -> Int
dfa23Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          105 -> 1
          110 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa24InitialState :: Int
dfa24InitialState = 1

dfa24FinalStates :: [Int]
dfa24FinalStates = [6]

dfa24Transition :: Int -> Char -> Int
dfa24Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          102 -> 1
          105 -> 2
          110 -> 3
          120 -> 4
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 3) -> 3
      (3, 1) -> 4
      (4, 2) -> 5
      (5, 4) -> 6
      _ -> 0

dfa25InitialState :: Int
dfa25InitialState = 1

dfa25FinalStates :: [Int]
dfa25FinalStates = [7]

dfa25Transition :: Int -> Char -> Int
dfa25Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          102 -> 1
          105 -> 2
          108 -> 3
          110 -> 4
          120 -> 5
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 4) -> 3
      (3, 1) -> 4
      (4, 2) -> 5
      (5, 5) -> 6
      (6, 3) -> 7
      _ -> 0

dfa26InitialState :: Int
dfa26InitialState = 1

dfa26FinalStates :: [Int]
dfa26FinalStates = [7]

dfa26Transition :: Int -> Char -> Int
dfa26Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          102 -> 1
          105 -> 2
          110 -> 3
          114 -> 4
          120 -> 5
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 3) -> 3
      (3, 1) -> 4
      (4, 2) -> 5
      (5, 5) -> 6
      (6, 4) -> 7
      _ -> 0

dfa27InitialState :: Int
dfa27InitialState = 1

dfa27FinalStates :: [Int]
dfa27FinalStates = [9]

dfa27Transition :: Int -> Char -> Int
dfa27Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          97 -> 1
          99 -> 2
          101 -> 3
          105 -> 4
          110 -> 5
          115 -> 6
          116 -> 7
          c'' ->
            0 in
    case (q, c') of
      (1, 4) -> 2
      (2, 5) -> 3
      (3, 6) -> 4
      (4, 7) -> 5
      (5, 1) -> 6
      (6, 5) -> 7
      (7, 2) -> 8
      (8, 3) -> 9
      _ -> 0

dfa28InitialState :: Int
dfa28InitialState = 1

dfa28FinalStates :: [Int]
dfa28FinalStates = [2,3,6,7]

dfa28Transition :: Int -> Char -> Int
dfa28Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          48 -> 1
          79 -> 5
          88 -> 6
          111 -> 8
          120 -> 9
          c'' ->
            if 49 <= c'' && c'' <= 55 then 2
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(56,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 3
            else if 65 <= c'' && c'' <= 70 then 4
            else if 97 <= c'' && c'' <= 102 then 7
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 3
      (1, 3) -> 3
      (2, 1) -> 3
      (2, 2) -> 3
      (2, 3) -> 3
      (2, 5) -> 4
      (2, 6) -> 5
      (2, 8) -> 4
      (2, 9) -> 5
      (3, 1) -> 3
      (3, 2) -> 3
      (3, 3) -> 3
      (4, 1) -> 6
      (4, 2) -> 6
      (5, 1) -> 7
      (5, 2) -> 7
      (5, 3) -> 7
      (5, 4) -> 7
      (5, 7) -> 7
      (6, 1) -> 6
      (6, 2) -> 6
      (7, 1) -> 7
      (7, 2) -> 7
      (7, 3) -> 7
      (7, 4) -> 7
      (7, 7) -> 7
      _ -> 0

dfa29InitialState :: Int
dfa29InitialState = 1

dfa29FinalStates :: [Int]
dfa29FinalStates = [2]

dfa29Transition :: Int -> Char -> Int
dfa29Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          92 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa30InitialState :: Int
dfa30InitialState = 1

dfa30FinalStates :: [Int]
dfa30FinalStates = [3]

dfa30Transition :: Int -> Char -> Int
dfa30Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          45 -> 1
          60 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      _ -> 0

dfa31InitialState :: Int
dfa31InitialState = 1

dfa31FinalStates :: [Int]
dfa31FinalStates = [2]

dfa31Transition :: Int -> Char -> Int
dfa31Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          123 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa32InitialState :: Int
dfa32InitialState = 1

dfa32FinalStates :: [Int]
dfa32FinalStates = [2]

dfa32Transition :: Int -> Char -> Int
dfa32Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          91 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa33InitialState :: Int
dfa33InitialState = 1

dfa33FinalStates :: [Int]
dfa33FinalStates = [4]

dfa33Transition :: Int -> Char -> Int
dfa33Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          108 -> 2
          116 -> 3
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      (3, 3) -> 4
      _ -> 0

dfa34InitialState :: Int
dfa34InitialState = 1

dfa34FinalStates :: [Int]
dfa34FinalStates = [2]

dfa34Transition :: Int -> Char -> Int
dfa34Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          40 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa35InitialState :: Int
dfa35InitialState = 1

dfa35FinalStates :: [Int]
dfa35FinalStates = [2]

dfa35Transition :: Int -> Char -> Int
dfa35Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          45 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa36InitialState :: Int
dfa36InitialState = 1

dfa36FinalStates :: [Int]
dfa36FinalStates = [7]

dfa36Transition :: Int -> Char -> Int
dfa36Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          100 -> 1
          101 -> 2
          108 -> 3
          109 -> 4
          111 -> 5
          117 -> 6
          c'' ->
            0 in
    case (q, c') of
      (1, 4) -> 2
      (2, 5) -> 3
      (3, 1) -> 4
      (4, 6) -> 5
      (5, 3) -> 6
      (6, 2) -> 7
      _ -> 0

dfa37InitialState :: Int
dfa37InitialState = 1

dfa37FinalStates :: [Int]
dfa37FinalStates = [8]

dfa37Transition :: Int -> Char -> Int
dfa37Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          110 -> 2
          112 -> 3
          116 -> 4
          119 -> 5
          121 -> 6
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      (3, 5) -> 4
      (4, 4) -> 5
      (5, 6) -> 6
      (6, 3) -> 7
      (7, 1) -> 8
      _ -> 0

dfa38InitialState :: Int
dfa38InitialState = 1

dfa38FinalStates :: [Int]
dfa38FinalStates = [3]

dfa38Transition :: Int -> Char -> Int
dfa38Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          102 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      _ -> 0

dfa39InitialState :: Int
dfa39InitialState = 1

dfa39FinalStates :: [Int]
dfa39FinalStates = [2]

dfa39Transition :: Int -> Char -> Int
dfa39Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          124 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa40InitialState :: Int
dfa40InitialState = 1

dfa40FinalStates :: [Int]
dfa40FinalStates = [10]

dfa40Transition :: Int -> Char -> Int
dfa40Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          97 -> 1
          100 -> 2
          101 -> 3
          102 -> 4
          105 -> 5
          108 -> 6
          113 -> 7
          117 -> 8
          c'' ->
            0 in
    case (q, c') of
      (1, 7) -> 2
      (2, 8) -> 3
      (3, 1) -> 4
      (4, 6) -> 5
      (5, 5) -> 6
      (6, 4) -> 7
      (7, 5) -> 8
      (8, 3) -> 9
      (9, 2) -> 10
      _ -> 0

dfa41InitialState :: Int
dfa41InitialState = 1

dfa41FinalStates :: [Int]
dfa41FinalStates = [3]

dfa41Transition :: Int -> Char -> Int
dfa41Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          45 -> 1
          62 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa42InitialState :: Int
dfa42InitialState = 1

dfa42FinalStates :: [Int]
dfa42FinalStates = [2]

dfa42Transition :: Int -> Char -> Int
dfa42Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          125 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa43InitialState :: Int
dfa43InitialState = 1

dfa43FinalStates :: [Int]
dfa43FinalStates = [2]

dfa43Transition :: Int -> Char -> Int
dfa43Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          93 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa44InitialState :: Int
dfa44InitialState = 1

dfa44FinalStates :: [Int]
dfa44FinalStates = [2]

dfa44Transition :: Int -> Char -> Int
dfa44Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          41 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa45InitialState :: Int
dfa45InitialState = 1

dfa45FinalStates :: [Int]
dfa45FinalStates = [2]

dfa45Transition :: Int -> Char -> Int
dfa45Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          59 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa46InitialState :: Int
dfa46InitialState = 1

dfa46FinalStates :: [Int]
dfa46FinalStates = [3,4,8]

dfa46Transition :: Int -> Char -> Int
dfa46Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          9 -> 2
          10 -> 3
          11 -> 4
          12 -> 5
          13 -> 6
          32 -> 7
          34 -> 8
          38 -> 9
          39 -> 10
          49 -> 12
          50 -> 13
          51 -> 14
          52 -> 15
          65 -> 17
          66 -> 18
          67 -> 19
          68 -> 20
          69 -> 21
          70 -> 22
          71 -> 23
          72 -> 24
          73 -> 25
          75 -> 26
          76 -> 27
          77 -> 28
          78 -> 29
          79 -> 30
          80 -> 31
          81 -> 32
          82 -> 33
          83 -> 34
          84 -> 35
          85 -> 36
          86 -> 37
          88 -> 38
          89 -> 39
          92 -> 40
          97 -> 41
          98 -> 42
          102 -> 44
          110 -> 45
          111 -> 46
          114 -> 47
          116 -> 48
          118 -> 49
          120 -> 50
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,8),(14,31),(33,33),(35,37),(40,47),(58,64),(74,74),(87,87),(90,91),(93,96),(103,109),(112,113),(115,115),(117,117),(119,119),(121,159),(161,1631),(1642,1775),(1786,1983),(1994,2405),(2416,2533),(2544,2661),(2672,2789),(2800,2917),(2928,3045),(3056,3173),(3184,3301),(3312,3429),(3440,3557),(3568,3663),(3674,3791),(3802,3871),(3882,4159),(4170,4239),(4250,5759),(5761,6111),(6122,6159),(6170,6469),(6480,6607),(6618,6783),(6794,6799),(6810,6991),(7002,7087),(7098,7231),(7242,7247),(7258,8191),(8203,8238),(8240,8286),(8288,12287),(12289,42527),(42538,43215),(43226,43263),(43274,43471),(43482,43503),(43514,43599),(43610,44015),(44026,65295),(65306,66719),(66730,69733),(69744,69871),(69882,69941),(69952,70095),(70106,70383),(70394,70863),(70874,71247),(71258,71359),(71370,71903),(71914,92767),(92778,93007),(93018,120781),(120832,1114111)] then 1
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,48),(53,55)] then 11
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(56,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 16
            else if 99 <= c'' && c'' <= 101 then 43
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(160,160),(5760,5760),(8192,8202),(8239,8239),(8287,8287),(12288,12288)] then 51
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 8) -> 2
      (2, 0) -> 3
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
      (2, 7) -> 2
      (2, 8) -> 4
      (2, 9) -> 2
      (2, 10) -> 5
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
      (2, 22) -> 2
      (2, 23) -> 2
      (2, 24) -> 2
      (2, 25) -> 2
      (2, 26) -> 2
      (2, 27) -> 2
      (2, 28) -> 2
      (2, 29) -> 2
      (2, 30) -> 2
      (2, 31) -> 2
      (2, 32) -> 2
      (2, 33) -> 2
      (2, 34) -> 2
      (2, 35) -> 2
      (2, 36) -> 2
      (2, 37) -> 2
      (2, 38) -> 2
      (2, 39) -> 2
      (2, 40) -> 6
      (2, 41) -> 2
      (2, 42) -> 2
      (2, 43) -> 2
      (2, 44) -> 2
      (2, 45) -> 2
      (2, 46) -> 2
      (2, 47) -> 2
      (2, 48) -> 2
      (2, 49) -> 2
      (2, 50) -> 2
      (2, 51) -> 2
      (3, 0) -> 3
      (3, 1) -> 2
      (3, 2) -> 7
      (3, 3) -> 7
      (3, 4) -> 7
      (3, 5) -> 7
      (3, 6) -> 7
      (3, 7) -> 7
      (3, 8) -> 8
      (3, 9) -> 2
      (3, 10) -> 5
      (3, 11) -> 2
      (3, 12) -> 2
      (3, 13) -> 2
      (3, 14) -> 2
      (3, 15) -> 2
      (3, 16) -> 2
      (3, 17) -> 2
      (3, 18) -> 2
      (3, 19) -> 2
      (3, 20) -> 2
      (3, 21) -> 2
      (3, 22) -> 2
      (3, 23) -> 2
      (3, 24) -> 2
      (3, 25) -> 2
      (3, 26) -> 2
      (3, 27) -> 2
      (3, 28) -> 2
      (3, 29) -> 2
      (3, 30) -> 2
      (3, 31) -> 2
      (3, 32) -> 2
      (3, 33) -> 2
      (3, 34) -> 2
      (3, 35) -> 2
      (3, 36) -> 2
      (3, 37) -> 2
      (3, 38) -> 2
      (3, 39) -> 2
      (3, 40) -> 9
      (3, 41) -> 2
      (3, 42) -> 2
      (3, 43) -> 2
      (3, 44) -> 2
      (3, 45) -> 2
      (3, 46) -> 2
      (3, 47) -> 2
      (3, 48) -> 2
      (3, 49) -> 2
      (3, 50) -> 2
      (3, 51) -> 7
      (5, 0) -> 3
      (5, 1) -> 2
      (5, 2) -> 7
      (5, 3) -> 7
      (5, 4) -> 7
      (5, 5) -> 7
      (5, 6) -> 7
      (5, 7) -> 7
      (5, 8) -> 4
      (5, 9) -> 2
      (5, 10) -> 5
      (5, 11) -> 2
      (5, 12) -> 2
      (5, 13) -> 2
      (5, 14) -> 2
      (5, 15) -> 2
      (5, 16) -> 2
      (5, 17) -> 2
      (5, 18) -> 2
      (5, 19) -> 2
      (5, 20) -> 2
      (5, 21) -> 2
      (5, 22) -> 2
      (5, 23) -> 2
      (5, 24) -> 2
      (5, 25) -> 2
      (5, 26) -> 2
      (5, 27) -> 2
      (5, 28) -> 2
      (5, 29) -> 2
      (5, 30) -> 2
      (5, 31) -> 2
      (5, 32) -> 2
      (5, 33) -> 2
      (5, 34) -> 2
      (5, 35) -> 2
      (5, 36) -> 2
      (5, 37) -> 2
      (5, 38) -> 2
      (5, 39) -> 2
      (5, 40) -> 6
      (5, 41) -> 2
      (5, 42) -> 2
      (5, 43) -> 2
      (5, 44) -> 2
      (5, 45) -> 2
      (5, 46) -> 2
      (5, 47) -> 2
      (5, 48) -> 2
      (5, 49) -> 2
      (5, 50) -> 2
      (5, 51) -> 7
      (6, 0) -> 2
      (6, 8) -> 2
      (6, 9) -> 2
      (6, 10) -> 2
      (6, 11) -> 2
      (6, 12) -> 2
      (6, 13) -> 2
      (6, 14) -> 2
      (6, 15) -> 2
      (6, 16) -> 2
      (6, 17) -> 10
      (6, 18) -> 11
      (6, 19) -> 12
      (6, 20) -> 13
      (6, 21) -> 14
      (6, 22) -> 15
      (6, 23) -> 16
      (6, 24) -> 17
      (6, 27) -> 18
      (6, 29) -> 19
      (6, 33) -> 16
      (6, 34) -> 20
      (6, 36) -> 16
      (6, 37) -> 17
      (6, 40) -> 2
      (6, 41) -> 2
      (6, 42) -> 2
      (6, 44) -> 2
      (6, 45) -> 2
      (6, 46) -> 21
      (6, 47) -> 2
      (6, 48) -> 2
      (6, 49) -> 2
      (6, 50) -> 22
      (7, 0) -> 3
      (7, 1) -> 2
      (7, 2) -> 7
      (7, 3) -> 7
      (7, 4) -> 7
      (7, 5) -> 7
      (7, 6) -> 7
      (7, 7) -> 7
      (7, 8) -> 4
      (7, 9) -> 2
      (7, 10) -> 5
      (7, 11) -> 2
      (7, 12) -> 2
      (7, 13) -> 2
      (7, 14) -> 2
      (7, 15) -> 2
      (7, 16) -> 2
      (7, 17) -> 2
      (7, 18) -> 2
      (7, 19) -> 2
      (7, 20) -> 2
      (7, 21) -> 2
      (7, 22) -> 2
      (7, 23) -> 2
      (7, 24) -> 2
      (7, 25) -> 2
      (7, 26) -> 2
      (7, 27) -> 2
      (7, 28) -> 2
      (7, 29) -> 2
      (7, 30) -> 2
      (7, 31) -> 2
      (7, 32) -> 2
      (7, 33) -> 2
      (7, 34) -> 2
      (7, 35) -> 2
      (7, 36) -> 2
      (7, 37) -> 2
      (7, 38) -> 2
      (7, 39) -> 2
      (7, 40) -> 9
      (7, 41) -> 2
      (7, 42) -> 2
      (7, 43) -> 2
      (7, 44) -> 2
      (7, 45) -> 2
      (7, 46) -> 2
      (7, 47) -> 2
      (7, 48) -> 2
      (7, 49) -> 2
      (7, 50) -> 2
      (7, 51) -> 7
      (8, 0) -> 3
      (8, 1) -> 2
      (8, 2) -> 2
      (8, 3) -> 2
      (8, 4) -> 2
      (8, 5) -> 2
      (8, 6) -> 2
      (8, 7) -> 2
      (8, 8) -> 4
      (8, 9) -> 2
      (8, 10) -> 5
      (8, 11) -> 2
      (8, 12) -> 2
      (8, 13) -> 2
      (8, 14) -> 2
      (8, 15) -> 2
      (8, 16) -> 2
      (8, 17) -> 2
      (8, 18) -> 2
      (8, 19) -> 2
      (8, 20) -> 2
      (8, 21) -> 2
      (8, 22) -> 2
      (8, 23) -> 2
      (8, 24) -> 2
      (8, 25) -> 2
      (8, 26) -> 2
      (8, 27) -> 2
      (8, 28) -> 2
      (8, 29) -> 2
      (8, 30) -> 2
      (8, 31) -> 2
      (8, 32) -> 2
      (8, 33) -> 2
      (8, 34) -> 2
      (8, 35) -> 2
      (8, 36) -> 2
      (8, 37) -> 2
      (8, 38) -> 2
      (8, 39) -> 2
      (8, 40) -> 6
      (8, 41) -> 2
      (8, 42) -> 2
      (8, 43) -> 2
      (8, 44) -> 2
      (8, 45) -> 2
      (8, 46) -> 2
      (8, 47) -> 2
      (8, 48) -> 2
      (8, 49) -> 2
      (8, 50) -> 2
      (8, 51) -> 2
      (9, 0) -> 3
      (9, 1) -> 2
      (9, 2) -> 2
      (9, 3) -> 2
      (9, 4) -> 2
      (9, 5) -> 2
      (9, 6) -> 2
      (9, 7) -> 2
      (9, 8) -> 8
      (9, 9) -> 2
      (9, 10) -> 5
      (9, 11) -> 2
      (9, 12) -> 2
      (9, 13) -> 2
      (9, 14) -> 2
      (9, 15) -> 2
      (9, 16) -> 2
      (9, 17) -> 2
      (9, 18) -> 2
      (9, 19) -> 2
      (9, 20) -> 2
      (9, 21) -> 2
      (9, 22) -> 2
      (9, 23) -> 2
      (9, 24) -> 2
      (9, 25) -> 2
      (9, 26) -> 2
      (9, 27) -> 2
      (9, 28) -> 2
      (9, 29) -> 2
      (9, 30) -> 2
      (9, 31) -> 2
      (9, 32) -> 2
      (9, 33) -> 2
      (9, 34) -> 2
      (9, 35) -> 2
      (9, 36) -> 2
      (9, 37) -> 2
      (9, 38) -> 2
      (9, 39) -> 2
      (9, 40) -> 9
      (9, 41) -> 2
      (9, 42) -> 2
      (9, 43) -> 2
      (9, 44) -> 2
      (9, 45) -> 2
      (9, 46) -> 2
      (9, 47) -> 2
      (9, 48) -> 2
      (9, 49) -> 2
      (9, 50) -> 2
      (9, 51) -> 2
      (10, 0) -> 23
      (10, 19) -> 23
      (11, 0) -> 2
      (11, 21) -> 24
      (11, 34) -> 2
      (12, 0) -> 2
      (12, 17) -> 25
      (12, 33) -> 2
      (13, 0) -> 26
      (13, 19) -> 27
      (13, 21) -> 24
      (13, 27) -> 28
      (14, 0) -> 2
      (14, 28) -> 2
      (14, 29) -> 29
      (14, 30) -> 17
      (14, 34) -> 30
      (14, 35) -> 31
      (15, 0) -> 2
      (15, 22) -> 2
      (15, 34) -> 2
      (16, 0) -> 2
      (16, 34) -> 2
      (17, 0) -> 2
      (17, 35) -> 2
      (18, 0) -> 2
      (18, 22) -> 2
      (19, 0) -> 32
      (19, 17) -> 23
      (19, 36) -> 24
      (20, 0) -> 2
      (20, 25) -> 2
      (20, 30) -> 2
      (20, 31) -> 2
      (20, 35) -> 33
      (20, 36) -> 34
      (20, 39) -> 25
      (21, 0) -> 2
      (21, 11) -> 2
      (21, 12) -> 2
      (21, 13) -> 2
      (21, 14) -> 2
      (21, 15) -> 2
      (22, 0) -> 2
      (22, 11) -> 2
      (22, 12) -> 2
      (22, 13) -> 2
      (22, 14) -> 2
      (22, 15) -> 2
      (22, 16) -> 2
      (22, 17) -> 2
      (22, 18) -> 2
      (22, 19) -> 2
      (22, 20) -> 2
      (22, 21) -> 2
      (22, 22) -> 2
      (22, 41) -> 2
      (22, 42) -> 2
      (22, 43) -> 2
      (22, 44) -> 2
      (23, 0) -> 2
      (23, 26) -> 2
      (24, 0) -> 2
      (24, 27) -> 2
      (25, 0) -> 2
      (25, 29) -> 2
      (26, 0) -> 2
      (26, 12) -> 2
      (26, 13) -> 2
      (26, 14) -> 2
      (26, 15) -> 2
      (26, 21) -> 2
      (26, 27) -> 2
      (27, 0) -> 2
      (27, 12) -> 2
      (27, 13) -> 2
      (27, 14) -> 2
      (27, 15) -> 2
      (28, 0) -> 2
      (28, 21) -> 2
      (29, 0) -> 2
      (29, 32) -> 2
      (30, 0) -> 2
      (30, 19) -> 2
      (31, 0) -> 2
      (31, 18) -> 2
      (31, 38) -> 2
      (32, 0) -> 2
      (32, 26) -> 2
      (32, 27) -> 2
      (33, 0) -> 2
      (33, 38) -> 2
      (34, 0) -> 2
      (34, 18) -> 2
      _ -> 0

dfa47InitialState :: Int
dfa47InitialState = 1

dfa47FinalStates :: [Int]
dfa47FinalStates = [5]

dfa47Transition :: Int -> Char -> Int
dfa47Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          104 -> 2
          110 -> 3
          116 -> 4
          c'' ->
            0 in
    case (q, c') of
      (1, 4) -> 2
      (2, 2) -> 3
      (3, 1) -> 4
      (4, 3) -> 5
      _ -> 0

dfa48InitialState :: Int
dfa48InitialState = 1

dfa48FinalStates :: [Int]
dfa48FinalStates = [5]

dfa48Transition :: Int -> Char -> Int
dfa48Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          112 -> 2
          116 -> 3
          121 -> 4
          c'' ->
            0 in
    case (q, c') of
      (1, 3) -> 2
      (2, 4) -> 3
      (3, 2) -> 4
      (4, 1) -> 5
      _ -> 0

dfa49InitialState :: Int
dfa49InitialState = 1

dfa49FinalStates :: [Int]
dfa49FinalStates = [6]

dfa49Transition :: Int -> Char -> Int
dfa49Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          101 -> 1
          104 -> 2
          114 -> 3
          119 -> 4
          c'' ->
            0 in
    case (q, c') of
      (1, 4) -> 2
      (2, 2) -> 3
      (3, 1) -> 4
      (4, 3) -> 5
      (5, 1) -> 6
      _ -> 0

dfa50InitialState :: Int
dfa50InitialState = 1

dfa50FinalStates :: [Int]
dfa50FinalStates = [3]

dfa50Transition :: Int -> Char -> Int
dfa50Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          39 -> 1
          46 -> 2
          95 -> 5
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 3
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(65,90),(192,214),(216,222),(256,256),(258,258),(260,260),(262,262),(264,264),(266,266),(268,268),(270,270),(272,272),(274,274),(276,276),(278,278),(280,280),(282,282),(284,284),(286,286),(288,288),(290,290),(292,292),(294,294),(296,296),(298,298),(300,300),(302,302),(304,304),(306,306),(308,308),(310,310),(313,313),(315,315),(317,317),(319,319),(321,321),(323,323),(325,325),(327,327),(330,330),(332,332),(334,334),(336,336),(338,338),(340,340),(342,342),(344,344),(346,346),(348,348),(350,350),(352,352),(354,354),(356,356),(358,358),(360,360),(362,362),(364,364),(366,366),(368,368),(370,370),(372,372),(374,374),(376,377),(379,379),(381,381),(385,386),(388,388),(390,391),(393,395),(398,401),(403,404),(406,408),(412,413),(415,416),(418,418),(420,420),(422,423),(425,425),(428,428),(430,431),(433,435),(437,437),(439,440),(444,444),(452,452),(455,455),(458,458),(461,461),(463,463),(465,465),(467,467),(469,469),(471,471),(473,473),(475,475),(478,478),(480,480),(482,482),(484,484),(486,486),(488,488),(490,490),(492,492),(494,494),(497,497),(500,500),(502,504),(506,506),(508,508),(510,510),(512,512),(514,514),(516,516),(518,518),(520,520),(522,522),(524,524),(526,526),(528,528),(530,530),(532,532),(534,534),(536,536),(538,538),(540,540),(542,542),(544,544),(546,546),(548,548),(550,550),(552,552),(554,554),(556,556),(558,558),(560,560),(562,562),(570,571),(573,574),(577,577),(579,582),(584,584),(586,586),(588,588),(590,590),(880,880),(882,882),(886,886),(895,895),(902,902),(904,906),(908,908),(910,911),(913,929),(931,939),(975,975),(978,980),(984,984),(986,986),(988,988),(990,990),(992,992),(994,994),(996,996),(998,998),(1000,1000),(1002,1002),(1004,1004),(1006,1006),(1012,1012),(1015,1015),(1017,1018),(1021,1071),(1120,1120),(1122,1122),(1124,1124),(1126,1126),(1128,1128),(1130,1130),(1132,1132),(1134,1134),(1136,1136),(1138,1138),(1140,1140),(1142,1142),(1144,1144),(1146,1146),(1148,1148),(1150,1150),(1152,1152),(1162,1162),(1164,1164),(1166,1166),(1168,1168),(1170,1170),(1172,1172),(1174,1174),(1176,1176),(1178,1178),(1180,1180),(1182,1182),(1184,1184),(1186,1186),(1188,1188),(1190,1190),(1192,1192),(1194,1194),(1196,1196),(1198,1198),(1200,1200),(1202,1202),(1204,1204),(1206,1206),(1208,1208),(1210,1210),(1212,1212),(1214,1214),(1216,1217),(1219,1219),(1221,1221),(1223,1223),(1225,1225),(1227,1227),(1229,1229),(1232,1232),(1234,1234),(1236,1236),(1238,1238),(1240,1240),(1242,1242),(1244,1244),(1246,1246),(1248,1248),(1250,1250),(1252,1252),(1254,1254),(1256,1256),(1258,1258),(1260,1260),(1262,1262),(1264,1264),(1266,1266),(1268,1268),(1270,1270),(1272,1272),(1274,1274),(1276,1276),(1278,1278),(1280,1280),(1282,1282),(1284,1284),(1286,1286),(1288,1288),(1290,1290),(1292,1292),(1294,1294),(1296,1296),(1298,1298),(1300,1300),(1302,1302),(1304,1304),(1306,1306),(1308,1308),(1310,1310),(1312,1312),(1314,1314),(1316,1316),(1318,1318),(1320,1320),(1322,1322),(1324,1324),(1326,1326),(1329,1366),(4256,4293),(4295,4295),(4301,4301),(7680,7680),(7682,7682),(7684,7684),(7686,7686),(7688,7688),(7690,7690),(7692,7692),(7694,7694),(7696,7696),(7698,7698),(7700,7700),(7702,7702),(7704,7704),(7706,7706),(7708,7708),(7710,7710),(7712,7712),(7714,7714),(7716,7716),(7718,7718),(7720,7720),(7722,7722),(7724,7724),(7726,7726),(7728,7728),(7730,7730),(7732,7732),(7734,7734),(7736,7736),(7738,7738),(7740,7740),(7742,7742),(7744,7744),(7746,7746),(7748,7748),(7750,7750),(7752,7752),(7754,7754),(7756,7756),(7758,7758),(7760,7760),(7762,7762),(7764,7764),(7766,7766),(7768,7768),(7770,7770),(7772,7772),(7774,7774),(7776,7776),(7778,7778),(7780,7780),(7782,7782),(7784,7784),(7786,7786),(7788,7788),(7790,7790),(7792,7792),(7794,7794),(7796,7796),(7798,7798),(7800,7800),(7802,7802),(7804,7804),(7806,7806),(7808,7808),(7810,7810),(7812,7812),(7814,7814),(7816,7816),(7818,7818),(7820,7820),(7822,7822),(7824,7824),(7826,7826),(7828,7828),(7838,7838),(7840,7840),(7842,7842),(7844,7844),(7846,7846),(7848,7848),(7850,7850),(7852,7852),(7854,7854),(7856,7856),(7858,7858),(7860,7860),(7862,7862),(7864,7864),(7866,7866),(7868,7868),(7870,7870),(7872,7872),(7874,7874),(7876,7876),(7878,7878),(7880,7880),(7882,7882),(7884,7884),(7886,7886),(7888,7888),(7890,7890),(7892,7892),(7894,7894),(7896,7896),(7898,7898),(7900,7900),(7902,7902),(7904,7904),(7906,7906),(7908,7908),(7910,7910),(7912,7912),(7914,7914),(7916,7916),(7918,7918),(7920,7920),(7922,7922),(7924,7924),(7926,7926),(7928,7928),(7930,7930),(7932,7932),(7934,7934),(7944,7951),(7960,7965),(7976,7983),(7992,7999),(8008,8013),(8025,8025),(8027,8027),(8029,8029),(8031,8031),(8040,8047),(8120,8123),(8136,8139),(8152,8155),(8168,8172),(8184,8187),(8450,8450),(8455,8455),(8459,8461),(8464,8466),(8469,8469),(8473,8477),(8484,8484),(8486,8486),(8488,8488),(8490,8493),(8496,8499),(8510,8511),(8517,8517),(8579,8579),(11264,11310),(11360,11360),(11362,11364),(11367,11367),(11369,11369),(11371,11371),(11373,11376),(11378,11378),(11381,11381),(11390,11392),(11394,11394),(11396,11396),(11398,11398),(11400,11400),(11402,11402),(11404,11404),(11406,11406),(11408,11408),(11410,11410),(11412,11412),(11414,11414),(11416,11416),(11418,11418),(11420,11420),(11422,11422),(11424,11424),(11426,11426),(11428,11428),(11430,11430),(11432,11432),(11434,11434),(11436,11436),(11438,11438),(11440,11440),(11442,11442),(11444,11444),(11446,11446),(11448,11448),(11450,11450),(11452,11452),(11454,11454),(11456,11456),(11458,11458),(11460,11460),(11462,11462),(11464,11464),(11466,11466),(11468,11468),(11470,11470),(11472,11472),(11474,11474),(11476,11476),(11478,11478),(11480,11480),(11482,11482),(11484,11484),(11486,11486),(11488,11488),(11490,11490),(11499,11499),(11501,11501),(11506,11506),(42560,42560),(42562,42562),(42564,42564),(42566,42566),(42568,42568),(42570,42570),(42572,42572),(42574,42574),(42576,42576),(42578,42578),(42580,42580),(42582,42582),(42584,42584),(42586,42586),(42588,42588),(42590,42590),(42592,42592),(42594,42594),(42596,42596),(42598,42598),(42600,42600),(42602,42602),(42604,42604),(42624,42624),(42626,42626),(42628,42628),(42630,42630),(42632,42632),(42634,42634),(42636,42636),(42638,42638),(42640,42640),(42642,42642),(42644,42644),(42646,42646),(42648,42648),(42650,42650),(42786,42786),(42788,42788),(42790,42790),(42792,42792),(42794,42794),(42796,42796),(42798,42798),(42802,42802),(42804,42804),(42806,42806),(42808,42808),(42810,42810),(42812,42812),(42814,42814),(42816,42816),(42818,42818),(42820,42820),(42822,42822),(42824,42824),(42826,42826),(42828,42828),(42830,42830),(42832,42832),(42834,42834),(42836,42836),(42838,42838),(42840,42840),(42842,42842),(42844,42844),(42846,42846),(42848,42848),(42850,42850),(42852,42852),(42854,42854),(42856,42856),(42858,42858),(42860,42860),(42862,42862),(42873,42873),(42875,42875),(42877,42878),(42880,42880),(42882,42882),(42884,42884),(42886,42886),(42891,42891),(42893,42893),(42896,42896),(42898,42898),(42902,42902),(42904,42904),(42906,42906),(42908,42908),(42910,42910),(42912,42912),(42914,42914),(42916,42916),(42918,42918),(42920,42920),(42922,42925),(42928,42929),(65313,65338),(66560,66599),(71840,71871),(119808,119833),(119860,119885),(119912,119937),(119964,119964),(119966,119967),(119970,119970),(119973,119974),(119977,119980),(119982,119989),(120016,120041),(120068,120069),(120071,120074),(120077,120084),(120086,120092),(120120,120121),(120123,120126),(120128,120132),(120134,120134),(120138,120144),(120172,120197),(120224,120249),(120276,120301),(120328,120353),(120380,120405),(120432,120457),(120488,120512),(120546,120570),(120604,120628),(120662,120686),(120720,120744),(120778,120778)] then 4
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,122),(181,181),(223,246),(248,255),(257,257),(259,259),(261,261),(263,263),(265,265),(267,267),(269,269),(271,271),(273,273),(275,275),(277,277),(279,279),(281,281),(283,283),(285,285),(287,287),(289,289),(291,291),(293,293),(295,295),(297,297),(299,299),(301,301),(303,303),(305,305),(307,307),(309,309),(311,312),(314,314),(316,316),(318,318),(320,320),(322,322),(324,324),(326,326),(328,329),(331,331),(333,333),(335,335),(337,337),(339,339),(341,341),(343,343),(345,345),(347,347),(349,349),(351,351),(353,353),(355,355),(357,357),(359,359),(361,361),(363,363),(365,365),(367,367),(369,369),(371,371),(373,373),(375,375),(378,378),(380,380),(382,384),(387,387),(389,389),(392,392),(396,397),(402,402),(405,405),(409,411),(414,414),(417,417),(419,419),(421,421),(424,424),(426,427),(429,429),(432,432),(436,436),(438,438),(441,442),(445,447),(454,454),(457,457),(460,460),(462,462),(464,464),(466,466),(468,468),(470,470),(472,472),(474,474),(476,477),(479,479),(481,481),(483,483),(485,485),(487,487),(489,489),(491,491),(493,493),(495,496),(499,499),(501,501),(505,505),(507,507),(509,509),(511,511),(513,513),(515,515),(517,517),(519,519),(521,521),(523,523),(525,525),(527,527),(529,529),(531,531),(533,533),(535,535),(537,537),(539,539),(541,541),(543,543),(545,545),(547,547),(549,549),(551,551),(553,553),(555,555),(557,557),(559,559),(561,561),(563,569),(572,572),(575,576),(578,578),(583,583),(585,585),(587,587),(589,589),(591,659),(661,687),(881,881),(883,883),(887,887),(891,893),(912,912),(940,974),(976,977),(981,983),(985,985),(987,987),(989,989),(991,991),(993,993),(995,995),(997,997),(999,999),(1001,1001),(1003,1003),(1005,1005),(1007,1011),(1013,1013),(1016,1016),(1019,1020),(1072,1119),(1121,1121),(1123,1123),(1125,1125),(1127,1127),(1129,1129),(1131,1131),(1133,1133),(1135,1135),(1137,1137),(1139,1139),(1141,1141),(1143,1143),(1145,1145),(1147,1147),(1149,1149),(1151,1151),(1153,1153),(1163,1163),(1165,1165),(1167,1167),(1169,1169),(1171,1171),(1173,1173),(1175,1175),(1177,1177),(1179,1179),(1181,1181),(1183,1183),(1185,1185),(1187,1187),(1189,1189),(1191,1191),(1193,1193),(1195,1195),(1197,1197),(1199,1199),(1201,1201),(1203,1203),(1205,1205),(1207,1207),(1209,1209),(1211,1211),(1213,1213),(1215,1215),(1218,1218),(1220,1220),(1222,1222),(1224,1224),(1226,1226),(1228,1228),(1230,1231),(1233,1233),(1235,1235),(1237,1237),(1239,1239),(1241,1241),(1243,1243),(1245,1245),(1247,1247),(1249,1249),(1251,1251),(1253,1253),(1255,1255),(1257,1257),(1259,1259),(1261,1261),(1263,1263),(1265,1265),(1267,1267),(1269,1269),(1271,1271),(1273,1273),(1275,1275),(1277,1277),(1279,1279),(1281,1281),(1283,1283),(1285,1285),(1287,1287),(1289,1289),(1291,1291),(1293,1293),(1295,1295),(1297,1297),(1299,1299),(1301,1301),(1303,1303),(1305,1305),(1307,1307),(1309,1309),(1311,1311),(1313,1313),(1315,1315),(1317,1317),(1319,1319),(1321,1321),(1323,1323),(1325,1325),(1327,1327),(1377,1415),(7424,7467),(7531,7543),(7545,7578),(7681,7681),(7683,7683),(7685,7685),(7687,7687),(7689,7689),(7691,7691),(7693,7693),(7695,7695),(7697,7697),(7699,7699),(7701,7701),(7703,7703),(7705,7705),(7707,7707),(7709,7709),(7711,7711),(7713,7713),(7715,7715),(7717,7717),(7719,7719),(7721,7721),(7723,7723),(7725,7725),(7727,7727),(7729,7729),(7731,7731),(7733,7733),(7735,7735),(7737,7737),(7739,7739),(7741,7741),(7743,7743),(7745,7745),(7747,7747),(7749,7749),(7751,7751),(7753,7753),(7755,7755),(7757,7757),(7759,7759),(7761,7761),(7763,7763),(7765,7765),(7767,7767),(7769,7769),(7771,7771),(7773,7773),(7775,7775),(7777,7777),(7779,7779),(7781,7781),(7783,7783),(7785,7785),(7787,7787),(7789,7789),(7791,7791),(7793,7793),(7795,7795),(7797,7797),(7799,7799),(7801,7801),(7803,7803),(7805,7805),(7807,7807),(7809,7809),(7811,7811),(7813,7813),(7815,7815),(7817,7817),(7819,7819),(7821,7821),(7823,7823),(7825,7825),(7827,7827),(7829,7837),(7839,7839),(7841,7841),(7843,7843),(7845,7845),(7847,7847),(7849,7849),(7851,7851),(7853,7853),(7855,7855),(7857,7857),(7859,7859),(7861,7861),(7863,7863),(7865,7865),(7867,7867),(7869,7869),(7871,7871),(7873,7873),(7875,7875),(7877,7877),(7879,7879),(7881,7881),(7883,7883),(7885,7885),(7887,7887),(7889,7889),(7891,7891),(7893,7893),(7895,7895),(7897,7897),(7899,7899),(7901,7901),(7903,7903),(7905,7905),(7907,7907),(7909,7909),(7911,7911),(7913,7913),(7915,7915),(7917,7917),(7919,7919),(7921,7921),(7923,7923),(7925,7925),(7927,7927),(7929,7929),(7931,7931),(7933,7933),(7935,7943),(7952,7957),(7968,7975),(7984,7991),(8000,8005),(8016,8023),(8032,8039),(8048,8061),(8064,8071),(8080,8087),(8096,8103),(8112,8116),(8118,8119),(8126,8126),(8130,8132),(8134,8135),(8144,8147),(8150,8151),(8160,8167),(8178,8180),(8182,8183),(8458,8458),(8462,8463),(8467,8467),(8495,8495),(8500,8500),(8505,8505),(8508,8509),(8518,8521),(8526,8526),(8580,8580),(11312,11358),(11361,11361),(11365,11366),(11368,11368),(11370,11370),(11372,11372),(11377,11377),(11379,11380),(11382,11387),(11393,11393),(11395,11395),(11397,11397),(11399,11399),(11401,11401),(11403,11403),(11405,11405),(11407,11407),(11409,11409),(11411,11411),(11413,11413),(11415,11415),(11417,11417),(11419,11419),(11421,11421),(11423,11423),(11425,11425),(11427,11427),(11429,11429),(11431,11431),(11433,11433),(11435,11435),(11437,11437),(11439,11439),(11441,11441),(11443,11443),(11445,11445),(11447,11447),(11449,11449),(11451,11451),(11453,11453),(11455,11455),(11457,11457),(11459,11459),(11461,11461),(11463,11463),(11465,11465),(11467,11467),(11469,11469),(11471,11471),(11473,11473),(11475,11475),(11477,11477),(11479,11479),(11481,11481),(11483,11483),(11485,11485),(11487,11487),(11489,11489),(11491,11492),(11500,11500),(11502,11502),(11507,11507),(11520,11557),(11559,11559),(11565,11565),(42561,42561),(42563,42563),(42565,42565),(42567,42567),(42569,42569),(42571,42571),(42573,42573),(42575,42575),(42577,42577),(42579,42579),(42581,42581),(42583,42583),(42585,42585),(42587,42587),(42589,42589),(42591,42591),(42593,42593),(42595,42595),(42597,42597),(42599,42599),(42601,42601),(42603,42603),(42605,42605),(42625,42625),(42627,42627),(42629,42629),(42631,42631),(42633,42633),(42635,42635),(42637,42637),(42639,42639),(42641,42641),(42643,42643),(42645,42645),(42647,42647),(42649,42649),(42651,42651),(42787,42787),(42789,42789),(42791,42791),(42793,42793),(42795,42795),(42797,42797),(42799,42801),(42803,42803),(42805,42805),(42807,42807),(42809,42809),(42811,42811),(42813,42813),(42815,42815),(42817,42817),(42819,42819),(42821,42821),(42823,42823),(42825,42825),(42827,42827),(42829,42829),(42831,42831),(42833,42833),(42835,42835),(42837,42837),(42839,42839),(42841,42841),(42843,42843),(42845,42845),(42847,42847),(42849,42849),(42851,42851),(42853,42853),(42855,42855),(42857,42857),(42859,42859),(42861,42861),(42863,42863),(42865,42872),(42874,42874),(42876,42876),(42879,42879),(42881,42881),(42883,42883),(42885,42885),(42887,42887),(42892,42892),(42894,42894),(42897,42897),(42899,42901),(42903,42903),(42905,42905),(42907,42907),(42909,42909),(42911,42911),(42913,42913),(42915,42915),(42917,42917),(42919,42919),(42921,42921),(43002,43002),(43824,43866),(43876,43877),(64256,64262),(64275,64279),(65345,65370),(66600,66639),(71872,71903),(119834,119859),(119886,119892),(119894,119911),(119938,119963),(119990,119993),(119995,119995),(119997,120003),(120005,120015),(120042,120067),(120094,120119),(120146,120171),(120198,120223),(120250,120275),(120302,120327),(120354,120379),(120406,120431),(120458,120485),(120514,120538),(120540,120545),(120572,120596),(120598,120603),(120630,120654),(120656,120661),(120688,120712),(120714,120719),(120746,120770),(120772,120777),(120779,120779)] then 6
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(453,453),(456,456),(459,459),(498,498),(8072,8079),(8088,8095),(8104,8111),(8124,8124),(8140,8140),(8188,8188)] then 7
            else 0 in
    case (q, c') of
      (1, 4) -> 2
      (1, 5) -> 3
      (1, 6) -> 3
      (1, 7) -> 2
      (2, 1) -> 2
      (2, 2) -> 1
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
      (2, 7) -> 2
      (3, 1) -> 3
      (3, 3) -> 3
      (3, 4) -> 3
      (3, 5) -> 3
      (3, 6) -> 3
      (3, 7) -> 3
      _ -> 0

dfa51InitialState :: Int
dfa51InitialState = 1

dfa51FinalStates :: [Int]
dfa51FinalStates = [2]

dfa51Transition :: Int -> Char -> Int
dfa51Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          39 -> 1
          46 -> 2
          95 -> 5
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 3
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(65,90),(192,214),(216,222),(256,256),(258,258),(260,260),(262,262),(264,264),(266,266),(268,268),(270,270),(272,272),(274,274),(276,276),(278,278),(280,280),(282,282),(284,284),(286,286),(288,288),(290,290),(292,292),(294,294),(296,296),(298,298),(300,300),(302,302),(304,304),(306,306),(308,308),(310,310),(313,313),(315,315),(317,317),(319,319),(321,321),(323,323),(325,325),(327,327),(330,330),(332,332),(334,334),(336,336),(338,338),(340,340),(342,342),(344,344),(346,346),(348,348),(350,350),(352,352),(354,354),(356,356),(358,358),(360,360),(362,362),(364,364),(366,366),(368,368),(370,370),(372,372),(374,374),(376,377),(379,379),(381,381),(385,386),(388,388),(390,391),(393,395),(398,401),(403,404),(406,408),(412,413),(415,416),(418,418),(420,420),(422,423),(425,425),(428,428),(430,431),(433,435),(437,437),(439,440),(444,444),(452,452),(455,455),(458,458),(461,461),(463,463),(465,465),(467,467),(469,469),(471,471),(473,473),(475,475),(478,478),(480,480),(482,482),(484,484),(486,486),(488,488),(490,490),(492,492),(494,494),(497,497),(500,500),(502,504),(506,506),(508,508),(510,510),(512,512),(514,514),(516,516),(518,518),(520,520),(522,522),(524,524),(526,526),(528,528),(530,530),(532,532),(534,534),(536,536),(538,538),(540,540),(542,542),(544,544),(546,546),(548,548),(550,550),(552,552),(554,554),(556,556),(558,558),(560,560),(562,562),(570,571),(573,574),(577,577),(579,582),(584,584),(586,586),(588,588),(590,590),(880,880),(882,882),(886,886),(895,895),(902,902),(904,906),(908,908),(910,911),(913,929),(931,939),(975,975),(978,980),(984,984),(986,986),(988,988),(990,990),(992,992),(994,994),(996,996),(998,998),(1000,1000),(1002,1002),(1004,1004),(1006,1006),(1012,1012),(1015,1015),(1017,1018),(1021,1071),(1120,1120),(1122,1122),(1124,1124),(1126,1126),(1128,1128),(1130,1130),(1132,1132),(1134,1134),(1136,1136),(1138,1138),(1140,1140),(1142,1142),(1144,1144),(1146,1146),(1148,1148),(1150,1150),(1152,1152),(1162,1162),(1164,1164),(1166,1166),(1168,1168),(1170,1170),(1172,1172),(1174,1174),(1176,1176),(1178,1178),(1180,1180),(1182,1182),(1184,1184),(1186,1186),(1188,1188),(1190,1190),(1192,1192),(1194,1194),(1196,1196),(1198,1198),(1200,1200),(1202,1202),(1204,1204),(1206,1206),(1208,1208),(1210,1210),(1212,1212),(1214,1214),(1216,1217),(1219,1219),(1221,1221),(1223,1223),(1225,1225),(1227,1227),(1229,1229),(1232,1232),(1234,1234),(1236,1236),(1238,1238),(1240,1240),(1242,1242),(1244,1244),(1246,1246),(1248,1248),(1250,1250),(1252,1252),(1254,1254),(1256,1256),(1258,1258),(1260,1260),(1262,1262),(1264,1264),(1266,1266),(1268,1268),(1270,1270),(1272,1272),(1274,1274),(1276,1276),(1278,1278),(1280,1280),(1282,1282),(1284,1284),(1286,1286),(1288,1288),(1290,1290),(1292,1292),(1294,1294),(1296,1296),(1298,1298),(1300,1300),(1302,1302),(1304,1304),(1306,1306),(1308,1308),(1310,1310),(1312,1312),(1314,1314),(1316,1316),(1318,1318),(1320,1320),(1322,1322),(1324,1324),(1326,1326),(1329,1366),(4256,4293),(4295,4295),(4301,4301),(7680,7680),(7682,7682),(7684,7684),(7686,7686),(7688,7688),(7690,7690),(7692,7692),(7694,7694),(7696,7696),(7698,7698),(7700,7700),(7702,7702),(7704,7704),(7706,7706),(7708,7708),(7710,7710),(7712,7712),(7714,7714),(7716,7716),(7718,7718),(7720,7720),(7722,7722),(7724,7724),(7726,7726),(7728,7728),(7730,7730),(7732,7732),(7734,7734),(7736,7736),(7738,7738),(7740,7740),(7742,7742),(7744,7744),(7746,7746),(7748,7748),(7750,7750),(7752,7752),(7754,7754),(7756,7756),(7758,7758),(7760,7760),(7762,7762),(7764,7764),(7766,7766),(7768,7768),(7770,7770),(7772,7772),(7774,7774),(7776,7776),(7778,7778),(7780,7780),(7782,7782),(7784,7784),(7786,7786),(7788,7788),(7790,7790),(7792,7792),(7794,7794),(7796,7796),(7798,7798),(7800,7800),(7802,7802),(7804,7804),(7806,7806),(7808,7808),(7810,7810),(7812,7812),(7814,7814),(7816,7816),(7818,7818),(7820,7820),(7822,7822),(7824,7824),(7826,7826),(7828,7828),(7838,7838),(7840,7840),(7842,7842),(7844,7844),(7846,7846),(7848,7848),(7850,7850),(7852,7852),(7854,7854),(7856,7856),(7858,7858),(7860,7860),(7862,7862),(7864,7864),(7866,7866),(7868,7868),(7870,7870),(7872,7872),(7874,7874),(7876,7876),(7878,7878),(7880,7880),(7882,7882),(7884,7884),(7886,7886),(7888,7888),(7890,7890),(7892,7892),(7894,7894),(7896,7896),(7898,7898),(7900,7900),(7902,7902),(7904,7904),(7906,7906),(7908,7908),(7910,7910),(7912,7912),(7914,7914),(7916,7916),(7918,7918),(7920,7920),(7922,7922),(7924,7924),(7926,7926),(7928,7928),(7930,7930),(7932,7932),(7934,7934),(7944,7951),(7960,7965),(7976,7983),(7992,7999),(8008,8013),(8025,8025),(8027,8027),(8029,8029),(8031,8031),(8040,8047),(8120,8123),(8136,8139),(8152,8155),(8168,8172),(8184,8187),(8450,8450),(8455,8455),(8459,8461),(8464,8466),(8469,8469),(8473,8477),(8484,8484),(8486,8486),(8488,8488),(8490,8493),(8496,8499),(8510,8511),(8517,8517),(8579,8579),(11264,11310),(11360,11360),(11362,11364),(11367,11367),(11369,11369),(11371,11371),(11373,11376),(11378,11378),(11381,11381),(11390,11392),(11394,11394),(11396,11396),(11398,11398),(11400,11400),(11402,11402),(11404,11404),(11406,11406),(11408,11408),(11410,11410),(11412,11412),(11414,11414),(11416,11416),(11418,11418),(11420,11420),(11422,11422),(11424,11424),(11426,11426),(11428,11428),(11430,11430),(11432,11432),(11434,11434),(11436,11436),(11438,11438),(11440,11440),(11442,11442),(11444,11444),(11446,11446),(11448,11448),(11450,11450),(11452,11452),(11454,11454),(11456,11456),(11458,11458),(11460,11460),(11462,11462),(11464,11464),(11466,11466),(11468,11468),(11470,11470),(11472,11472),(11474,11474),(11476,11476),(11478,11478),(11480,11480),(11482,11482),(11484,11484),(11486,11486),(11488,11488),(11490,11490),(11499,11499),(11501,11501),(11506,11506),(42560,42560),(42562,42562),(42564,42564),(42566,42566),(42568,42568),(42570,42570),(42572,42572),(42574,42574),(42576,42576),(42578,42578),(42580,42580),(42582,42582),(42584,42584),(42586,42586),(42588,42588),(42590,42590),(42592,42592),(42594,42594),(42596,42596),(42598,42598),(42600,42600),(42602,42602),(42604,42604),(42624,42624),(42626,42626),(42628,42628),(42630,42630),(42632,42632),(42634,42634),(42636,42636),(42638,42638),(42640,42640),(42642,42642),(42644,42644),(42646,42646),(42648,42648),(42650,42650),(42786,42786),(42788,42788),(42790,42790),(42792,42792),(42794,42794),(42796,42796),(42798,42798),(42802,42802),(42804,42804),(42806,42806),(42808,42808),(42810,42810),(42812,42812),(42814,42814),(42816,42816),(42818,42818),(42820,42820),(42822,42822),(42824,42824),(42826,42826),(42828,42828),(42830,42830),(42832,42832),(42834,42834),(42836,42836),(42838,42838),(42840,42840),(42842,42842),(42844,42844),(42846,42846),(42848,42848),(42850,42850),(42852,42852),(42854,42854),(42856,42856),(42858,42858),(42860,42860),(42862,42862),(42873,42873),(42875,42875),(42877,42878),(42880,42880),(42882,42882),(42884,42884),(42886,42886),(42891,42891),(42893,42893),(42896,42896),(42898,42898),(42902,42902),(42904,42904),(42906,42906),(42908,42908),(42910,42910),(42912,42912),(42914,42914),(42916,42916),(42918,42918),(42920,42920),(42922,42925),(42928,42929),(65313,65338),(66560,66599),(71840,71871),(119808,119833),(119860,119885),(119912,119937),(119964,119964),(119966,119967),(119970,119970),(119973,119974),(119977,119980),(119982,119989),(120016,120041),(120068,120069),(120071,120074),(120077,120084),(120086,120092),(120120,120121),(120123,120126),(120128,120132),(120134,120134),(120138,120144),(120172,120197),(120224,120249),(120276,120301),(120328,120353),(120380,120405),(120432,120457),(120488,120512),(120546,120570),(120604,120628),(120662,120686),(120720,120744),(120778,120778)] then 4
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,122),(181,181),(223,246),(248,255),(257,257),(259,259),(261,261),(263,263),(265,265),(267,267),(269,269),(271,271),(273,273),(275,275),(277,277),(279,279),(281,281),(283,283),(285,285),(287,287),(289,289),(291,291),(293,293),(295,295),(297,297),(299,299),(301,301),(303,303),(305,305),(307,307),(309,309),(311,312),(314,314),(316,316),(318,318),(320,320),(322,322),(324,324),(326,326),(328,329),(331,331),(333,333),(335,335),(337,337),(339,339),(341,341),(343,343),(345,345),(347,347),(349,349),(351,351),(353,353),(355,355),(357,357),(359,359),(361,361),(363,363),(365,365),(367,367),(369,369),(371,371),(373,373),(375,375),(378,378),(380,380),(382,384),(387,387),(389,389),(392,392),(396,397),(402,402),(405,405),(409,411),(414,414),(417,417),(419,419),(421,421),(424,424),(426,427),(429,429),(432,432),(436,436),(438,438),(441,442),(445,447),(454,454),(457,457),(460,460),(462,462),(464,464),(466,466),(468,468),(470,470),(472,472),(474,474),(476,477),(479,479),(481,481),(483,483),(485,485),(487,487),(489,489),(491,491),(493,493),(495,496),(499,499),(501,501),(505,505),(507,507),(509,509),(511,511),(513,513),(515,515),(517,517),(519,519),(521,521),(523,523),(525,525),(527,527),(529,529),(531,531),(533,533),(535,535),(537,537),(539,539),(541,541),(543,543),(545,545),(547,547),(549,549),(551,551),(553,553),(555,555),(557,557),(559,559),(561,561),(563,569),(572,572),(575,576),(578,578),(583,583),(585,585),(587,587),(589,589),(591,659),(661,687),(881,881),(883,883),(887,887),(891,893),(912,912),(940,974),(976,977),(981,983),(985,985),(987,987),(989,989),(991,991),(993,993),(995,995),(997,997),(999,999),(1001,1001),(1003,1003),(1005,1005),(1007,1011),(1013,1013),(1016,1016),(1019,1020),(1072,1119),(1121,1121),(1123,1123),(1125,1125),(1127,1127),(1129,1129),(1131,1131),(1133,1133),(1135,1135),(1137,1137),(1139,1139),(1141,1141),(1143,1143),(1145,1145),(1147,1147),(1149,1149),(1151,1151),(1153,1153),(1163,1163),(1165,1165),(1167,1167),(1169,1169),(1171,1171),(1173,1173),(1175,1175),(1177,1177),(1179,1179),(1181,1181),(1183,1183),(1185,1185),(1187,1187),(1189,1189),(1191,1191),(1193,1193),(1195,1195),(1197,1197),(1199,1199),(1201,1201),(1203,1203),(1205,1205),(1207,1207),(1209,1209),(1211,1211),(1213,1213),(1215,1215),(1218,1218),(1220,1220),(1222,1222),(1224,1224),(1226,1226),(1228,1228),(1230,1231),(1233,1233),(1235,1235),(1237,1237),(1239,1239),(1241,1241),(1243,1243),(1245,1245),(1247,1247),(1249,1249),(1251,1251),(1253,1253),(1255,1255),(1257,1257),(1259,1259),(1261,1261),(1263,1263),(1265,1265),(1267,1267),(1269,1269),(1271,1271),(1273,1273),(1275,1275),(1277,1277),(1279,1279),(1281,1281),(1283,1283),(1285,1285),(1287,1287),(1289,1289),(1291,1291),(1293,1293),(1295,1295),(1297,1297),(1299,1299),(1301,1301),(1303,1303),(1305,1305),(1307,1307),(1309,1309),(1311,1311),(1313,1313),(1315,1315),(1317,1317),(1319,1319),(1321,1321),(1323,1323),(1325,1325),(1327,1327),(1377,1415),(7424,7467),(7531,7543),(7545,7578),(7681,7681),(7683,7683),(7685,7685),(7687,7687),(7689,7689),(7691,7691),(7693,7693),(7695,7695),(7697,7697),(7699,7699),(7701,7701),(7703,7703),(7705,7705),(7707,7707),(7709,7709),(7711,7711),(7713,7713),(7715,7715),(7717,7717),(7719,7719),(7721,7721),(7723,7723),(7725,7725),(7727,7727),(7729,7729),(7731,7731),(7733,7733),(7735,7735),(7737,7737),(7739,7739),(7741,7741),(7743,7743),(7745,7745),(7747,7747),(7749,7749),(7751,7751),(7753,7753),(7755,7755),(7757,7757),(7759,7759),(7761,7761),(7763,7763),(7765,7765),(7767,7767),(7769,7769),(7771,7771),(7773,7773),(7775,7775),(7777,7777),(7779,7779),(7781,7781),(7783,7783),(7785,7785),(7787,7787),(7789,7789),(7791,7791),(7793,7793),(7795,7795),(7797,7797),(7799,7799),(7801,7801),(7803,7803),(7805,7805),(7807,7807),(7809,7809),(7811,7811),(7813,7813),(7815,7815),(7817,7817),(7819,7819),(7821,7821),(7823,7823),(7825,7825),(7827,7827),(7829,7837),(7839,7839),(7841,7841),(7843,7843),(7845,7845),(7847,7847),(7849,7849),(7851,7851),(7853,7853),(7855,7855),(7857,7857),(7859,7859),(7861,7861),(7863,7863),(7865,7865),(7867,7867),(7869,7869),(7871,7871),(7873,7873),(7875,7875),(7877,7877),(7879,7879),(7881,7881),(7883,7883),(7885,7885),(7887,7887),(7889,7889),(7891,7891),(7893,7893),(7895,7895),(7897,7897),(7899,7899),(7901,7901),(7903,7903),(7905,7905),(7907,7907),(7909,7909),(7911,7911),(7913,7913),(7915,7915),(7917,7917),(7919,7919),(7921,7921),(7923,7923),(7925,7925),(7927,7927),(7929,7929),(7931,7931),(7933,7933),(7935,7943),(7952,7957),(7968,7975),(7984,7991),(8000,8005),(8016,8023),(8032,8039),(8048,8061),(8064,8071),(8080,8087),(8096,8103),(8112,8116),(8118,8119),(8126,8126),(8130,8132),(8134,8135),(8144,8147),(8150,8151),(8160,8167),(8178,8180),(8182,8183),(8458,8458),(8462,8463),(8467,8467),(8495,8495),(8500,8500),(8505,8505),(8508,8509),(8518,8521),(8526,8526),(8580,8580),(11312,11358),(11361,11361),(11365,11366),(11368,11368),(11370,11370),(11372,11372),(11377,11377),(11379,11380),(11382,11387),(11393,11393),(11395,11395),(11397,11397),(11399,11399),(11401,11401),(11403,11403),(11405,11405),(11407,11407),(11409,11409),(11411,11411),(11413,11413),(11415,11415),(11417,11417),(11419,11419),(11421,11421),(11423,11423),(11425,11425),(11427,11427),(11429,11429),(11431,11431),(11433,11433),(11435,11435),(11437,11437),(11439,11439),(11441,11441),(11443,11443),(11445,11445),(11447,11447),(11449,11449),(11451,11451),(11453,11453),(11455,11455),(11457,11457),(11459,11459),(11461,11461),(11463,11463),(11465,11465),(11467,11467),(11469,11469),(11471,11471),(11473,11473),(11475,11475),(11477,11477),(11479,11479),(11481,11481),(11483,11483),(11485,11485),(11487,11487),(11489,11489),(11491,11492),(11500,11500),(11502,11502),(11507,11507),(11520,11557),(11559,11559),(11565,11565),(42561,42561),(42563,42563),(42565,42565),(42567,42567),(42569,42569),(42571,42571),(42573,42573),(42575,42575),(42577,42577),(42579,42579),(42581,42581),(42583,42583),(42585,42585),(42587,42587),(42589,42589),(42591,42591),(42593,42593),(42595,42595),(42597,42597),(42599,42599),(42601,42601),(42603,42603),(42605,42605),(42625,42625),(42627,42627),(42629,42629),(42631,42631),(42633,42633),(42635,42635),(42637,42637),(42639,42639),(42641,42641),(42643,42643),(42645,42645),(42647,42647),(42649,42649),(42651,42651),(42787,42787),(42789,42789),(42791,42791),(42793,42793),(42795,42795),(42797,42797),(42799,42801),(42803,42803),(42805,42805),(42807,42807),(42809,42809),(42811,42811),(42813,42813),(42815,42815),(42817,42817),(42819,42819),(42821,42821),(42823,42823),(42825,42825),(42827,42827),(42829,42829),(42831,42831),(42833,42833),(42835,42835),(42837,42837),(42839,42839),(42841,42841),(42843,42843),(42845,42845),(42847,42847),(42849,42849),(42851,42851),(42853,42853),(42855,42855),(42857,42857),(42859,42859),(42861,42861),(42863,42863),(42865,42872),(42874,42874),(42876,42876),(42879,42879),(42881,42881),(42883,42883),(42885,42885),(42887,42887),(42892,42892),(42894,42894),(42897,42897),(42899,42901),(42903,42903),(42905,42905),(42907,42907),(42909,42909),(42911,42911),(42913,42913),(42915,42915),(42917,42917),(42919,42919),(42921,42921),(43002,43002),(43824,43866),(43876,43877),(64256,64262),(64275,64279),(65345,65370),(66600,66639),(71872,71903),(119834,119859),(119886,119892),(119894,119911),(119938,119963),(119990,119993),(119995,119995),(119997,120003),(120005,120015),(120042,120067),(120094,120119),(120146,120171),(120198,120223),(120250,120275),(120302,120327),(120354,120379),(120406,120431),(120458,120485),(120514,120538),(120540,120545),(120572,120596),(120598,120603),(120630,120654),(120656,120661),(120688,120712),(120714,120719),(120746,120770),(120772,120777),(120779,120779)] then 6
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(453,453),(456,456),(459,459),(498,498),(8072,8079),(8088,8095),(8104,8111),(8124,8124),(8140,8140),(8188,8188)] then 7
            else 0 in
    case (q, c') of
      (1, 4) -> 2
      (1, 7) -> 2
      (2, 1) -> 2
      (2, 2) -> 1
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
      (2, 7) -> 2
      _ -> 0

dfa52InitialState :: Int
dfa52InitialState = 1

dfa52FinalStates :: [Int]
dfa52FinalStates = [2]

dfa52Transition :: Int -> Char -> Int
dfa52Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          33 -> 1
          35 -> 2
          36 -> 3
          37 -> 4
          38 -> 5
          39 -> 6
          42 -> 7
          43 -> 8
          45 -> 9
          46 -> 10
          47 -> 11
          58 -> 13
          60 -> 14
          61 -> 15
          62 -> 16
          63 -> 17
          64 -> 18
          92 -> 20
          94 -> 21
          95 -> 22
          124 -> 24
          126 -> 25
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 12
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(65,90),(192,214),(216,222),(256,256),(258,258),(260,260),(262,262),(264,264),(266,266),(268,268),(270,270),(272,272),(274,274),(276,276),(278,278),(280,280),(282,282),(284,284),(286,286),(288,288),(290,290),(292,292),(294,294),(296,296),(298,298),(300,300),(302,302),(304,304),(306,306),(308,308),(310,310),(313,313),(315,315),(317,317),(319,319),(321,321),(323,323),(325,325),(327,327),(330,330),(332,332),(334,334),(336,336),(338,338),(340,340),(342,342),(344,344),(346,346),(348,348),(350,350),(352,352),(354,354),(356,356),(358,358),(360,360),(362,362),(364,364),(366,366),(368,368),(370,370),(372,372),(374,374),(376,377),(379,379),(381,381),(385,386),(388,388),(390,391),(393,395),(398,401),(403,404),(406,408),(412,413),(415,416),(418,418),(420,420),(422,423),(425,425),(428,428),(430,431),(433,435),(437,437),(439,440),(444,444),(452,452),(455,455),(458,458),(461,461),(463,463),(465,465),(467,467),(469,469),(471,471),(473,473),(475,475),(478,478),(480,480),(482,482),(484,484),(486,486),(488,488),(490,490),(492,492),(494,494),(497,497),(500,500),(502,504),(506,506),(508,508),(510,510),(512,512),(514,514),(516,516),(518,518),(520,520),(522,522),(524,524),(526,526),(528,528),(530,530),(532,532),(534,534),(536,536),(538,538),(540,540),(542,542),(544,544),(546,546),(548,548),(550,550),(552,552),(554,554),(556,556),(558,558),(560,560),(562,562),(570,571),(573,574),(577,577),(579,582),(584,584),(586,586),(588,588),(590,590),(880,880),(882,882),(886,886),(895,895),(902,902),(904,906),(908,908),(910,911),(913,929),(931,939),(975,975),(978,980),(984,984),(986,986),(988,988),(990,990),(992,992),(994,994),(996,996),(998,998),(1000,1000),(1002,1002),(1004,1004),(1006,1006),(1012,1012),(1015,1015),(1017,1018),(1021,1071),(1120,1120),(1122,1122),(1124,1124),(1126,1126),(1128,1128),(1130,1130),(1132,1132),(1134,1134),(1136,1136),(1138,1138),(1140,1140),(1142,1142),(1144,1144),(1146,1146),(1148,1148),(1150,1150),(1152,1152),(1162,1162),(1164,1164),(1166,1166),(1168,1168),(1170,1170),(1172,1172),(1174,1174),(1176,1176),(1178,1178),(1180,1180),(1182,1182),(1184,1184),(1186,1186),(1188,1188),(1190,1190),(1192,1192),(1194,1194),(1196,1196),(1198,1198),(1200,1200),(1202,1202),(1204,1204),(1206,1206),(1208,1208),(1210,1210),(1212,1212),(1214,1214),(1216,1217),(1219,1219),(1221,1221),(1223,1223),(1225,1225),(1227,1227),(1229,1229),(1232,1232),(1234,1234),(1236,1236),(1238,1238),(1240,1240),(1242,1242),(1244,1244),(1246,1246),(1248,1248),(1250,1250),(1252,1252),(1254,1254),(1256,1256),(1258,1258),(1260,1260),(1262,1262),(1264,1264),(1266,1266),(1268,1268),(1270,1270),(1272,1272),(1274,1274),(1276,1276),(1278,1278),(1280,1280),(1282,1282),(1284,1284),(1286,1286),(1288,1288),(1290,1290),(1292,1292),(1294,1294),(1296,1296),(1298,1298),(1300,1300),(1302,1302),(1304,1304),(1306,1306),(1308,1308),(1310,1310),(1312,1312),(1314,1314),(1316,1316),(1318,1318),(1320,1320),(1322,1322),(1324,1324),(1326,1326),(1329,1366),(4256,4293),(4295,4295),(4301,4301),(7680,7680),(7682,7682),(7684,7684),(7686,7686),(7688,7688),(7690,7690),(7692,7692),(7694,7694),(7696,7696),(7698,7698),(7700,7700),(7702,7702),(7704,7704),(7706,7706),(7708,7708),(7710,7710),(7712,7712),(7714,7714),(7716,7716),(7718,7718),(7720,7720),(7722,7722),(7724,7724),(7726,7726),(7728,7728),(7730,7730),(7732,7732),(7734,7734),(7736,7736),(7738,7738),(7740,7740),(7742,7742),(7744,7744),(7746,7746),(7748,7748),(7750,7750),(7752,7752),(7754,7754),(7756,7756),(7758,7758),(7760,7760),(7762,7762),(7764,7764),(7766,7766),(7768,7768),(7770,7770),(7772,7772),(7774,7774),(7776,7776),(7778,7778),(7780,7780),(7782,7782),(7784,7784),(7786,7786),(7788,7788),(7790,7790),(7792,7792),(7794,7794),(7796,7796),(7798,7798),(7800,7800),(7802,7802),(7804,7804),(7806,7806),(7808,7808),(7810,7810),(7812,7812),(7814,7814),(7816,7816),(7818,7818),(7820,7820),(7822,7822),(7824,7824),(7826,7826),(7828,7828),(7838,7838),(7840,7840),(7842,7842),(7844,7844),(7846,7846),(7848,7848),(7850,7850),(7852,7852),(7854,7854),(7856,7856),(7858,7858),(7860,7860),(7862,7862),(7864,7864),(7866,7866),(7868,7868),(7870,7870),(7872,7872),(7874,7874),(7876,7876),(7878,7878),(7880,7880),(7882,7882),(7884,7884),(7886,7886),(7888,7888),(7890,7890),(7892,7892),(7894,7894),(7896,7896),(7898,7898),(7900,7900),(7902,7902),(7904,7904),(7906,7906),(7908,7908),(7910,7910),(7912,7912),(7914,7914),(7916,7916),(7918,7918),(7920,7920),(7922,7922),(7924,7924),(7926,7926),(7928,7928),(7930,7930),(7932,7932),(7934,7934),(7944,7951),(7960,7965),(7976,7983),(7992,7999),(8008,8013),(8025,8025),(8027,8027),(8029,8029),(8031,8031),(8040,8047),(8120,8123),(8136,8139),(8152,8155),(8168,8172),(8184,8187),(8450,8450),(8455,8455),(8459,8461),(8464,8466),(8469,8469),(8473,8477),(8484,8484),(8486,8486),(8488,8488),(8490,8493),(8496,8499),(8510,8511),(8517,8517),(8579,8579),(11264,11310),(11360,11360),(11362,11364),(11367,11367),(11369,11369),(11371,11371),(11373,11376),(11378,11378),(11381,11381),(11390,11392),(11394,11394),(11396,11396),(11398,11398),(11400,11400),(11402,11402),(11404,11404),(11406,11406),(11408,11408),(11410,11410),(11412,11412),(11414,11414),(11416,11416),(11418,11418),(11420,11420),(11422,11422),(11424,11424),(11426,11426),(11428,11428),(11430,11430),(11432,11432),(11434,11434),(11436,11436),(11438,11438),(11440,11440),(11442,11442),(11444,11444),(11446,11446),(11448,11448),(11450,11450),(11452,11452),(11454,11454),(11456,11456),(11458,11458),(11460,11460),(11462,11462),(11464,11464),(11466,11466),(11468,11468),(11470,11470),(11472,11472),(11474,11474),(11476,11476),(11478,11478),(11480,11480),(11482,11482),(11484,11484),(11486,11486),(11488,11488),(11490,11490),(11499,11499),(11501,11501),(11506,11506),(42560,42560),(42562,42562),(42564,42564),(42566,42566),(42568,42568),(42570,42570),(42572,42572),(42574,42574),(42576,42576),(42578,42578),(42580,42580),(42582,42582),(42584,42584),(42586,42586),(42588,42588),(42590,42590),(42592,42592),(42594,42594),(42596,42596),(42598,42598),(42600,42600),(42602,42602),(42604,42604),(42624,42624),(42626,42626),(42628,42628),(42630,42630),(42632,42632),(42634,42634),(42636,42636),(42638,42638),(42640,42640),(42642,42642),(42644,42644),(42646,42646),(42648,42648),(42650,42650),(42786,42786),(42788,42788),(42790,42790),(42792,42792),(42794,42794),(42796,42796),(42798,42798),(42802,42802),(42804,42804),(42806,42806),(42808,42808),(42810,42810),(42812,42812),(42814,42814),(42816,42816),(42818,42818),(42820,42820),(42822,42822),(42824,42824),(42826,42826),(42828,42828),(42830,42830),(42832,42832),(42834,42834),(42836,42836),(42838,42838),(42840,42840),(42842,42842),(42844,42844),(42846,42846),(42848,42848),(42850,42850),(42852,42852),(42854,42854),(42856,42856),(42858,42858),(42860,42860),(42862,42862),(42873,42873),(42875,42875),(42877,42878),(42880,42880),(42882,42882),(42884,42884),(42886,42886),(42891,42891),(42893,42893),(42896,42896),(42898,42898),(42902,42902),(42904,42904),(42906,42906),(42908,42908),(42910,42910),(42912,42912),(42914,42914),(42916,42916),(42918,42918),(42920,42920),(42922,42925),(42928,42929),(65313,65338),(66560,66599),(71840,71871),(119808,119833),(119860,119885),(119912,119937),(119964,119964),(119966,119967),(119970,119970),(119973,119974),(119977,119980),(119982,119989),(120016,120041),(120068,120069),(120071,120074),(120077,120084),(120086,120092),(120120,120121),(120123,120126),(120128,120132),(120134,120134),(120138,120144),(120172,120197),(120224,120249),(120276,120301),(120328,120353),(120380,120405),(120432,120457),(120488,120512),(120546,120570),(120604,120628),(120662,120686),(120720,120744),(120778,120778)] then 19
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,122),(181,181),(223,246),(248,255),(257,257),(259,259),(261,261),(263,263),(265,265),(267,267),(269,269),(271,271),(273,273),(275,275),(277,277),(279,279),(281,281),(283,283),(285,285),(287,287),(289,289),(291,291),(293,293),(295,295),(297,297),(299,299),(301,301),(303,303),(305,305),(307,307),(309,309),(311,312),(314,314),(316,316),(318,318),(320,320),(322,322),(324,324),(326,326),(328,329),(331,331),(333,333),(335,335),(337,337),(339,339),(341,341),(343,343),(345,345),(347,347),(349,349),(351,351),(353,353),(355,355),(357,357),(359,359),(361,361),(363,363),(365,365),(367,367),(369,369),(371,371),(373,373),(375,375),(378,378),(380,380),(382,384),(387,387),(389,389),(392,392),(396,397),(402,402),(405,405),(409,411),(414,414),(417,417),(419,419),(421,421),(424,424),(426,427),(429,429),(432,432),(436,436),(438,438),(441,442),(445,447),(454,454),(457,457),(460,460),(462,462),(464,464),(466,466),(468,468),(470,470),(472,472),(474,474),(476,477),(479,479),(481,481),(483,483),(485,485),(487,487),(489,489),(491,491),(493,493),(495,496),(499,499),(501,501),(505,505),(507,507),(509,509),(511,511),(513,513),(515,515),(517,517),(519,519),(521,521),(523,523),(525,525),(527,527),(529,529),(531,531),(533,533),(535,535),(537,537),(539,539),(541,541),(543,543),(545,545),(547,547),(549,549),(551,551),(553,553),(555,555),(557,557),(559,559),(561,561),(563,569),(572,572),(575,576),(578,578),(583,583),(585,585),(587,587),(589,589),(591,659),(661,687),(881,881),(883,883),(887,887),(891,893),(912,912),(940,974),(976,977),(981,983),(985,985),(987,987),(989,989),(991,991),(993,993),(995,995),(997,997),(999,999),(1001,1001),(1003,1003),(1005,1005),(1007,1011),(1013,1013),(1016,1016),(1019,1020),(1072,1119),(1121,1121),(1123,1123),(1125,1125),(1127,1127),(1129,1129),(1131,1131),(1133,1133),(1135,1135),(1137,1137),(1139,1139),(1141,1141),(1143,1143),(1145,1145),(1147,1147),(1149,1149),(1151,1151),(1153,1153),(1163,1163),(1165,1165),(1167,1167),(1169,1169),(1171,1171),(1173,1173),(1175,1175),(1177,1177),(1179,1179),(1181,1181),(1183,1183),(1185,1185),(1187,1187),(1189,1189),(1191,1191),(1193,1193),(1195,1195),(1197,1197),(1199,1199),(1201,1201),(1203,1203),(1205,1205),(1207,1207),(1209,1209),(1211,1211),(1213,1213),(1215,1215),(1218,1218),(1220,1220),(1222,1222),(1224,1224),(1226,1226),(1228,1228),(1230,1231),(1233,1233),(1235,1235),(1237,1237),(1239,1239),(1241,1241),(1243,1243),(1245,1245),(1247,1247),(1249,1249),(1251,1251),(1253,1253),(1255,1255),(1257,1257),(1259,1259),(1261,1261),(1263,1263),(1265,1265),(1267,1267),(1269,1269),(1271,1271),(1273,1273),(1275,1275),(1277,1277),(1279,1279),(1281,1281),(1283,1283),(1285,1285),(1287,1287),(1289,1289),(1291,1291),(1293,1293),(1295,1295),(1297,1297),(1299,1299),(1301,1301),(1303,1303),(1305,1305),(1307,1307),(1309,1309),(1311,1311),(1313,1313),(1315,1315),(1317,1317),(1319,1319),(1321,1321),(1323,1323),(1325,1325),(1327,1327),(1377,1415),(7424,7467),(7531,7543),(7545,7578),(7681,7681),(7683,7683),(7685,7685),(7687,7687),(7689,7689),(7691,7691),(7693,7693),(7695,7695),(7697,7697),(7699,7699),(7701,7701),(7703,7703),(7705,7705),(7707,7707),(7709,7709),(7711,7711),(7713,7713),(7715,7715),(7717,7717),(7719,7719),(7721,7721),(7723,7723),(7725,7725),(7727,7727),(7729,7729),(7731,7731),(7733,7733),(7735,7735),(7737,7737),(7739,7739),(7741,7741),(7743,7743),(7745,7745),(7747,7747),(7749,7749),(7751,7751),(7753,7753),(7755,7755),(7757,7757),(7759,7759),(7761,7761),(7763,7763),(7765,7765),(7767,7767),(7769,7769),(7771,7771),(7773,7773),(7775,7775),(7777,7777),(7779,7779),(7781,7781),(7783,7783),(7785,7785),(7787,7787),(7789,7789),(7791,7791),(7793,7793),(7795,7795),(7797,7797),(7799,7799),(7801,7801),(7803,7803),(7805,7805),(7807,7807),(7809,7809),(7811,7811),(7813,7813),(7815,7815),(7817,7817),(7819,7819),(7821,7821),(7823,7823),(7825,7825),(7827,7827),(7829,7837),(7839,7839),(7841,7841),(7843,7843),(7845,7845),(7847,7847),(7849,7849),(7851,7851),(7853,7853),(7855,7855),(7857,7857),(7859,7859),(7861,7861),(7863,7863),(7865,7865),(7867,7867),(7869,7869),(7871,7871),(7873,7873),(7875,7875),(7877,7877),(7879,7879),(7881,7881),(7883,7883),(7885,7885),(7887,7887),(7889,7889),(7891,7891),(7893,7893),(7895,7895),(7897,7897),(7899,7899),(7901,7901),(7903,7903),(7905,7905),(7907,7907),(7909,7909),(7911,7911),(7913,7913),(7915,7915),(7917,7917),(7919,7919),(7921,7921),(7923,7923),(7925,7925),(7927,7927),(7929,7929),(7931,7931),(7933,7933),(7935,7943),(7952,7957),(7968,7975),(7984,7991),(8000,8005),(8016,8023),(8032,8039),(8048,8061),(8064,8071),(8080,8087),(8096,8103),(8112,8116),(8118,8119),(8126,8126),(8130,8132),(8134,8135),(8144,8147),(8150,8151),(8160,8167),(8178,8180),(8182,8183),(8458,8458),(8462,8463),(8467,8467),(8495,8495),(8500,8500),(8505,8505),(8508,8509),(8518,8521),(8526,8526),(8580,8580),(11312,11358),(11361,11361),(11365,11366),(11368,11368),(11370,11370),(11372,11372),(11377,11377),(11379,11380),(11382,11387),(11393,11393),(11395,11395),(11397,11397),(11399,11399),(11401,11401),(11403,11403),(11405,11405),(11407,11407),(11409,11409),(11411,11411),(11413,11413),(11415,11415),(11417,11417),(11419,11419),(11421,11421),(11423,11423),(11425,11425),(11427,11427),(11429,11429),(11431,11431),(11433,11433),(11435,11435),(11437,11437),(11439,11439),(11441,11441),(11443,11443),(11445,11445),(11447,11447),(11449,11449),(11451,11451),(11453,11453),(11455,11455),(11457,11457),(11459,11459),(11461,11461),(11463,11463),(11465,11465),(11467,11467),(11469,11469),(11471,11471),(11473,11473),(11475,11475),(11477,11477),(11479,11479),(11481,11481),(11483,11483),(11485,11485),(11487,11487),(11489,11489),(11491,11492),(11500,11500),(11502,11502),(11507,11507),(11520,11557),(11559,11559),(11565,11565),(42561,42561),(42563,42563),(42565,42565),(42567,42567),(42569,42569),(42571,42571),(42573,42573),(42575,42575),(42577,42577),(42579,42579),(42581,42581),(42583,42583),(42585,42585),(42587,42587),(42589,42589),(42591,42591),(42593,42593),(42595,42595),(42597,42597),(42599,42599),(42601,42601),(42603,42603),(42605,42605),(42625,42625),(42627,42627),(42629,42629),(42631,42631),(42633,42633),(42635,42635),(42637,42637),(42639,42639),(42641,42641),(42643,42643),(42645,42645),(42647,42647),(42649,42649),(42651,42651),(42787,42787),(42789,42789),(42791,42791),(42793,42793),(42795,42795),(42797,42797),(42799,42801),(42803,42803),(42805,42805),(42807,42807),(42809,42809),(42811,42811),(42813,42813),(42815,42815),(42817,42817),(42819,42819),(42821,42821),(42823,42823),(42825,42825),(42827,42827),(42829,42829),(42831,42831),(42833,42833),(42835,42835),(42837,42837),(42839,42839),(42841,42841),(42843,42843),(42845,42845),(42847,42847),(42849,42849),(42851,42851),(42853,42853),(42855,42855),(42857,42857),(42859,42859),(42861,42861),(42863,42863),(42865,42872),(42874,42874),(42876,42876),(42879,42879),(42881,42881),(42883,42883),(42885,42885),(42887,42887),(42892,42892),(42894,42894),(42897,42897),(42899,42901),(42903,42903),(42905,42905),(42907,42907),(42909,42909),(42911,42911),(42913,42913),(42915,42915),(42917,42917),(42919,42919),(42921,42921),(43002,43002),(43824,43866),(43876,43877),(64256,64262),(64275,64279),(65345,65370),(66600,66639),(71872,71903),(119834,119859),(119886,119892),(119894,119911),(119938,119963),(119990,119993),(119995,119995),(119997,120003),(120005,120015),(120042,120067),(120094,120119),(120146,120171),(120198,120223),(120250,120275),(120302,120327),(120354,120379),(120406,120431),(120458,120485),(120514,120538),(120540,120545),(120572,120596),(120598,120603),(120630,120654),(120656,120661),(120688,120712),(120714,120719),(120746,120770),(120772,120777),(120779,120779)] then 23
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(453,453),(456,456),(459,459),(498,498),(8072,8079),(8088,8095),(8104,8111),(8124,8124),(8140,8140),(8188,8188)] then 26
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      (1, 2) -> 2
      (1, 3) -> 2
      (1, 4) -> 2
      (1, 5) -> 2
      (1, 7) -> 2
      (1, 8) -> 2
      (1, 9) -> 2
      (1, 10) -> 2
      (1, 11) -> 2
      (1, 14) -> 2
      (1, 15) -> 2
      (1, 16) -> 2
      (1, 17) -> 2
      (1, 18) -> 2
      (1, 19) -> 3
      (1, 20) -> 2
      (1, 21) -> 2
      (1, 24) -> 2
      (1, 25) -> 2
      (1, 26) -> 3
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 7) -> 2
      (2, 8) -> 2
      (2, 9) -> 2
      (2, 10) -> 2
      (2, 11) -> 2
      (2, 13) -> 2
      (2, 14) -> 2
      (2, 15) -> 2
      (2, 16) -> 2
      (2, 17) -> 2
      (2, 18) -> 2
      (2, 20) -> 2
      (2, 21) -> 2
      (2, 24) -> 2
      (2, 25) -> 2
      (3, 6) -> 3
      (3, 10) -> 1
      (3, 12) -> 3
      (3, 19) -> 3
      (3, 22) -> 3
      (3, 23) -> 3
      (3, 26) -> 3
      _ -> 0

dfa53InitialState :: Int
dfa53InitialState = 1

dfa53FinalStates :: [Int]
dfa53FinalStates = [2]

dfa53Transition :: Int -> Char -> Int
dfa53Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          33 -> 1
          35 -> 2
          36 -> 3
          37 -> 4
          38 -> 5
          39 -> 6
          42 -> 7
          43 -> 8
          45 -> 9
          46 -> 10
          47 -> 11
          58 -> 13
          60 -> 14
          61 -> 15
          62 -> 16
          63 -> 17
          64 -> 18
          92 -> 20
          94 -> 21
          95 -> 22
          124 -> 24
          126 -> 25
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 12
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(65,90),(192,214),(216,222),(256,256),(258,258),(260,260),(262,262),(264,264),(266,266),(268,268),(270,270),(272,272),(274,274),(276,276),(278,278),(280,280),(282,282),(284,284),(286,286),(288,288),(290,290),(292,292),(294,294),(296,296),(298,298),(300,300),(302,302),(304,304),(306,306),(308,308),(310,310),(313,313),(315,315),(317,317),(319,319),(321,321),(323,323),(325,325),(327,327),(330,330),(332,332),(334,334),(336,336),(338,338),(340,340),(342,342),(344,344),(346,346),(348,348),(350,350),(352,352),(354,354),(356,356),(358,358),(360,360),(362,362),(364,364),(366,366),(368,368),(370,370),(372,372),(374,374),(376,377),(379,379),(381,381),(385,386),(388,388),(390,391),(393,395),(398,401),(403,404),(406,408),(412,413),(415,416),(418,418),(420,420),(422,423),(425,425),(428,428),(430,431),(433,435),(437,437),(439,440),(444,444),(452,452),(455,455),(458,458),(461,461),(463,463),(465,465),(467,467),(469,469),(471,471),(473,473),(475,475),(478,478),(480,480),(482,482),(484,484),(486,486),(488,488),(490,490),(492,492),(494,494),(497,497),(500,500),(502,504),(506,506),(508,508),(510,510),(512,512),(514,514),(516,516),(518,518),(520,520),(522,522),(524,524),(526,526),(528,528),(530,530),(532,532),(534,534),(536,536),(538,538),(540,540),(542,542),(544,544),(546,546),(548,548),(550,550),(552,552),(554,554),(556,556),(558,558),(560,560),(562,562),(570,571),(573,574),(577,577),(579,582),(584,584),(586,586),(588,588),(590,590),(880,880),(882,882),(886,886),(895,895),(902,902),(904,906),(908,908),(910,911),(913,929),(931,939),(975,975),(978,980),(984,984),(986,986),(988,988),(990,990),(992,992),(994,994),(996,996),(998,998),(1000,1000),(1002,1002),(1004,1004),(1006,1006),(1012,1012),(1015,1015),(1017,1018),(1021,1071),(1120,1120),(1122,1122),(1124,1124),(1126,1126),(1128,1128),(1130,1130),(1132,1132),(1134,1134),(1136,1136),(1138,1138),(1140,1140),(1142,1142),(1144,1144),(1146,1146),(1148,1148),(1150,1150),(1152,1152),(1162,1162),(1164,1164),(1166,1166),(1168,1168),(1170,1170),(1172,1172),(1174,1174),(1176,1176),(1178,1178),(1180,1180),(1182,1182),(1184,1184),(1186,1186),(1188,1188),(1190,1190),(1192,1192),(1194,1194),(1196,1196),(1198,1198),(1200,1200),(1202,1202),(1204,1204),(1206,1206),(1208,1208),(1210,1210),(1212,1212),(1214,1214),(1216,1217),(1219,1219),(1221,1221),(1223,1223),(1225,1225),(1227,1227),(1229,1229),(1232,1232),(1234,1234),(1236,1236),(1238,1238),(1240,1240),(1242,1242),(1244,1244),(1246,1246),(1248,1248),(1250,1250),(1252,1252),(1254,1254),(1256,1256),(1258,1258),(1260,1260),(1262,1262),(1264,1264),(1266,1266),(1268,1268),(1270,1270),(1272,1272),(1274,1274),(1276,1276),(1278,1278),(1280,1280),(1282,1282),(1284,1284),(1286,1286),(1288,1288),(1290,1290),(1292,1292),(1294,1294),(1296,1296),(1298,1298),(1300,1300),(1302,1302),(1304,1304),(1306,1306),(1308,1308),(1310,1310),(1312,1312),(1314,1314),(1316,1316),(1318,1318),(1320,1320),(1322,1322),(1324,1324),(1326,1326),(1329,1366),(4256,4293),(4295,4295),(4301,4301),(7680,7680),(7682,7682),(7684,7684),(7686,7686),(7688,7688),(7690,7690),(7692,7692),(7694,7694),(7696,7696),(7698,7698),(7700,7700),(7702,7702),(7704,7704),(7706,7706),(7708,7708),(7710,7710),(7712,7712),(7714,7714),(7716,7716),(7718,7718),(7720,7720),(7722,7722),(7724,7724),(7726,7726),(7728,7728),(7730,7730),(7732,7732),(7734,7734),(7736,7736),(7738,7738),(7740,7740),(7742,7742),(7744,7744),(7746,7746),(7748,7748),(7750,7750),(7752,7752),(7754,7754),(7756,7756),(7758,7758),(7760,7760),(7762,7762),(7764,7764),(7766,7766),(7768,7768),(7770,7770),(7772,7772),(7774,7774),(7776,7776),(7778,7778),(7780,7780),(7782,7782),(7784,7784),(7786,7786),(7788,7788),(7790,7790),(7792,7792),(7794,7794),(7796,7796),(7798,7798),(7800,7800),(7802,7802),(7804,7804),(7806,7806),(7808,7808),(7810,7810),(7812,7812),(7814,7814),(7816,7816),(7818,7818),(7820,7820),(7822,7822),(7824,7824),(7826,7826),(7828,7828),(7838,7838),(7840,7840),(7842,7842),(7844,7844),(7846,7846),(7848,7848),(7850,7850),(7852,7852),(7854,7854),(7856,7856),(7858,7858),(7860,7860),(7862,7862),(7864,7864),(7866,7866),(7868,7868),(7870,7870),(7872,7872),(7874,7874),(7876,7876),(7878,7878),(7880,7880),(7882,7882),(7884,7884),(7886,7886),(7888,7888),(7890,7890),(7892,7892),(7894,7894),(7896,7896),(7898,7898),(7900,7900),(7902,7902),(7904,7904),(7906,7906),(7908,7908),(7910,7910),(7912,7912),(7914,7914),(7916,7916),(7918,7918),(7920,7920),(7922,7922),(7924,7924),(7926,7926),(7928,7928),(7930,7930),(7932,7932),(7934,7934),(7944,7951),(7960,7965),(7976,7983),(7992,7999),(8008,8013),(8025,8025),(8027,8027),(8029,8029),(8031,8031),(8040,8047),(8120,8123),(8136,8139),(8152,8155),(8168,8172),(8184,8187),(8450,8450),(8455,8455),(8459,8461),(8464,8466),(8469,8469),(8473,8477),(8484,8484),(8486,8486),(8488,8488),(8490,8493),(8496,8499),(8510,8511),(8517,8517),(8579,8579),(11264,11310),(11360,11360),(11362,11364),(11367,11367),(11369,11369),(11371,11371),(11373,11376),(11378,11378),(11381,11381),(11390,11392),(11394,11394),(11396,11396),(11398,11398),(11400,11400),(11402,11402),(11404,11404),(11406,11406),(11408,11408),(11410,11410),(11412,11412),(11414,11414),(11416,11416),(11418,11418),(11420,11420),(11422,11422),(11424,11424),(11426,11426),(11428,11428),(11430,11430),(11432,11432),(11434,11434),(11436,11436),(11438,11438),(11440,11440),(11442,11442),(11444,11444),(11446,11446),(11448,11448),(11450,11450),(11452,11452),(11454,11454),(11456,11456),(11458,11458),(11460,11460),(11462,11462),(11464,11464),(11466,11466),(11468,11468),(11470,11470),(11472,11472),(11474,11474),(11476,11476),(11478,11478),(11480,11480),(11482,11482),(11484,11484),(11486,11486),(11488,11488),(11490,11490),(11499,11499),(11501,11501),(11506,11506),(42560,42560),(42562,42562),(42564,42564),(42566,42566),(42568,42568),(42570,42570),(42572,42572),(42574,42574),(42576,42576),(42578,42578),(42580,42580),(42582,42582),(42584,42584),(42586,42586),(42588,42588),(42590,42590),(42592,42592),(42594,42594),(42596,42596),(42598,42598),(42600,42600),(42602,42602),(42604,42604),(42624,42624),(42626,42626),(42628,42628),(42630,42630),(42632,42632),(42634,42634),(42636,42636),(42638,42638),(42640,42640),(42642,42642),(42644,42644),(42646,42646),(42648,42648),(42650,42650),(42786,42786),(42788,42788),(42790,42790),(42792,42792),(42794,42794),(42796,42796),(42798,42798),(42802,42802),(42804,42804),(42806,42806),(42808,42808),(42810,42810),(42812,42812),(42814,42814),(42816,42816),(42818,42818),(42820,42820),(42822,42822),(42824,42824),(42826,42826),(42828,42828),(42830,42830),(42832,42832),(42834,42834),(42836,42836),(42838,42838),(42840,42840),(42842,42842),(42844,42844),(42846,42846),(42848,42848),(42850,42850),(42852,42852),(42854,42854),(42856,42856),(42858,42858),(42860,42860),(42862,42862),(42873,42873),(42875,42875),(42877,42878),(42880,42880),(42882,42882),(42884,42884),(42886,42886),(42891,42891),(42893,42893),(42896,42896),(42898,42898),(42902,42902),(42904,42904),(42906,42906),(42908,42908),(42910,42910),(42912,42912),(42914,42914),(42916,42916),(42918,42918),(42920,42920),(42922,42925),(42928,42929),(65313,65338),(66560,66599),(71840,71871),(119808,119833),(119860,119885),(119912,119937),(119964,119964),(119966,119967),(119970,119970),(119973,119974),(119977,119980),(119982,119989),(120016,120041),(120068,120069),(120071,120074),(120077,120084),(120086,120092),(120120,120121),(120123,120126),(120128,120132),(120134,120134),(120138,120144),(120172,120197),(120224,120249),(120276,120301),(120328,120353),(120380,120405),(120432,120457),(120488,120512),(120546,120570),(120604,120628),(120662,120686),(120720,120744),(120778,120778)] then 19
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,122),(181,181),(223,246),(248,255),(257,257),(259,259),(261,261),(263,263),(265,265),(267,267),(269,269),(271,271),(273,273),(275,275),(277,277),(279,279),(281,281),(283,283),(285,285),(287,287),(289,289),(291,291),(293,293),(295,295),(297,297),(299,299),(301,301),(303,303),(305,305),(307,307),(309,309),(311,312),(314,314),(316,316),(318,318),(320,320),(322,322),(324,324),(326,326),(328,329),(331,331),(333,333),(335,335),(337,337),(339,339),(341,341),(343,343),(345,345),(347,347),(349,349),(351,351),(353,353),(355,355),(357,357),(359,359),(361,361),(363,363),(365,365),(367,367),(369,369),(371,371),(373,373),(375,375),(378,378),(380,380),(382,384),(387,387),(389,389),(392,392),(396,397),(402,402),(405,405),(409,411),(414,414),(417,417),(419,419),(421,421),(424,424),(426,427),(429,429),(432,432),(436,436),(438,438),(441,442),(445,447),(454,454),(457,457),(460,460),(462,462),(464,464),(466,466),(468,468),(470,470),(472,472),(474,474),(476,477),(479,479),(481,481),(483,483),(485,485),(487,487),(489,489),(491,491),(493,493),(495,496),(499,499),(501,501),(505,505),(507,507),(509,509),(511,511),(513,513),(515,515),(517,517),(519,519),(521,521),(523,523),(525,525),(527,527),(529,529),(531,531),(533,533),(535,535),(537,537),(539,539),(541,541),(543,543),(545,545),(547,547),(549,549),(551,551),(553,553),(555,555),(557,557),(559,559),(561,561),(563,569),(572,572),(575,576),(578,578),(583,583),(585,585),(587,587),(589,589),(591,659),(661,687),(881,881),(883,883),(887,887),(891,893),(912,912),(940,974),(976,977),(981,983),(985,985),(987,987),(989,989),(991,991),(993,993),(995,995),(997,997),(999,999),(1001,1001),(1003,1003),(1005,1005),(1007,1011),(1013,1013),(1016,1016),(1019,1020),(1072,1119),(1121,1121),(1123,1123),(1125,1125),(1127,1127),(1129,1129),(1131,1131),(1133,1133),(1135,1135),(1137,1137),(1139,1139),(1141,1141),(1143,1143),(1145,1145),(1147,1147),(1149,1149),(1151,1151),(1153,1153),(1163,1163),(1165,1165),(1167,1167),(1169,1169),(1171,1171),(1173,1173),(1175,1175),(1177,1177),(1179,1179),(1181,1181),(1183,1183),(1185,1185),(1187,1187),(1189,1189),(1191,1191),(1193,1193),(1195,1195),(1197,1197),(1199,1199),(1201,1201),(1203,1203),(1205,1205),(1207,1207),(1209,1209),(1211,1211),(1213,1213),(1215,1215),(1218,1218),(1220,1220),(1222,1222),(1224,1224),(1226,1226),(1228,1228),(1230,1231),(1233,1233),(1235,1235),(1237,1237),(1239,1239),(1241,1241),(1243,1243),(1245,1245),(1247,1247),(1249,1249),(1251,1251),(1253,1253),(1255,1255),(1257,1257),(1259,1259),(1261,1261),(1263,1263),(1265,1265),(1267,1267),(1269,1269),(1271,1271),(1273,1273),(1275,1275),(1277,1277),(1279,1279),(1281,1281),(1283,1283),(1285,1285),(1287,1287),(1289,1289),(1291,1291),(1293,1293),(1295,1295),(1297,1297),(1299,1299),(1301,1301),(1303,1303),(1305,1305),(1307,1307),(1309,1309),(1311,1311),(1313,1313),(1315,1315),(1317,1317),(1319,1319),(1321,1321),(1323,1323),(1325,1325),(1327,1327),(1377,1415),(7424,7467),(7531,7543),(7545,7578),(7681,7681),(7683,7683),(7685,7685),(7687,7687),(7689,7689),(7691,7691),(7693,7693),(7695,7695),(7697,7697),(7699,7699),(7701,7701),(7703,7703),(7705,7705),(7707,7707),(7709,7709),(7711,7711),(7713,7713),(7715,7715),(7717,7717),(7719,7719),(7721,7721),(7723,7723),(7725,7725),(7727,7727),(7729,7729),(7731,7731),(7733,7733),(7735,7735),(7737,7737),(7739,7739),(7741,7741),(7743,7743),(7745,7745),(7747,7747),(7749,7749),(7751,7751),(7753,7753),(7755,7755),(7757,7757),(7759,7759),(7761,7761),(7763,7763),(7765,7765),(7767,7767),(7769,7769),(7771,7771),(7773,7773),(7775,7775),(7777,7777),(7779,7779),(7781,7781),(7783,7783),(7785,7785),(7787,7787),(7789,7789),(7791,7791),(7793,7793),(7795,7795),(7797,7797),(7799,7799),(7801,7801),(7803,7803),(7805,7805),(7807,7807),(7809,7809),(7811,7811),(7813,7813),(7815,7815),(7817,7817),(7819,7819),(7821,7821),(7823,7823),(7825,7825),(7827,7827),(7829,7837),(7839,7839),(7841,7841),(7843,7843),(7845,7845),(7847,7847),(7849,7849),(7851,7851),(7853,7853),(7855,7855),(7857,7857),(7859,7859),(7861,7861),(7863,7863),(7865,7865),(7867,7867),(7869,7869),(7871,7871),(7873,7873),(7875,7875),(7877,7877),(7879,7879),(7881,7881),(7883,7883),(7885,7885),(7887,7887),(7889,7889),(7891,7891),(7893,7893),(7895,7895),(7897,7897),(7899,7899),(7901,7901),(7903,7903),(7905,7905),(7907,7907),(7909,7909),(7911,7911),(7913,7913),(7915,7915),(7917,7917),(7919,7919),(7921,7921),(7923,7923),(7925,7925),(7927,7927),(7929,7929),(7931,7931),(7933,7933),(7935,7943),(7952,7957),(7968,7975),(7984,7991),(8000,8005),(8016,8023),(8032,8039),(8048,8061),(8064,8071),(8080,8087),(8096,8103),(8112,8116),(8118,8119),(8126,8126),(8130,8132),(8134,8135),(8144,8147),(8150,8151),(8160,8167),(8178,8180),(8182,8183),(8458,8458),(8462,8463),(8467,8467),(8495,8495),(8500,8500),(8505,8505),(8508,8509),(8518,8521),(8526,8526),(8580,8580),(11312,11358),(11361,11361),(11365,11366),(11368,11368),(11370,11370),(11372,11372),(11377,11377),(11379,11380),(11382,11387),(11393,11393),(11395,11395),(11397,11397),(11399,11399),(11401,11401),(11403,11403),(11405,11405),(11407,11407),(11409,11409),(11411,11411),(11413,11413),(11415,11415),(11417,11417),(11419,11419),(11421,11421),(11423,11423),(11425,11425),(11427,11427),(11429,11429),(11431,11431),(11433,11433),(11435,11435),(11437,11437),(11439,11439),(11441,11441),(11443,11443),(11445,11445),(11447,11447),(11449,11449),(11451,11451),(11453,11453),(11455,11455),(11457,11457),(11459,11459),(11461,11461),(11463,11463),(11465,11465),(11467,11467),(11469,11469),(11471,11471),(11473,11473),(11475,11475),(11477,11477),(11479,11479),(11481,11481),(11483,11483),(11485,11485),(11487,11487),(11489,11489),(11491,11492),(11500,11500),(11502,11502),(11507,11507),(11520,11557),(11559,11559),(11565,11565),(42561,42561),(42563,42563),(42565,42565),(42567,42567),(42569,42569),(42571,42571),(42573,42573),(42575,42575),(42577,42577),(42579,42579),(42581,42581),(42583,42583),(42585,42585),(42587,42587),(42589,42589),(42591,42591),(42593,42593),(42595,42595),(42597,42597),(42599,42599),(42601,42601),(42603,42603),(42605,42605),(42625,42625),(42627,42627),(42629,42629),(42631,42631),(42633,42633),(42635,42635),(42637,42637),(42639,42639),(42641,42641),(42643,42643),(42645,42645),(42647,42647),(42649,42649),(42651,42651),(42787,42787),(42789,42789),(42791,42791),(42793,42793),(42795,42795),(42797,42797),(42799,42801),(42803,42803),(42805,42805),(42807,42807),(42809,42809),(42811,42811),(42813,42813),(42815,42815),(42817,42817),(42819,42819),(42821,42821),(42823,42823),(42825,42825),(42827,42827),(42829,42829),(42831,42831),(42833,42833),(42835,42835),(42837,42837),(42839,42839),(42841,42841),(42843,42843),(42845,42845),(42847,42847),(42849,42849),(42851,42851),(42853,42853),(42855,42855),(42857,42857),(42859,42859),(42861,42861),(42863,42863),(42865,42872),(42874,42874),(42876,42876),(42879,42879),(42881,42881),(42883,42883),(42885,42885),(42887,42887),(42892,42892),(42894,42894),(42897,42897),(42899,42901),(42903,42903),(42905,42905),(42907,42907),(42909,42909),(42911,42911),(42913,42913),(42915,42915),(42917,42917),(42919,42919),(42921,42921),(43002,43002),(43824,43866),(43876,43877),(64256,64262),(64275,64279),(65345,65370),(66600,66639),(71872,71903),(119834,119859),(119886,119892),(119894,119911),(119938,119963),(119990,119993),(119995,119995),(119997,120003),(120005,120015),(120042,120067),(120094,120119),(120146,120171),(120198,120223),(120250,120275),(120302,120327),(120354,120379),(120406,120431),(120458,120485),(120514,120538),(120540,120545),(120572,120596),(120598,120603),(120630,120654),(120656,120661),(120688,120712),(120714,120719),(120746,120770),(120772,120777),(120779,120779)] then 23
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(453,453),(456,456),(459,459),(498,498),(8072,8079),(8088,8095),(8104,8111),(8124,8124),(8140,8140),(8188,8188)] then 26
            else 0 in
    case (q, c') of
      (1, 13) -> 2
      (1, 19) -> 3
      (1, 26) -> 3
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 7) -> 2
      (2, 8) -> 2
      (2, 9) -> 2
      (2, 10) -> 2
      (2, 11) -> 2
      (2, 13) -> 2
      (2, 14) -> 2
      (2, 15) -> 2
      (2, 16) -> 2
      (2, 17) -> 2
      (2, 18) -> 2
      (2, 20) -> 2
      (2, 21) -> 2
      (2, 24) -> 2
      (2, 25) -> 2
      (3, 6) -> 3
      (3, 10) -> 1
      (3, 12) -> 3
      (3, 19) -> 3
      (3, 22) -> 3
      (3, 23) -> 3
      (3, 26) -> 3
      _ -> 0

dfa54InitialState :: Int
dfa54InitialState = 1

dfa54FinalStates :: [Int]
dfa54FinalStates = [2,3]

dfa54Transition :: Int -> Char -> Int
dfa54Transition q c =
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

dfa55InitialState :: Int
dfa55InitialState = 1

dfa55FinalStates :: [Int]
dfa55FinalStates = [2]

dfa55Transition :: Int -> Char -> Int
dfa55Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          c'' ->
            0 in
    case (q, c') of
      (1, 0) -> 2
      _ -> 0

dfa56InitialState :: Int
dfa56InitialState = 1

dfa56FinalStates :: [Int]
dfa56FinalStates = [3]

dfa56Transition :: Int -> Char -> Int
dfa56Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          45 -> 1
          123 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 2) -> 2
      (2, 1) -> 3
      _ -> 0

dfa57InitialState :: Int
dfa57InitialState = 1

dfa57FinalStates :: [Int]
dfa57FinalStates = [3]

dfa57Transition :: Int -> Char -> Int
dfa57Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          45 -> 1
          125 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa58InitialState :: Int
dfa58InitialState = 1

dfa58FinalStates :: [Int]
dfa58FinalStates = [2]

dfa58Transition :: Int -> Char -> Int
dfa58Transition q c =
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
      case max (match dfa0InitialState dfa0FinalStates dfa0Transition s, -0) $ max (match dfa1InitialState dfa1FinalStates dfa1Transition s, -1) $ max (match dfa2InitialState dfa2FinalStates dfa2Transition s, -2) $ max (match dfa3InitialState dfa3FinalStates dfa3Transition s, -3) $ max (match dfa4InitialState dfa4FinalStates dfa4Transition s, -4) $ max (match dfa5InitialState dfa5FinalStates dfa5Transition s, -5) $ max (match dfa6InitialState dfa6FinalStates dfa6Transition s, -6) $ max (match dfa7InitialState dfa7FinalStates dfa7Transition s, -7) $ max (match dfa8InitialState dfa8FinalStates dfa8Transition s, -8) $ max (match dfa9InitialState dfa9FinalStates dfa9Transition s, -9) $ max (match dfa10InitialState dfa10FinalStates dfa10Transition s, -10) $ max (match dfa11InitialState dfa11FinalStates dfa11Transition s, -11) $ max (match dfa12InitialState dfa12FinalStates dfa12Transition s, -12) $ max (match dfa13InitialState dfa13FinalStates dfa13Transition s, -13) $ max (match dfa14InitialState dfa14FinalStates dfa14Transition s, -14) $ max (match dfa15InitialState dfa15FinalStates dfa15Transition s, -15) $ max (match dfa16InitialState dfa16FinalStates dfa16Transition s, -16) $ max (match dfa17InitialState dfa17FinalStates dfa17Transition s, -17) $ max (match dfa18InitialState dfa18FinalStates dfa18Transition s, -18) $ max (match dfa19InitialState dfa19FinalStates dfa19Transition s, -19) $ max (match dfa20InitialState dfa20FinalStates dfa20Transition s, -20) $ max (match dfa21InitialState dfa21FinalStates dfa21Transition s, -21) $ max (match dfa22InitialState dfa22FinalStates dfa22Transition s, -22) $ max (match dfa23InitialState dfa23FinalStates dfa23Transition s, -23) $ max (match dfa24InitialState dfa24FinalStates dfa24Transition s, -24) $ max (match dfa25InitialState dfa25FinalStates dfa25Transition s, -25) $ max (match dfa26InitialState dfa26FinalStates dfa26Transition s, -26) $ max (match dfa27InitialState dfa27FinalStates dfa27Transition s, -27) $ max (match dfa28InitialState dfa28FinalStates dfa28Transition s, -28) $ max (match dfa29InitialState dfa29FinalStates dfa29Transition s, -29) $ max (match dfa30InitialState dfa30FinalStates dfa30Transition s, -30) $ max (match dfa31InitialState dfa31FinalStates dfa31Transition s, -31) $ max (match dfa32InitialState dfa32FinalStates dfa32Transition s, -32) $ max (match dfa33InitialState dfa33FinalStates dfa33Transition s, -33) $ max (match dfa34InitialState dfa34FinalStates dfa34Transition s, -34) $ max (match dfa35InitialState dfa35FinalStates dfa35Transition s, -35) $ max (match dfa36InitialState dfa36FinalStates dfa36Transition s, -36) $ max (match dfa37InitialState dfa37FinalStates dfa37Transition s, -37) $ max (match dfa38InitialState dfa38FinalStates dfa38Transition s, -38) $ max (match dfa39InitialState dfa39FinalStates dfa39Transition s, -39) $ max (match dfa40InitialState dfa40FinalStates dfa40Transition s, -40) $ max (match dfa41InitialState dfa41FinalStates dfa41Transition s, -41) $ max (match dfa42InitialState dfa42FinalStates dfa42Transition s, -42) $ max (match dfa43InitialState dfa43FinalStates dfa43Transition s, -43) $ max (match dfa44InitialState dfa44FinalStates dfa44Transition s, -44) $ max (match dfa45InitialState dfa45FinalStates dfa45Transition s, -45) $ max (match dfa46InitialState dfa46FinalStates dfa46Transition s, -46) $ max (match dfa47InitialState dfa47FinalStates dfa47Transition s, -47) $ max (match dfa48InitialState dfa48FinalStates dfa48Transition s, -48) $ max (match dfa49InitialState dfa49FinalStates dfa49Transition s, -49) $ max (match dfa50InitialState dfa50FinalStates dfa50Transition s, -50) $ max (match dfa51InitialState dfa51FinalStates dfa51Transition s, -51) $ max (match dfa52InitialState dfa52FinalStates dfa52Transition s, -52) $ max (match dfa53InitialState dfa53FinalStates dfa53Transition s, -53) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -0 -> do
                x <- saWhitechar actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -1 -> do
                x <- saOpenDashes actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -2 -> do
                x <- saOpenNested actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -3 -> do
                x <- saAs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -4 -> do
                x <- saBackquote actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -5 -> do
                x <- saCase actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -6 -> do
                x <- saClass actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -7 -> do
                x <- saColonColon actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -8 -> do
                x <- saComma actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -9 -> do
                x <- saDArrow actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -10 -> do
                x <- saData actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -11 -> do
                x <- saDefault actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -12 -> do
                x <- saDeriving actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -13 -> do
                x <- saDo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -14 -> do
                x <- saDotDot actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -15 -> do
                x <- saElse actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -16 -> do
                x <- saEqual actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -17 -> do
                x <- saExcl actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -18 -> do
                x <- saExport actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -19 -> do
                x <- saForeign actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -20 -> do
                x <- saHiding actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -21 -> do
                x <- saImport actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -22 -> do
                x <- saIf actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -23 -> do
                x <- saIn actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -24 -> do
                x <- saInfix actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -25 -> do
                x <- saInfixL actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -26 -> do
                x <- saInfixR actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -27 -> do
                x <- saInstance actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -28 -> do
                x <- saInteger actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -29 -> do
                x <- saLambda actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -30 -> do
                x <- saLArrow actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -31 -> do
                x <- saLBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -32 -> do
                x <- saLBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -33 -> do
                x <- saLet actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -34 -> do
                x <- saLParen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -35 -> do
                x <- saMinus actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -36 -> do
                x <- saModule actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -37 -> do
                x <- saNewtype actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -38 -> do
                x <- saOf actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -39 -> do
                x <- saPipe actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -40 -> do
                x <- saQualified actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -41 -> do
                x <- saRArrow actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -42 -> do
                x <- saRBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -43 -> do
                x <- saRBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -44 -> do
                x <- saRParen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -45 -> do
                x <- saSemicolon actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -46 -> do
                x <- saString actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -47 -> do
                x <- saThen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -48 -> do
                x <- saType actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -49 -> do
                x <- saWhere actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -50 -> do
                x <- saVarId actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -51 -> do
                x <- saConId actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -52 -> do
                x <- saVarSym actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -53 -> do
                x <- saConSym actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == Dashes then
      case max (match dfa54InitialState dfa54FinalStates dfa54Transition s, -54) $ max (match dfa55InitialState dfa55FinalStates dfa55Transition s, -55) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -54 -> do
                x <- saCloseDashes actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -55 -> do
                x <- saComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == Nested then
      case max (match dfa56InitialState dfa56FinalStates dfa56Transition s, -56) $ max (match dfa57InitialState dfa57FinalStates dfa57Transition s, -57) $ max (match dfa58InitialState dfa58FinalStates dfa58Transition s, -58) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -56 -> do
                x <- saOpenNested actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -57 -> do
                x <- saCloseNested actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -58 -> do
                x <- saComment actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else
      return ([], s)



withPosition :: (Int -> Int -> String -> Lexing (State.State (Int, Int)) (Maybe Parsing.Token)) -> String -> Lexing (State.State (Int, Int)) (Maybe Parsing.Token)
withPosition f yytext = do
  let n = length yytext
  (pos, nest) <- MonadTrans.lift State.get
  MonadTrans.lift $ State.put (pos + n, nest)
  f pos n yytext

semanticActions :: SemanticActions (State.State (Int, Int)) (Maybe Parsing.Token)
semanticActions = SemanticActions
  { saWhitechar = withPosition $ \_ _ _ ->
      return Nothing
  , saComment = withPosition $ \_ _ _ ->
      return Nothing
  , saOpenDashes = withPosition $ \_ _ _ -> do
      yybegin Dashes
      return Nothing

  , saOpenNested = withPosition $ \_ _ _ -> do
      yybegin Nested
      (pos, nest) <- MonadTrans.lift State.get
      MonadTrans.lift $ State.put (pos, nest + 1)
      return Nothing

  , saCloseNested = withPosition $ \_ _ _ -> do
      (pos, nest) <- MonadTrans.lift State.get

      if nest <= 1 then do
        yybegin Initial
        MonadTrans.lift $ State.put (pos, 0)
      else
        MonadTrans.lift $ State.put (pos, nest - 1)

      return Nothing

  , saAs = withPosition $ \pos n _ ->
      return $ Just $ Parsing.AS (pos, n)
  , saBackquote = withPosition $ \pos n _ ->
      return $ Just $ Parsing.BACKQUOTE (pos, n)
  , saCase = withPosition $ \pos n _ ->
      return $ Just $ Parsing.CASE (pos, n)
  , saClass = withPosition $ \pos n _ ->
      return $ Just $ Parsing.CLASS (pos, n)
  , saColonColon = withPosition $ \pos n _ ->
      return $ Just $ Parsing.COLON_COLON (pos, n)
  , saComma = withPosition $ \pos n _ ->
      return $ Just $ Parsing.COMMA (pos, n)
  , saDArrow = withPosition $ \pos n _ ->
      return $ Just $ Parsing.DARROW (pos, n)
  , saData = withPosition $ \pos n _ ->
      return $ Just $ Parsing.DATA (pos, n)
  , saDefault = withPosition $ \pos n _ ->
      return $ Just $ Parsing.DEFAULT (pos, n)
  , saDeriving = withPosition $ \pos n _ ->
      return $ Just $ Parsing.DERIVING (pos, n)
  , saDo = withPosition $ \pos n _ ->
      return $ Just $ Parsing.DO (pos, n)
  , saDotDot = withPosition $ \pos n _ ->
      return $ Just $ Parsing.DOT_DOT (pos, n)
  , saElse = withPosition $ \pos n _ ->
      return $ Just $ Parsing.ELSE (pos, n)
  , saEqual = withPosition $ \pos n _ ->
      return $ Just $ Parsing.EQUAL (pos, n)
  , saExcl = withPosition $ \pos n _ ->
      return $ Just $ Parsing.EXCL (pos, n)
  , saExport = withPosition $ \pos n _ ->
      return $ Just $ Parsing.EXPORT (pos, n)
  , saForeign = withPosition $ \pos n _ ->
      return $ Just $ Parsing.FOREIGN (pos, n)
  , saHiding = withPosition $ \pos n _ ->
      return $ Just $ Parsing.HIDING (pos, n)
  , saImport = withPosition $ \pos n _ ->
      return $ Just $ Parsing.IMPORT (pos, n)
  , saIf = withPosition $ \pos n _ ->
      return $ Just $ Parsing.IF (pos, n)
  , saIn = withPosition $ \pos n _ ->
      return $ Just $ Parsing.IN (pos, n)
  , saInfix = withPosition $ \pos n _ ->
      return $ Just $ Parsing.INFIX (pos, n)
  , saInfixL = withPosition $ \pos n _ ->
      return $ Just $ Parsing.INFIXL (pos, n)
  , saInfixR = withPosition $ \pos n _ ->
      return $ Just $ Parsing.INFIXR (pos, n)
  , saInstance = withPosition $ \pos n _ ->
      return $ Just $ Parsing.INSTANCE (pos, n)
  , saInteger = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.INTEGER ((pos, n), read yytext)
  , saLambda = withPosition $ \pos n _ ->
      return $ Just $ Parsing.LAMBDA (pos, n)
  , saLArrow = withPosition $ \pos n _ ->
      return $ Just $ Parsing.LARROW (pos, n)
  , saLBrace = withPosition $ \pos n _ ->
      return $ Just $ Parsing.LBRACE (pos, n)
  , saLBracket = withPosition $ \pos n _ ->
      return $ Just $ Parsing.LBRACKET (pos, n)
  , saLet = withPosition $ \pos n _ ->
      return $ Just $ Parsing.LET (pos, n)
  , saLParen = withPosition $ \pos n _ ->
      return $ Just $ Parsing.LPAREN (pos, n)
  , saMinus = withPosition $ \pos n _ ->
      return $ Just $ Parsing.MINUS (pos, n)
  , saModule = withPosition $ \pos n _ ->
      return $ Just $ Parsing.MODULE (pos, n)
  , saNewtype = withPosition $ \pos n _ ->
      return $ Just $ Parsing.NEWTYPE (pos, n)
  , saOf = withPosition $ \pos n _ ->
      return $ Just $ Parsing.OF (pos, n)
  , saPipe = withPosition $ \pos n _ ->
      return $ Just $ Parsing.PIPE (pos, n)
  , saQualified = withPosition $ \pos n _ ->
      return $ Just $ Parsing.QUALIFIED (pos, n)
  , saRArrow = withPosition $ \pos n _ ->
      return $ Just $ Parsing.RARROW (pos, n)
  , saRBrace = withPosition $ \pos n _ ->
      return $ Just $ Parsing.RBRACE (pos, n)
  , saRBracket = withPosition $ \pos n _ ->
      return $ Just $ Parsing.RBRACKET (pos, n)
  , saRParen = withPosition $ \pos n _ ->
      return $ Just $ Parsing.RPAREN (pos, n)
  , saSemicolon = withPosition $ \pos n _ ->
      return $ Just $ Parsing.SEMICOLON (pos, n)
  , saString = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.STRING ((pos, n), read yytext)
  , saThen = withPosition $ \pos n _ ->
      return $ Just $ Parsing.THEN (pos, n)
  , saType = withPosition $ \pos n _ ->
      return $ Just $ Parsing.TYPE (pos, n)
  , saWhere = withPosition $ \pos n _ ->
      return $ Just $ Parsing.WHERE (pos, n)
  , saVarId = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.QVARID ((pos, n), yytext)
  , saConId = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.QCONID ((pos, n), yytext)
  , saVarSym = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.QVARSYM ((pos, n), yytext)
  , saConSym = withPosition $ \pos n yytext ->
      return $ Just $ Parsing.QCONSYM ((pos, n), yytext)
  , saCloseDashes = withPosition $ \_ _ _ -> do
      yybegin Initial
      return Nothing }


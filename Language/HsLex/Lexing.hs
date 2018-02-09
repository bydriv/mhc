module  Language.HsLex.Lexing  where

import           Prelude
  hiding (lex)
import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans
import qualified Data.Char           as Char


import qualified Language.HsLex.Parsing as Parsing

newtype Lexing m a = Lexing { unLexing :: LexingState -> m (a, LexingState) }

data LexingState =
    Initial
  | Rule
  | Code
  deriving (Eq, Ord, Read, Show)

data SemanticActions m a = SemanticActions
  { initialSpace :: String -> Lexing m a
  , initialNewline :: String -> Lexing m a
  , initialAny :: String -> Lexing m a
  , initialLu :: String -> Lexing m a
  , initialLl :: String -> Lexing m a
  , initialLt :: String -> Lexing m a
  , initialLm :: String -> Lexing m a
  , initialLo :: String -> Lexing m a
  , initialMn :: String -> Lexing m a
  , initialMc :: String -> Lexing m a
  , initialMe :: String -> Lexing m a
  , initialNd :: String -> Lexing m a
  , initialNl :: String -> Lexing m a
  , initialNo :: String -> Lexing m a
  , initialPc :: String -> Lexing m a
  , initialPd :: String -> Lexing m a
  , initialPs :: String -> Lexing m a
  , initialPe :: String -> Lexing m a
  , initialPi :: String -> Lexing m a
  , initialPf :: String -> Lexing m a
  , initialPo :: String -> Lexing m a
  , initialSm :: String -> Lexing m a
  , initialSc :: String -> Lexing m a
  , initialSk :: String -> Lexing m a
  , initialSo :: String -> Lexing m a
  , initialZs :: String -> Lexing m a
  , initialZl :: String -> Lexing m a
  , initialZp :: String -> Lexing m a
  , initialCc :: String -> Lexing m a
  , initialCf :: String -> Lexing m a
  , initialCs :: String -> Lexing m a
  , initialCo :: String -> Lexing m a
  , initialCn :: String -> Lexing m a
  , initialHat :: String -> Lexing m a
  , initialHyphen :: String -> Lexing m a
  , initialLBracket :: String -> Lexing m a
  , initialRBracket :: String -> Lexing m a
  , initialComma :: String -> Lexing m a
  , initialDArrow :: String -> Lexing m a
  , initialCase :: String -> Lexing m a
  , initialStar :: String -> Lexing m a
  , initialPlus :: String -> Lexing m a
  , initialQues :: String -> Lexing m a
  , initialLParen :: String -> Lexing m a
  , initialRParen :: String -> Lexing m a
  , initialString :: String -> Lexing m a
  , initialChar :: String -> Lexing m a
  , initialPLBrace :: String -> Lexing m a
  , initialPModule :: String -> Lexing m a
  , initialPP :: String -> Lexing m a
  , initialPipe :: String -> Lexing m a
  , initialPWhere :: String -> Lexing m a
  , initialPRBrace :: String -> Lexing m a
  , initialSemanticAction :: String -> Lexing m a
  , initialLexingState :: String -> Lexing m a
  , initialCode :: String -> Lexing m a
  , ruleSpace :: String -> Lexing m a
  , ruleNewline :: String -> Lexing m a
  , ruleAny :: String -> Lexing m a
  , ruleLu :: String -> Lexing m a
  , ruleLl :: String -> Lexing m a
  , ruleLt :: String -> Lexing m a
  , ruleLm :: String -> Lexing m a
  , ruleLo :: String -> Lexing m a
  , ruleMn :: String -> Lexing m a
  , ruleMc :: String -> Lexing m a
  , ruleMe :: String -> Lexing m a
  , ruleNd :: String -> Lexing m a
  , ruleNl :: String -> Lexing m a
  , ruleNo :: String -> Lexing m a
  , rulePc :: String -> Lexing m a
  , rulePd :: String -> Lexing m a
  , rulePs :: String -> Lexing m a
  , rulePe :: String -> Lexing m a
  , rulePi :: String -> Lexing m a
  , rulePf :: String -> Lexing m a
  , rulePo :: String -> Lexing m a
  , ruleSm :: String -> Lexing m a
  , ruleSc :: String -> Lexing m a
  , ruleSk :: String -> Lexing m a
  , ruleSo :: String -> Lexing m a
  , ruleZs :: String -> Lexing m a
  , ruleZl :: String -> Lexing m a
  , ruleZp :: String -> Lexing m a
  , ruleCc :: String -> Lexing m a
  , ruleCf :: String -> Lexing m a
  , ruleCs :: String -> Lexing m a
  , ruleCo :: String -> Lexing m a
  , ruleCn :: String -> Lexing m a
  , ruleHat :: String -> Lexing m a
  , ruleHyphen :: String -> Lexing m a
  , ruleLBracket :: String -> Lexing m a
  , ruleRBracket :: String -> Lexing m a
  , ruleComma :: String -> Lexing m a
  , ruleDArrow :: String -> Lexing m a
  , ruleCase :: String -> Lexing m a
  , ruleStar :: String -> Lexing m a
  , rulePlus :: String -> Lexing m a
  , ruleQues :: String -> Lexing m a
  , ruleLParen :: String -> Lexing m a
  , ruleRParen :: String -> Lexing m a
  , ruleString :: String -> Lexing m a
  , ruleChar :: String -> Lexing m a
  , rulePLBrace :: String -> Lexing m a
  , rulePModule :: String -> Lexing m a
  , rulePP :: String -> Lexing m a
  , rulePipe :: String -> Lexing m a
  , rulePWhere :: String -> Lexing m a
  , rulePRBrace :: String -> Lexing m a
  , ruleSemanticAction :: String -> Lexing m a
  , ruleLexingState :: String -> Lexing m a
  , ruleCode :: String -> Lexing m a
  , codePLBrace :: String -> Lexing m a
  , codePModule :: String -> Lexing m a
  , codePP :: String -> Lexing m a
  , codePWhere :: String -> Lexing m a
  , codePRBrace :: String -> Lexing m a
  , codeCode :: String -> Lexing m a }

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
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(32,32),(160,160),(5760,5760),(8192,8202),(8239,8239),(8287,8287),(12288,12288)] then 1
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa1InitialState :: Int
dfa1InitialState = 1

dfa1FinalStates :: [Int]
dfa1FinalStates = [2,3]

dfa1Transition :: Int -> Char -> Int
dfa1Transition q c =
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

dfa2InitialState :: Int
dfa2InitialState = 1

dfa2FinalStates :: [Int]
dfa2FinalStates = [2]

dfa2Transition :: Int -> Char -> Int
dfa2Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          46 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
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
          76 -> 1
          117 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          76 -> 1
          108 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa5InitialState :: Int
dfa5InitialState = 1

dfa5FinalStates :: [Int]
dfa5FinalStates = [3]

dfa5Transition :: Int -> Char -> Int
dfa5Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          76 -> 1
          116 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa6InitialState :: Int
dfa6InitialState = 1

dfa6FinalStates :: [Int]
dfa6FinalStates = [3]

dfa6Transition :: Int -> Char -> Int
dfa6Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          76 -> 1
          109 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          76 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa8InitialState :: Int
dfa8InitialState = 1

dfa8FinalStates :: [Int]
dfa8FinalStates = [3]

dfa8Transition :: Int -> Char -> Int
dfa8Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          77 -> 1
          110 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          77 -> 1
          99 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa10InitialState :: Int
dfa10InitialState = 1

dfa10FinalStates :: [Int]
dfa10FinalStates = [3]

dfa10Transition :: Int -> Char -> Int
dfa10Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          77 -> 1
          101 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa11InitialState :: Int
dfa11InitialState = 1

dfa11FinalStates :: [Int]
dfa11FinalStates = [3]

dfa11Transition :: Int -> Char -> Int
dfa11Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          78 -> 1
          100 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa12InitialState :: Int
dfa12InitialState = 1

dfa12FinalStates :: [Int]
dfa12FinalStates = [3]

dfa12Transition :: Int -> Char -> Int
dfa12Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          78 -> 1
          108 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          78 -> 1
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
          80 -> 1
          99 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa15InitialState :: Int
dfa15InitialState = 1

dfa15FinalStates :: [Int]
dfa15FinalStates = [3]

dfa15Transition :: Int -> Char -> Int
dfa15Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          100 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa16InitialState :: Int
dfa16InitialState = 1

dfa16FinalStates :: [Int]
dfa16FinalStates = [3]

dfa16Transition :: Int -> Char -> Int
dfa16Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          115 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          80 -> 1
          101 -> 2
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
          80 -> 1
          105 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa19InitialState :: Int
dfa19InitialState = 1

dfa19FinalStates :: [Int]
dfa19FinalStates = [3]

dfa19Transition :: Int -> Char -> Int
dfa19Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          102 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa20InitialState :: Int
dfa20InitialState = 1

dfa20FinalStates :: [Int]
dfa20FinalStates = [3]

dfa20Transition :: Int -> Char -> Int
dfa20Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa21InitialState :: Int
dfa21InitialState = 1

dfa21FinalStates :: [Int]
dfa21FinalStates = [3]

dfa21Transition :: Int -> Char -> Int
dfa21Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          83 -> 1
          109 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          83 -> 1
          99 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          83 -> 1
          107 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa24InitialState :: Int
dfa24InitialState = 1

dfa24FinalStates :: [Int]
dfa24FinalStates = [3]

dfa24Transition :: Int -> Char -> Int
dfa24Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          83 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa25InitialState :: Int
dfa25InitialState = 1

dfa25FinalStates :: [Int]
dfa25FinalStates = [3]

dfa25Transition :: Int -> Char -> Int
dfa25Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          90 -> 1
          115 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa26InitialState :: Int
dfa26InitialState = 1

dfa26FinalStates :: [Int]
dfa26FinalStates = [3]

dfa26Transition :: Int -> Char -> Int
dfa26Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          90 -> 1
          108 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa27InitialState :: Int
dfa27InitialState = 1

dfa27FinalStates :: [Int]
dfa27FinalStates = [3]

dfa27Transition :: Int -> Char -> Int
dfa27Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          90 -> 1
          112 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa28InitialState :: Int
dfa28InitialState = 1

dfa28FinalStates :: [Int]
dfa28FinalStates = [3]

dfa28Transition :: Int -> Char -> Int
dfa28Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          99 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa29InitialState :: Int
dfa29InitialState = 1

dfa29FinalStates :: [Int]
dfa29FinalStates = [3]

dfa29Transition :: Int -> Char -> Int
dfa29Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          102 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          67 -> 1
          115 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa31InitialState :: Int
dfa31InitialState = 1

dfa31FinalStates :: [Int]
dfa31FinalStates = [3]

dfa31Transition :: Int -> Char -> Int
dfa31Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa32InitialState :: Int
dfa32InitialState = 1

dfa32FinalStates :: [Int]
dfa32FinalStates = [3]

dfa32Transition :: Int -> Char -> Int
dfa32Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          110 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa33InitialState :: Int
dfa33InitialState = 1

dfa33FinalStates :: [Int]
dfa33FinalStates = [2]

dfa33Transition :: Int -> Char -> Int
dfa33Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          94 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
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
          45 -> 1
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
          91 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa36InitialState :: Int
dfa36InitialState = 1

dfa36FinalStates :: [Int]
dfa36FinalStates = [2]

dfa36Transition :: Int -> Char -> Int
dfa36Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          93 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa37InitialState :: Int
dfa37InitialState = 1

dfa37FinalStates :: [Int]
dfa37FinalStates = [2]

dfa37Transition :: Int -> Char -> Int
dfa37Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          44 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
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
          61 -> 1
          62 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa39InitialState :: Int
dfa39InitialState = 1

dfa39FinalStates :: [Int]
dfa39FinalStates = [5]

dfa39Transition :: Int -> Char -> Int
dfa39Transition q c =
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

dfa40InitialState :: Int
dfa40InitialState = 1

dfa40FinalStates :: [Int]
dfa40FinalStates = [2]

dfa40Transition :: Int -> Char -> Int
dfa40Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          42 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa41InitialState :: Int
dfa41InitialState = 1

dfa41FinalStates :: [Int]
dfa41FinalStates = [2]

dfa41Transition :: Int -> Char -> Int
dfa41Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          43 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
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
          63 -> 1
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
          40 -> 1
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
dfa45FinalStates = [3,4,7]

dfa45Transition :: Int -> Char -> Int
dfa45Transition q c =
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
      (2, 40) -> 5
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
      (3, 2) -> 6
      (3, 3) -> 6
      (3, 4) -> 6
      (3, 5) -> 6
      (3, 6) -> 6
      (3, 7) -> 6
      (3, 8) -> 7
      (3, 9) -> 2
      (3, 10) -> 2
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
      (3, 40) -> 8
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
      (3, 51) -> 6
      (5, 0) -> 6
      (5, 2) -> 9
      (5, 3) -> 9
      (5, 4) -> 9
      (5, 5) -> 9
      (5, 6) -> 9
      (5, 7) -> 9
      (5, 8) -> 2
      (5, 9) -> 2
      (5, 10) -> 2
      (5, 11) -> 2
      (5, 12) -> 2
      (5, 13) -> 2
      (5, 14) -> 2
      (5, 15) -> 2
      (5, 16) -> 2
      (5, 17) -> 10
      (5, 18) -> 11
      (5, 19) -> 12
      (5, 20) -> 13
      (5, 21) -> 14
      (5, 22) -> 15
      (5, 23) -> 16
      (5, 24) -> 17
      (5, 27) -> 18
      (5, 29) -> 19
      (5, 33) -> 16
      (5, 34) -> 20
      (5, 36) -> 16
      (5, 37) -> 17
      (5, 40) -> 2
      (5, 41) -> 2
      (5, 42) -> 2
      (5, 44) -> 2
      (5, 45) -> 2
      (5, 46) -> 21
      (5, 47) -> 2
      (5, 48) -> 2
      (5, 49) -> 2
      (5, 50) -> 22
      (5, 51) -> 9
      (6, 0) -> 3
      (6, 1) -> 2
      (6, 2) -> 6
      (6, 3) -> 6
      (6, 4) -> 6
      (6, 5) -> 6
      (6, 6) -> 6
      (6, 7) -> 6
      (6, 8) -> 4
      (6, 9) -> 2
      (6, 10) -> 2
      (6, 11) -> 2
      (6, 12) -> 2
      (6, 13) -> 2
      (6, 14) -> 2
      (6, 15) -> 2
      (6, 16) -> 2
      (6, 17) -> 2
      (6, 18) -> 2
      (6, 19) -> 2
      (6, 20) -> 2
      (6, 21) -> 2
      (6, 22) -> 2
      (6, 23) -> 2
      (6, 24) -> 2
      (6, 25) -> 2
      (6, 26) -> 2
      (6, 27) -> 2
      (6, 28) -> 2
      (6, 29) -> 2
      (6, 30) -> 2
      (6, 31) -> 2
      (6, 32) -> 2
      (6, 33) -> 2
      (6, 34) -> 2
      (6, 35) -> 2
      (6, 36) -> 2
      (6, 37) -> 2
      (6, 38) -> 2
      (6, 39) -> 2
      (6, 40) -> 8
      (6, 41) -> 2
      (6, 42) -> 2
      (6, 43) -> 2
      (6, 44) -> 2
      (6, 45) -> 2
      (6, 46) -> 2
      (6, 47) -> 2
      (6, 48) -> 2
      (6, 49) -> 2
      (6, 50) -> 2
      (6, 51) -> 6
      (7, 0) -> 3
      (7, 1) -> 2
      (7, 2) -> 2
      (7, 3) -> 2
      (7, 4) -> 2
      (7, 5) -> 2
      (7, 6) -> 2
      (7, 7) -> 2
      (7, 8) -> 4
      (7, 9) -> 2
      (7, 10) -> 2
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
      (7, 40) -> 5
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
      (7, 51) -> 2
      (8, 0) -> 3
      (8, 1) -> 2
      (8, 2) -> 6
      (8, 3) -> 6
      (8, 4) -> 6
      (8, 5) -> 6
      (8, 6) -> 6
      (8, 7) -> 6
      (8, 8) -> 7
      (8, 9) -> 2
      (8, 10) -> 2
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
      (8, 40) -> 8
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
      (8, 51) -> 6
      (9, 0) -> 6
      (9, 2) -> 9
      (9, 3) -> 9
      (9, 4) -> 9
      (9, 5) -> 9
      (9, 6) -> 9
      (9, 7) -> 9
      (9, 40) -> 2
      (9, 51) -> 9
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

dfa46InitialState :: Int
dfa46InitialState = 1

dfa46FinalStates :: [Int]
dfa46FinalStates = [6,7,22,24,36,52,53]

dfa46Transition :: Int -> Char -> Int
dfa46Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          34 -> 2
          39 -> 3
          49 -> 5
          50 -> 6
          51 -> 7
          52 -> 8
          65 -> 10
          66 -> 11
          67 -> 12
          68 -> 13
          69 -> 14
          70 -> 15
          71 -> 16
          72 -> 17
          73 -> 18
          75 -> 19
          76 -> 20
          77 -> 21
          78 -> 22
          79 -> 23
          80 -> 24
          81 -> 25
          82 -> 26
          83 -> 27
          84 -> 28
          85 -> 29
          86 -> 30
          88 -> 31
          89 -> 32
          92 -> 33
          97 -> 34
          98 -> 35
          102 -> 37
          110 -> 38
          111 -> 39
          114 -> 40
          116 -> 41
          118 -> 42
          120 -> 43
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,33),(35,38),(40,47),(58,64),(74,74),(87,87),(90,91),(93,96),(103,109),(112,113),(115,115),(117,117),(119,119),(121,1631),(1642,1775),(1786,1983),(1994,2405),(2416,2533),(2544,2661),(2672,2789),(2800,2917),(2928,3045),(3056,3173),(3184,3301),(3312,3429),(3440,3557),(3568,3663),(3674,3791),(3802,3871),(3882,4159),(4170,4239),(4250,6111),(6122,6159),(6170,6469),(6480,6607),(6618,6783),(6794,6799),(6810,6991),(7002,7087),(7098,7231),(7242,7247),(7258,42527),(42538,43215),(43226,43263),(43274,43471),(43482,43503),(43514,43599),(43610,44015),(44026,65295),(65306,66719),(66730,69733),(69744,69871),(69882,69941),(69952,70095),(70106,70383),(70394,70863),(70874,71247),(71258,71359),(71370,71903),(71914,92767),(92778,93007),(93018,120781),(120832,1114111)] then 1
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,48),(53,55)] then 4
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(56,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 9
            else if 99 <= c'' && c'' <= 101 then 36
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 3) -> 2
      (2, 0) -> 3
      (2, 1) -> 4
      (2, 2) -> 4
      (2, 4) -> 4
      (2, 5) -> 4
      (2, 6) -> 4
      (2, 7) -> 4
      (2, 8) -> 4
      (2, 9) -> 4
      (2, 10) -> 4
      (2, 11) -> 4
      (2, 12) -> 4
      (2, 13) -> 4
      (2, 14) -> 4
      (2, 15) -> 4
      (2, 16) -> 4
      (2, 17) -> 4
      (2, 18) -> 4
      (2, 19) -> 4
      (2, 20) -> 4
      (2, 21) -> 4
      (2, 22) -> 4
      (2, 23) -> 4
      (2, 24) -> 4
      (2, 25) -> 4
      (2, 26) -> 4
      (2, 27) -> 4
      (2, 28) -> 4
      (2, 29) -> 4
      (2, 30) -> 4
      (2, 31) -> 4
      (2, 32) -> 4
      (2, 33) -> 5
      (2, 34) -> 4
      (2, 35) -> 4
      (2, 36) -> 4
      (2, 37) -> 4
      (2, 38) -> 4
      (2, 39) -> 4
      (2, 40) -> 4
      (2, 41) -> 4
      (2, 42) -> 4
      (2, 43) -> 4
      (3, 0) -> 6
      (3, 2) -> 4
      (3, 3) -> 7
      (3, 4) -> 8
      (3, 5) -> 8
      (3, 6) -> 8
      (3, 7) -> 8
      (3, 8) -> 8
      (3, 9) -> 8
      (3, 10) -> 9
      (3, 11) -> 10
      (3, 12) -> 11
      (3, 13) -> 12
      (3, 14) -> 13
      (3, 15) -> 14
      (3, 16) -> 15
      (3, 17) -> 16
      (3, 20) -> 17
      (3, 22) -> 18
      (3, 26) -> 15
      (3, 27) -> 19
      (3, 29) -> 15
      (3, 30) -> 16
      (3, 33) -> 4
      (3, 34) -> 4
      (3, 35) -> 4
      (3, 37) -> 4
      (3, 38) -> 4
      (3, 39) -> 20
      (3, 40) -> 4
      (3, 41) -> 4
      (3, 42) -> 4
      (3, 43) -> 21
      (4, 0) -> 22
      (4, 3) -> 22
      (5, 0) -> 23
      (5, 2) -> 4
      (5, 3) -> 4
      (5, 4) -> 8
      (5, 5) -> 8
      (5, 6) -> 8
      (5, 7) -> 8
      (5, 8) -> 8
      (5, 9) -> 8
      (5, 10) -> 9
      (5, 11) -> 10
      (5, 12) -> 11
      (5, 13) -> 12
      (5, 14) -> 13
      (5, 15) -> 14
      (5, 16) -> 15
      (5, 17) -> 16
      (5, 20) -> 17
      (5, 22) -> 18
      (5, 26) -> 15
      (5, 27) -> 19
      (5, 29) -> 15
      (5, 30) -> 16
      (5, 33) -> 4
      (5, 34) -> 4
      (5, 35) -> 4
      (5, 37) -> 4
      (5, 38) -> 4
      (5, 39) -> 20
      (5, 40) -> 4
      (5, 41) -> 4
      (5, 42) -> 4
      (5, 43) -> 21
      (6, 0) -> 24
      (6, 3) -> 22
      (6, 4) -> 25
      (6, 5) -> 25
      (6, 6) -> 25
      (6, 7) -> 25
      (6, 8) -> 25
      (6, 9) -> 25
      (6, 10) -> 26
      (6, 11) -> 25
      (6, 12) -> 27
      (6, 13) -> 25
      (6, 14) -> 28
      (6, 15) -> 25
      (6, 18) -> 4
      (6, 20) -> 29
      (6, 21) -> 4
      (6, 22) -> 30
      (6, 23) -> 31
      (6, 24) -> 4
      (6, 26) -> 4
      (6, 27) -> 32
      (6, 28) -> 33
      (6, 29) -> 34
      (6, 32) -> 35
      (6, 34) -> 25
      (6, 35) -> 25
      (6, 36) -> 25
      (6, 37) -> 25
      (7, 0) -> 22
      (7, 3) -> 22
      (8, 0) -> 36
      (8, 3) -> 22
      (8, 4) -> 8
      (8, 5) -> 8
      (8, 6) -> 8
      (8, 7) -> 8
      (8, 8) -> 8
      (8, 9) -> 8
      (9, 0) -> 37
      (9, 12) -> 37
      (10, 0) -> 38
      (10, 14) -> 39
      (10, 27) -> 4
      (11, 0) -> 40
      (11, 10) -> 35
      (11, 26) -> 4
      (12, 0) -> 41
      (12, 12) -> 42
      (12, 14) -> 39
      (12, 20) -> 29
      (13, 0) -> 43
      (13, 21) -> 4
      (13, 22) -> 30
      (13, 23) -> 16
      (13, 27) -> 44
      (13, 28) -> 45
      (14, 0) -> 4
      (14, 15) -> 4
      (14, 27) -> 4
      (15, 0) -> 4
      (15, 27) -> 4
      (16, 0) -> 4
      (16, 28) -> 4
      (17, 0) -> 4
      (17, 15) -> 4
      (18, 0) -> 46
      (18, 10) -> 37
      (18, 29) -> 39
      (19, 0) -> 47
      (19, 18) -> 4
      (19, 23) -> 48
      (19, 24) -> 4
      (19, 28) -> 49
      (19, 29) -> 50
      (19, 32) -> 35
      (20, 0) -> 51
      (20, 4) -> 51
      (20, 5) -> 51
      (20, 6) -> 51
      (20, 7) -> 51
      (20, 8) -> 51
      (21, 0) -> 25
      (21, 4) -> 25
      (21, 5) -> 25
      (21, 6) -> 25
      (21, 7) -> 25
      (21, 8) -> 25
      (21, 9) -> 25
      (21, 10) -> 25
      (21, 11) -> 25
      (21, 12) -> 25
      (21, 13) -> 25
      (21, 14) -> 25
      (21, 15) -> 25
      (21, 34) -> 25
      (21, 35) -> 25
      (21, 36) -> 25
      (21, 37) -> 25
      (23, 0) -> 24
      (23, 3) -> 22
      (23, 4) -> 25
      (23, 5) -> 25
      (23, 6) -> 25
      (23, 7) -> 25
      (23, 8) -> 25
      (23, 9) -> 25
      (23, 10) -> 26
      (23, 11) -> 25
      (23, 12) -> 27
      (23, 13) -> 25
      (23, 14) -> 28
      (23, 15) -> 25
      (23, 18) -> 4
      (23, 20) -> 29
      (23, 21) -> 4
      (23, 22) -> 30
      (23, 23) -> 31
      (23, 24) -> 4
      (23, 26) -> 4
      (23, 27) -> 32
      (23, 28) -> 33
      (23, 29) -> 34
      (23, 32) -> 35
      (23, 34) -> 25
      (23, 35) -> 25
      (23, 36) -> 25
      (23, 37) -> 25
      (24, 0) -> 52
      (24, 3) -> 22
      (24, 4) -> 25
      (24, 5) -> 25
      (24, 6) -> 25
      (24, 7) -> 25
      (24, 8) -> 25
      (24, 9) -> 25
      (24, 10) -> 25
      (24, 11) -> 25
      (24, 12) -> 25
      (24, 13) -> 25
      (24, 14) -> 25
      (24, 15) -> 25
      (24, 17) -> 4
      (24, 19) -> 4
      (24, 20) -> 4
      (24, 22) -> 4
      (24, 25) -> 4
      (24, 28) -> 4
      (24, 31) -> 4
      (24, 34) -> 25
      (24, 35) -> 25
      (24, 36) -> 25
      (24, 37) -> 25
      (25, 0) -> 52
      (25, 3) -> 22
      (25, 4) -> 25
      (25, 5) -> 25
      (25, 6) -> 25
      (25, 7) -> 25
      (25, 8) -> 25
      (25, 9) -> 25
      (25, 10) -> 25
      (25, 11) -> 25
      (25, 12) -> 25
      (25, 13) -> 25
      (25, 14) -> 25
      (25, 15) -> 25
      (25, 34) -> 25
      (25, 35) -> 25
      (25, 36) -> 25
      (25, 37) -> 25
      (26, 0) -> 52
      (26, 3) -> 22
      (26, 4) -> 25
      (26, 5) -> 25
      (26, 6) -> 25
      (26, 7) -> 25
      (26, 8) -> 25
      (26, 9) -> 25
      (26, 10) -> 25
      (26, 11) -> 25
      (26, 12) -> 25
      (26, 13) -> 25
      (26, 14) -> 25
      (26, 15) -> 25
      (26, 19) -> 4
      (26, 22) -> 4
      (26, 34) -> 25
      (26, 35) -> 25
      (26, 36) -> 25
      (26, 37) -> 25
      (27, 0) -> 52
      (27, 3) -> 22
      (27, 4) -> 25
      (27, 5) -> 25
      (27, 6) -> 25
      (27, 7) -> 25
      (27, 8) -> 25
      (27, 9) -> 25
      (27, 10) -> 25
      (27, 11) -> 25
      (27, 12) -> 25
      (27, 13) -> 25
      (27, 14) -> 25
      (27, 15) -> 25
      (27, 19) -> 4
      (27, 34) -> 25
      (27, 35) -> 25
      (27, 36) -> 25
      (27, 37) -> 25
      (28, 0) -> 52
      (28, 3) -> 22
      (28, 4) -> 25
      (28, 5) -> 25
      (28, 6) -> 25
      (28, 7) -> 25
      (28, 8) -> 25
      (28, 9) -> 25
      (28, 10) -> 25
      (28, 11) -> 25
      (28, 12) -> 25
      (28, 13) -> 25
      (28, 14) -> 25
      (28, 15) -> 25
      (28, 20) -> 4
      (28, 34) -> 25
      (28, 35) -> 25
      (28, 36) -> 25
      (28, 37) -> 25
      (29, 0) -> 4
      (29, 14) -> 4
      (30, 0) -> 4
      (30, 25) -> 4
      (31, 0) -> 7
      (31, 3) -> 22
      (31, 17) -> 4
      (31, 28) -> 4
      (32, 0) -> 7
      (32, 3) -> 22
      (32, 12) -> 4
      (33, 0) -> 7
      (33, 3) -> 22
      (33, 11) -> 4
      (33, 31) -> 4
      (34, 0) -> 4
      (34, 11) -> 4
      (34, 20) -> 4
      (35, 0) -> 4
      (35, 22) -> 4
      (36, 0) -> 36
      (36, 3) -> 22
      (36, 4) -> 8
      (36, 5) -> 8
      (36, 6) -> 8
      (36, 7) -> 8
      (36, 8) -> 8
      (36, 9) -> 8
      (37, 0) -> 4
      (37, 19) -> 4
      (38, 0) -> 7
      (38, 3) -> 22
      (38, 20) -> 4
      (39, 0) -> 4
      (39, 20) -> 4
      (40, 0) -> 7
      (40, 3) -> 22
      (40, 22) -> 4
      (41, 0) -> 4
      (41, 5) -> 4
      (41, 6) -> 4
      (41, 7) -> 4
      (41, 8) -> 4
      (41, 14) -> 4
      (41, 20) -> 4
      (42, 0) -> 4
      (42, 5) -> 4
      (42, 6) -> 4
      (42, 7) -> 4
      (42, 8) -> 4
      (43, 0) -> 7
      (43, 3) -> 22
      (43, 11) -> 4
      (43, 12) -> 4
      (43, 25) -> 4
      (43, 28) -> 4
      (43, 31) -> 4
      (44, 0) -> 4
      (44, 12) -> 4
      (45, 0) -> 4
      (45, 11) -> 4
      (45, 31) -> 4
      (46, 0) -> 4
      (46, 19) -> 4
      (46, 20) -> 4
      (47, 0) -> 7
      (47, 3) -> 22
      (47, 11) -> 4
      (47, 17) -> 4
      (47, 22) -> 4
      (47, 31) -> 4
      (48, 0) -> 7
      (48, 3) -> 22
      (48, 17) -> 4
      (49, 0) -> 4
      (49, 31) -> 4
      (50, 0) -> 4
      (50, 11) -> 4
      (51, 0) -> 53
      (51, 3) -> 22
      (51, 4) -> 51
      (51, 5) -> 51
      (51, 6) -> 51
      (51, 7) -> 51
      (51, 8) -> 51
      (52, 0) -> 52
      (52, 3) -> 22
      (52, 4) -> 25
      (52, 5) -> 25
      (52, 6) -> 25
      (52, 7) -> 25
      (52, 8) -> 25
      (52, 9) -> 25
      (52, 10) -> 25
      (52, 11) -> 25
      (52, 12) -> 25
      (52, 13) -> 25
      (52, 14) -> 25
      (52, 15) -> 25
      (52, 34) -> 25
      (52, 35) -> 25
      (52, 36) -> 25
      (52, 37) -> 25
      (53, 0) -> 53
      (53, 3) -> 22
      (53, 4) -> 51
      (53, 5) -> 51
      (53, 6) -> 51
      (53, 7) -> 51
      (53, 8) -> 51
      _ -> 0

dfa47InitialState :: Int
dfa47InitialState = 1

dfa47FinalStates :: [Int]
dfa47FinalStates = [3]

dfa47Transition :: Int -> Char -> Int
dfa47Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          123 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa48InitialState :: Int
dfa48InitialState = 1

dfa48FinalStates :: [Int]
dfa48FinalStates = [8]

dfa48Transition :: Int -> Char -> Int
dfa48Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          100 -> 2
          101 -> 3
          108 -> 4
          109 -> 5
          111 -> 6
          117 -> 7
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 5) -> 3
      (3, 6) -> 4
      (4, 2) -> 5
      (5, 7) -> 6
      (6, 4) -> 7
      (7, 3) -> 8
      _ -> 0

dfa49InitialState :: Int
dfa49InitialState = 1

dfa49FinalStates :: [Int]
dfa49FinalStates = [3]

dfa49Transition :: Int -> Char -> Int
dfa49Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 1) -> 3
      _ -> 0

dfa50InitialState :: Int
dfa50InitialState = 1

dfa50FinalStates :: [Int]
dfa50FinalStates = [2]

dfa50Transition :: Int -> Char -> Int
dfa50Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          124 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa51InitialState :: Int
dfa51InitialState = 1

dfa51FinalStates :: [Int]
dfa51FinalStates = [7]

dfa51Transition :: Int -> Char -> Int
dfa51Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          101 -> 2
          104 -> 3
          114 -> 4
          119 -> 5
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 5) -> 3
      (3, 3) -> 4
      (4, 2) -> 5
      (5, 4) -> 6
      (6, 2) -> 7
      _ -> 0

dfa52InitialState :: Int
dfa52InitialState = 1

dfa52FinalStates :: [Int]
dfa52FinalStates = [3]

dfa52Transition :: Int -> Char -> Int
dfa52Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          125 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
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
          39 -> 1
          95 -> 4
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 2
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(65,90),(192,214),(216,222),(256,256),(258,258),(260,260),(262,262),(264,264),(266,266),(268,268),(270,270),(272,272),(274,274),(276,276),(278,278),(280,280),(282,282),(284,284),(286,286),(288,288),(290,290),(292,292),(294,294),(296,296),(298,298),(300,300),(302,302),(304,304),(306,306),(308,308),(310,310),(313,313),(315,315),(317,317),(319,319),(321,321),(323,323),(325,325),(327,327),(330,330),(332,332),(334,334),(336,336),(338,338),(340,340),(342,342),(344,344),(346,346),(348,348),(350,350),(352,352),(354,354),(356,356),(358,358),(360,360),(362,362),(364,364),(366,366),(368,368),(370,370),(372,372),(374,374),(376,377),(379,379),(381,381),(385,386),(388,388),(390,391),(393,395),(398,401),(403,404),(406,408),(412,413),(415,416),(418,418),(420,420),(422,423),(425,425),(428,428),(430,431),(433,435),(437,437),(439,440),(444,444),(452,452),(455,455),(458,458),(461,461),(463,463),(465,465),(467,467),(469,469),(471,471),(473,473),(475,475),(478,478),(480,480),(482,482),(484,484),(486,486),(488,488),(490,490),(492,492),(494,494),(497,497),(500,500),(502,504),(506,506),(508,508),(510,510),(512,512),(514,514),(516,516),(518,518),(520,520),(522,522),(524,524),(526,526),(528,528),(530,530),(532,532),(534,534),(536,536),(538,538),(540,540),(542,542),(544,544),(546,546),(548,548),(550,550),(552,552),(554,554),(556,556),(558,558),(560,560),(562,562),(570,571),(573,574),(577,577),(579,582),(584,584),(586,586),(588,588),(590,590),(880,880),(882,882),(886,886),(895,895),(902,902),(904,906),(908,908),(910,911),(913,929),(931,939),(975,975),(978,980),(984,984),(986,986),(988,988),(990,990),(992,992),(994,994),(996,996),(998,998),(1000,1000),(1002,1002),(1004,1004),(1006,1006),(1012,1012),(1015,1015),(1017,1018),(1021,1071),(1120,1120),(1122,1122),(1124,1124),(1126,1126),(1128,1128),(1130,1130),(1132,1132),(1134,1134),(1136,1136),(1138,1138),(1140,1140),(1142,1142),(1144,1144),(1146,1146),(1148,1148),(1150,1150),(1152,1152),(1162,1162),(1164,1164),(1166,1166),(1168,1168),(1170,1170),(1172,1172),(1174,1174),(1176,1176),(1178,1178),(1180,1180),(1182,1182),(1184,1184),(1186,1186),(1188,1188),(1190,1190),(1192,1192),(1194,1194),(1196,1196),(1198,1198),(1200,1200),(1202,1202),(1204,1204),(1206,1206),(1208,1208),(1210,1210),(1212,1212),(1214,1214),(1216,1217),(1219,1219),(1221,1221),(1223,1223),(1225,1225),(1227,1227),(1229,1229),(1232,1232),(1234,1234),(1236,1236),(1238,1238),(1240,1240),(1242,1242),(1244,1244),(1246,1246),(1248,1248),(1250,1250),(1252,1252),(1254,1254),(1256,1256),(1258,1258),(1260,1260),(1262,1262),(1264,1264),(1266,1266),(1268,1268),(1270,1270),(1272,1272),(1274,1274),(1276,1276),(1278,1278),(1280,1280),(1282,1282),(1284,1284),(1286,1286),(1288,1288),(1290,1290),(1292,1292),(1294,1294),(1296,1296),(1298,1298),(1300,1300),(1302,1302),(1304,1304),(1306,1306),(1308,1308),(1310,1310),(1312,1312),(1314,1314),(1316,1316),(1318,1318),(1320,1320),(1322,1322),(1324,1324),(1326,1326),(1329,1366),(4256,4293),(4295,4295),(4301,4301),(7680,7680),(7682,7682),(7684,7684),(7686,7686),(7688,7688),(7690,7690),(7692,7692),(7694,7694),(7696,7696),(7698,7698),(7700,7700),(7702,7702),(7704,7704),(7706,7706),(7708,7708),(7710,7710),(7712,7712),(7714,7714),(7716,7716),(7718,7718),(7720,7720),(7722,7722),(7724,7724),(7726,7726),(7728,7728),(7730,7730),(7732,7732),(7734,7734),(7736,7736),(7738,7738),(7740,7740),(7742,7742),(7744,7744),(7746,7746),(7748,7748),(7750,7750),(7752,7752),(7754,7754),(7756,7756),(7758,7758),(7760,7760),(7762,7762),(7764,7764),(7766,7766),(7768,7768),(7770,7770),(7772,7772),(7774,7774),(7776,7776),(7778,7778),(7780,7780),(7782,7782),(7784,7784),(7786,7786),(7788,7788),(7790,7790),(7792,7792),(7794,7794),(7796,7796),(7798,7798),(7800,7800),(7802,7802),(7804,7804),(7806,7806),(7808,7808),(7810,7810),(7812,7812),(7814,7814),(7816,7816),(7818,7818),(7820,7820),(7822,7822),(7824,7824),(7826,7826),(7828,7828),(7838,7838),(7840,7840),(7842,7842),(7844,7844),(7846,7846),(7848,7848),(7850,7850),(7852,7852),(7854,7854),(7856,7856),(7858,7858),(7860,7860),(7862,7862),(7864,7864),(7866,7866),(7868,7868),(7870,7870),(7872,7872),(7874,7874),(7876,7876),(7878,7878),(7880,7880),(7882,7882),(7884,7884),(7886,7886),(7888,7888),(7890,7890),(7892,7892),(7894,7894),(7896,7896),(7898,7898),(7900,7900),(7902,7902),(7904,7904),(7906,7906),(7908,7908),(7910,7910),(7912,7912),(7914,7914),(7916,7916),(7918,7918),(7920,7920),(7922,7922),(7924,7924),(7926,7926),(7928,7928),(7930,7930),(7932,7932),(7934,7934),(7944,7951),(7960,7965),(7976,7983),(7992,7999),(8008,8013),(8025,8025),(8027,8027),(8029,8029),(8031,8031),(8040,8047),(8120,8123),(8136,8139),(8152,8155),(8168,8172),(8184,8187),(8450,8450),(8455,8455),(8459,8461),(8464,8466),(8469,8469),(8473,8477),(8484,8484),(8486,8486),(8488,8488),(8490,8493),(8496,8499),(8510,8511),(8517,8517),(8579,8579),(11264,11310),(11360,11360),(11362,11364),(11367,11367),(11369,11369),(11371,11371),(11373,11376),(11378,11378),(11381,11381),(11390,11392),(11394,11394),(11396,11396),(11398,11398),(11400,11400),(11402,11402),(11404,11404),(11406,11406),(11408,11408),(11410,11410),(11412,11412),(11414,11414),(11416,11416),(11418,11418),(11420,11420),(11422,11422),(11424,11424),(11426,11426),(11428,11428),(11430,11430),(11432,11432),(11434,11434),(11436,11436),(11438,11438),(11440,11440),(11442,11442),(11444,11444),(11446,11446),(11448,11448),(11450,11450),(11452,11452),(11454,11454),(11456,11456),(11458,11458),(11460,11460),(11462,11462),(11464,11464),(11466,11466),(11468,11468),(11470,11470),(11472,11472),(11474,11474),(11476,11476),(11478,11478),(11480,11480),(11482,11482),(11484,11484),(11486,11486),(11488,11488),(11490,11490),(11499,11499),(11501,11501),(11506,11506),(42560,42560),(42562,42562),(42564,42564),(42566,42566),(42568,42568),(42570,42570),(42572,42572),(42574,42574),(42576,42576),(42578,42578),(42580,42580),(42582,42582),(42584,42584),(42586,42586),(42588,42588),(42590,42590),(42592,42592),(42594,42594),(42596,42596),(42598,42598),(42600,42600),(42602,42602),(42604,42604),(42624,42624),(42626,42626),(42628,42628),(42630,42630),(42632,42632),(42634,42634),(42636,42636),(42638,42638),(42640,42640),(42642,42642),(42644,42644),(42646,42646),(42648,42648),(42650,42650),(42786,42786),(42788,42788),(42790,42790),(42792,42792),(42794,42794),(42796,42796),(42798,42798),(42802,42802),(42804,42804),(42806,42806),(42808,42808),(42810,42810),(42812,42812),(42814,42814),(42816,42816),(42818,42818),(42820,42820),(42822,42822),(42824,42824),(42826,42826),(42828,42828),(42830,42830),(42832,42832),(42834,42834),(42836,42836),(42838,42838),(42840,42840),(42842,42842),(42844,42844),(42846,42846),(42848,42848),(42850,42850),(42852,42852),(42854,42854),(42856,42856),(42858,42858),(42860,42860),(42862,42862),(42873,42873),(42875,42875),(42877,42878),(42880,42880),(42882,42882),(42884,42884),(42886,42886),(42891,42891),(42893,42893),(42896,42896),(42898,42898),(42902,42902),(42904,42904),(42906,42906),(42908,42908),(42910,42910),(42912,42912),(42914,42914),(42916,42916),(42918,42918),(42920,42920),(42922,42925),(42928,42929),(65313,65338),(66560,66599),(71840,71871),(119808,119833),(119860,119885),(119912,119937),(119964,119964),(119966,119967),(119970,119970),(119973,119974),(119977,119980),(119982,119989),(120016,120041),(120068,120069),(120071,120074),(120077,120084),(120086,120092),(120120,120121),(120123,120126),(120128,120132),(120134,120134),(120138,120144),(120172,120197),(120224,120249),(120276,120301),(120328,120353),(120380,120405),(120432,120457),(120488,120512),(120546,120570),(120604,120628),(120662,120686),(120720,120744),(120778,120778)] then 3
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,122),(181,181),(223,246),(248,255),(257,257),(259,259),(261,261),(263,263),(265,265),(267,267),(269,269),(271,271),(273,273),(275,275),(277,277),(279,279),(281,281),(283,283),(285,285),(287,287),(289,289),(291,291),(293,293),(295,295),(297,297),(299,299),(301,301),(303,303),(305,305),(307,307),(309,309),(311,312),(314,314),(316,316),(318,318),(320,320),(322,322),(324,324),(326,326),(328,329),(331,331),(333,333),(335,335),(337,337),(339,339),(341,341),(343,343),(345,345),(347,347),(349,349),(351,351),(353,353),(355,355),(357,357),(359,359),(361,361),(363,363),(365,365),(367,367),(369,369),(371,371),(373,373),(375,375),(378,378),(380,380),(382,384),(387,387),(389,389),(392,392),(396,397),(402,402),(405,405),(409,411),(414,414),(417,417),(419,419),(421,421),(424,424),(426,427),(429,429),(432,432),(436,436),(438,438),(441,442),(445,447),(454,454),(457,457),(460,460),(462,462),(464,464),(466,466),(468,468),(470,470),(472,472),(474,474),(476,477),(479,479),(481,481),(483,483),(485,485),(487,487),(489,489),(491,491),(493,493),(495,496),(499,499),(501,501),(505,505),(507,507),(509,509),(511,511),(513,513),(515,515),(517,517),(519,519),(521,521),(523,523),(525,525),(527,527),(529,529),(531,531),(533,533),(535,535),(537,537),(539,539),(541,541),(543,543),(545,545),(547,547),(549,549),(551,551),(553,553),(555,555),(557,557),(559,559),(561,561),(563,569),(572,572),(575,576),(578,578),(583,583),(585,585),(587,587),(589,589),(591,659),(661,687),(881,881),(883,883),(887,887),(891,893),(912,912),(940,974),(976,977),(981,983),(985,985),(987,987),(989,989),(991,991),(993,993),(995,995),(997,997),(999,999),(1001,1001),(1003,1003),(1005,1005),(1007,1011),(1013,1013),(1016,1016),(1019,1020),(1072,1119),(1121,1121),(1123,1123),(1125,1125),(1127,1127),(1129,1129),(1131,1131),(1133,1133),(1135,1135),(1137,1137),(1139,1139),(1141,1141),(1143,1143),(1145,1145),(1147,1147),(1149,1149),(1151,1151),(1153,1153),(1163,1163),(1165,1165),(1167,1167),(1169,1169),(1171,1171),(1173,1173),(1175,1175),(1177,1177),(1179,1179),(1181,1181),(1183,1183),(1185,1185),(1187,1187),(1189,1189),(1191,1191),(1193,1193),(1195,1195),(1197,1197),(1199,1199),(1201,1201),(1203,1203),(1205,1205),(1207,1207),(1209,1209),(1211,1211),(1213,1213),(1215,1215),(1218,1218),(1220,1220),(1222,1222),(1224,1224),(1226,1226),(1228,1228),(1230,1231),(1233,1233),(1235,1235),(1237,1237),(1239,1239),(1241,1241),(1243,1243),(1245,1245),(1247,1247),(1249,1249),(1251,1251),(1253,1253),(1255,1255),(1257,1257),(1259,1259),(1261,1261),(1263,1263),(1265,1265),(1267,1267),(1269,1269),(1271,1271),(1273,1273),(1275,1275),(1277,1277),(1279,1279),(1281,1281),(1283,1283),(1285,1285),(1287,1287),(1289,1289),(1291,1291),(1293,1293),(1295,1295),(1297,1297),(1299,1299),(1301,1301),(1303,1303),(1305,1305),(1307,1307),(1309,1309),(1311,1311),(1313,1313),(1315,1315),(1317,1317),(1319,1319),(1321,1321),(1323,1323),(1325,1325),(1327,1327),(1377,1415),(7424,7467),(7531,7543),(7545,7578),(7681,7681),(7683,7683),(7685,7685),(7687,7687),(7689,7689),(7691,7691),(7693,7693),(7695,7695),(7697,7697),(7699,7699),(7701,7701),(7703,7703),(7705,7705),(7707,7707),(7709,7709),(7711,7711),(7713,7713),(7715,7715),(7717,7717),(7719,7719),(7721,7721),(7723,7723),(7725,7725),(7727,7727),(7729,7729),(7731,7731),(7733,7733),(7735,7735),(7737,7737),(7739,7739),(7741,7741),(7743,7743),(7745,7745),(7747,7747),(7749,7749),(7751,7751),(7753,7753),(7755,7755),(7757,7757),(7759,7759),(7761,7761),(7763,7763),(7765,7765),(7767,7767),(7769,7769),(7771,7771),(7773,7773),(7775,7775),(7777,7777),(7779,7779),(7781,7781),(7783,7783),(7785,7785),(7787,7787),(7789,7789),(7791,7791),(7793,7793),(7795,7795),(7797,7797),(7799,7799),(7801,7801),(7803,7803),(7805,7805),(7807,7807),(7809,7809),(7811,7811),(7813,7813),(7815,7815),(7817,7817),(7819,7819),(7821,7821),(7823,7823),(7825,7825),(7827,7827),(7829,7837),(7839,7839),(7841,7841),(7843,7843),(7845,7845),(7847,7847),(7849,7849),(7851,7851),(7853,7853),(7855,7855),(7857,7857),(7859,7859),(7861,7861),(7863,7863),(7865,7865),(7867,7867),(7869,7869),(7871,7871),(7873,7873),(7875,7875),(7877,7877),(7879,7879),(7881,7881),(7883,7883),(7885,7885),(7887,7887),(7889,7889),(7891,7891),(7893,7893),(7895,7895),(7897,7897),(7899,7899),(7901,7901),(7903,7903),(7905,7905),(7907,7907),(7909,7909),(7911,7911),(7913,7913),(7915,7915),(7917,7917),(7919,7919),(7921,7921),(7923,7923),(7925,7925),(7927,7927),(7929,7929),(7931,7931),(7933,7933),(7935,7943),(7952,7957),(7968,7975),(7984,7991),(8000,8005),(8016,8023),(8032,8039),(8048,8061),(8064,8071),(8080,8087),(8096,8103),(8112,8116),(8118,8119),(8126,8126),(8130,8132),(8134,8135),(8144,8147),(8150,8151),(8160,8167),(8178,8180),(8182,8183),(8458,8458),(8462,8463),(8467,8467),(8495,8495),(8500,8500),(8505,8505),(8508,8509),(8518,8521),(8526,8526),(8580,8580),(11312,11358),(11361,11361),(11365,11366),(11368,11368),(11370,11370),(11372,11372),(11377,11377),(11379,11380),(11382,11387),(11393,11393),(11395,11395),(11397,11397),(11399,11399),(11401,11401),(11403,11403),(11405,11405),(11407,11407),(11409,11409),(11411,11411),(11413,11413),(11415,11415),(11417,11417),(11419,11419),(11421,11421),(11423,11423),(11425,11425),(11427,11427),(11429,11429),(11431,11431),(11433,11433),(11435,11435),(11437,11437),(11439,11439),(11441,11441),(11443,11443),(11445,11445),(11447,11447),(11449,11449),(11451,11451),(11453,11453),(11455,11455),(11457,11457),(11459,11459),(11461,11461),(11463,11463),(11465,11465),(11467,11467),(11469,11469),(11471,11471),(11473,11473),(11475,11475),(11477,11477),(11479,11479),(11481,11481),(11483,11483),(11485,11485),(11487,11487),(11489,11489),(11491,11492),(11500,11500),(11502,11502),(11507,11507),(11520,11557),(11559,11559),(11565,11565),(42561,42561),(42563,42563),(42565,42565),(42567,42567),(42569,42569),(42571,42571),(42573,42573),(42575,42575),(42577,42577),(42579,42579),(42581,42581),(42583,42583),(42585,42585),(42587,42587),(42589,42589),(42591,42591),(42593,42593),(42595,42595),(42597,42597),(42599,42599),(42601,42601),(42603,42603),(42605,42605),(42625,42625),(42627,42627),(42629,42629),(42631,42631),(42633,42633),(42635,42635),(42637,42637),(42639,42639),(42641,42641),(42643,42643),(42645,42645),(42647,42647),(42649,42649),(42651,42651),(42787,42787),(42789,42789),(42791,42791),(42793,42793),(42795,42795),(42797,42797),(42799,42801),(42803,42803),(42805,42805),(42807,42807),(42809,42809),(42811,42811),(42813,42813),(42815,42815),(42817,42817),(42819,42819),(42821,42821),(42823,42823),(42825,42825),(42827,42827),(42829,42829),(42831,42831),(42833,42833),(42835,42835),(42837,42837),(42839,42839),(42841,42841),(42843,42843),(42845,42845),(42847,42847),(42849,42849),(42851,42851),(42853,42853),(42855,42855),(42857,42857),(42859,42859),(42861,42861),(42863,42863),(42865,42872),(42874,42874),(42876,42876),(42879,42879),(42881,42881),(42883,42883),(42885,42885),(42887,42887),(42892,42892),(42894,42894),(42897,42897),(42899,42901),(42903,42903),(42905,42905),(42907,42907),(42909,42909),(42911,42911),(42913,42913),(42915,42915),(42917,42917),(42919,42919),(42921,42921),(43002,43002),(43824,43866),(43876,43877),(64256,64262),(64275,64279),(65345,65370),(66600,66639),(71872,71903),(119834,119859),(119886,119892),(119894,119911),(119938,119963),(119990,119993),(119995,119995),(119997,120003),(120005,120015),(120042,120067),(120094,120119),(120146,120171),(120198,120223),(120250,120275),(120302,120327),(120354,120379),(120406,120431),(120458,120485),(120514,120538),(120540,120545),(120572,120596),(120598,120603),(120630,120654),(120656,120661),(120688,120712),(120714,120719),(120746,120770),(120772,120777),(120779,120779)] then 5
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(453,453),(456,456),(459,459),(498,498),(8072,8079),(8088,8095),(8104,8111),(8124,8124),(8140,8140),(8188,8188)] then 6
            else 0 in
    case (q, c') of
      (1, 4) -> 2
      (1, 5) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
      _ -> 0

dfa54InitialState :: Int
dfa54InitialState = 1

dfa54FinalStates :: [Int]
dfa54FinalStates = [2]

dfa54Transition :: Int -> Char -> Int
dfa54Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          39 -> 1
          95 -> 4
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 2
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(65,90),(192,214),(216,222),(256,256),(258,258),(260,260),(262,262),(264,264),(266,266),(268,268),(270,270),(272,272),(274,274),(276,276),(278,278),(280,280),(282,282),(284,284),(286,286),(288,288),(290,290),(292,292),(294,294),(296,296),(298,298),(300,300),(302,302),(304,304),(306,306),(308,308),(310,310),(313,313),(315,315),(317,317),(319,319),(321,321),(323,323),(325,325),(327,327),(330,330),(332,332),(334,334),(336,336),(338,338),(340,340),(342,342),(344,344),(346,346),(348,348),(350,350),(352,352),(354,354),(356,356),(358,358),(360,360),(362,362),(364,364),(366,366),(368,368),(370,370),(372,372),(374,374),(376,377),(379,379),(381,381),(385,386),(388,388),(390,391),(393,395),(398,401),(403,404),(406,408),(412,413),(415,416),(418,418),(420,420),(422,423),(425,425),(428,428),(430,431),(433,435),(437,437),(439,440),(444,444),(452,452),(455,455),(458,458),(461,461),(463,463),(465,465),(467,467),(469,469),(471,471),(473,473),(475,475),(478,478),(480,480),(482,482),(484,484),(486,486),(488,488),(490,490),(492,492),(494,494),(497,497),(500,500),(502,504),(506,506),(508,508),(510,510),(512,512),(514,514),(516,516),(518,518),(520,520),(522,522),(524,524),(526,526),(528,528),(530,530),(532,532),(534,534),(536,536),(538,538),(540,540),(542,542),(544,544),(546,546),(548,548),(550,550),(552,552),(554,554),(556,556),(558,558),(560,560),(562,562),(570,571),(573,574),(577,577),(579,582),(584,584),(586,586),(588,588),(590,590),(880,880),(882,882),(886,886),(895,895),(902,902),(904,906),(908,908),(910,911),(913,929),(931,939),(975,975),(978,980),(984,984),(986,986),(988,988),(990,990),(992,992),(994,994),(996,996),(998,998),(1000,1000),(1002,1002),(1004,1004),(1006,1006),(1012,1012),(1015,1015),(1017,1018),(1021,1071),(1120,1120),(1122,1122),(1124,1124),(1126,1126),(1128,1128),(1130,1130),(1132,1132),(1134,1134),(1136,1136),(1138,1138),(1140,1140),(1142,1142),(1144,1144),(1146,1146),(1148,1148),(1150,1150),(1152,1152),(1162,1162),(1164,1164),(1166,1166),(1168,1168),(1170,1170),(1172,1172),(1174,1174),(1176,1176),(1178,1178),(1180,1180),(1182,1182),(1184,1184),(1186,1186),(1188,1188),(1190,1190),(1192,1192),(1194,1194),(1196,1196),(1198,1198),(1200,1200),(1202,1202),(1204,1204),(1206,1206),(1208,1208),(1210,1210),(1212,1212),(1214,1214),(1216,1217),(1219,1219),(1221,1221),(1223,1223),(1225,1225),(1227,1227),(1229,1229),(1232,1232),(1234,1234),(1236,1236),(1238,1238),(1240,1240),(1242,1242),(1244,1244),(1246,1246),(1248,1248),(1250,1250),(1252,1252),(1254,1254),(1256,1256),(1258,1258),(1260,1260),(1262,1262),(1264,1264),(1266,1266),(1268,1268),(1270,1270),(1272,1272),(1274,1274),(1276,1276),(1278,1278),(1280,1280),(1282,1282),(1284,1284),(1286,1286),(1288,1288),(1290,1290),(1292,1292),(1294,1294),(1296,1296),(1298,1298),(1300,1300),(1302,1302),(1304,1304),(1306,1306),(1308,1308),(1310,1310),(1312,1312),(1314,1314),(1316,1316),(1318,1318),(1320,1320),(1322,1322),(1324,1324),(1326,1326),(1329,1366),(4256,4293),(4295,4295),(4301,4301),(7680,7680),(7682,7682),(7684,7684),(7686,7686),(7688,7688),(7690,7690),(7692,7692),(7694,7694),(7696,7696),(7698,7698),(7700,7700),(7702,7702),(7704,7704),(7706,7706),(7708,7708),(7710,7710),(7712,7712),(7714,7714),(7716,7716),(7718,7718),(7720,7720),(7722,7722),(7724,7724),(7726,7726),(7728,7728),(7730,7730),(7732,7732),(7734,7734),(7736,7736),(7738,7738),(7740,7740),(7742,7742),(7744,7744),(7746,7746),(7748,7748),(7750,7750),(7752,7752),(7754,7754),(7756,7756),(7758,7758),(7760,7760),(7762,7762),(7764,7764),(7766,7766),(7768,7768),(7770,7770),(7772,7772),(7774,7774),(7776,7776),(7778,7778),(7780,7780),(7782,7782),(7784,7784),(7786,7786),(7788,7788),(7790,7790),(7792,7792),(7794,7794),(7796,7796),(7798,7798),(7800,7800),(7802,7802),(7804,7804),(7806,7806),(7808,7808),(7810,7810),(7812,7812),(7814,7814),(7816,7816),(7818,7818),(7820,7820),(7822,7822),(7824,7824),(7826,7826),(7828,7828),(7838,7838),(7840,7840),(7842,7842),(7844,7844),(7846,7846),(7848,7848),(7850,7850),(7852,7852),(7854,7854),(7856,7856),(7858,7858),(7860,7860),(7862,7862),(7864,7864),(7866,7866),(7868,7868),(7870,7870),(7872,7872),(7874,7874),(7876,7876),(7878,7878),(7880,7880),(7882,7882),(7884,7884),(7886,7886),(7888,7888),(7890,7890),(7892,7892),(7894,7894),(7896,7896),(7898,7898),(7900,7900),(7902,7902),(7904,7904),(7906,7906),(7908,7908),(7910,7910),(7912,7912),(7914,7914),(7916,7916),(7918,7918),(7920,7920),(7922,7922),(7924,7924),(7926,7926),(7928,7928),(7930,7930),(7932,7932),(7934,7934),(7944,7951),(7960,7965),(7976,7983),(7992,7999),(8008,8013),(8025,8025),(8027,8027),(8029,8029),(8031,8031),(8040,8047),(8120,8123),(8136,8139),(8152,8155),(8168,8172),(8184,8187),(8450,8450),(8455,8455),(8459,8461),(8464,8466),(8469,8469),(8473,8477),(8484,8484),(8486,8486),(8488,8488),(8490,8493),(8496,8499),(8510,8511),(8517,8517),(8579,8579),(11264,11310),(11360,11360),(11362,11364),(11367,11367),(11369,11369),(11371,11371),(11373,11376),(11378,11378),(11381,11381),(11390,11392),(11394,11394),(11396,11396),(11398,11398),(11400,11400),(11402,11402),(11404,11404),(11406,11406),(11408,11408),(11410,11410),(11412,11412),(11414,11414),(11416,11416),(11418,11418),(11420,11420),(11422,11422),(11424,11424),(11426,11426),(11428,11428),(11430,11430),(11432,11432),(11434,11434),(11436,11436),(11438,11438),(11440,11440),(11442,11442),(11444,11444),(11446,11446),(11448,11448),(11450,11450),(11452,11452),(11454,11454),(11456,11456),(11458,11458),(11460,11460),(11462,11462),(11464,11464),(11466,11466),(11468,11468),(11470,11470),(11472,11472),(11474,11474),(11476,11476),(11478,11478),(11480,11480),(11482,11482),(11484,11484),(11486,11486),(11488,11488),(11490,11490),(11499,11499),(11501,11501),(11506,11506),(42560,42560),(42562,42562),(42564,42564),(42566,42566),(42568,42568),(42570,42570),(42572,42572),(42574,42574),(42576,42576),(42578,42578),(42580,42580),(42582,42582),(42584,42584),(42586,42586),(42588,42588),(42590,42590),(42592,42592),(42594,42594),(42596,42596),(42598,42598),(42600,42600),(42602,42602),(42604,42604),(42624,42624),(42626,42626),(42628,42628),(42630,42630),(42632,42632),(42634,42634),(42636,42636),(42638,42638),(42640,42640),(42642,42642),(42644,42644),(42646,42646),(42648,42648),(42650,42650),(42786,42786),(42788,42788),(42790,42790),(42792,42792),(42794,42794),(42796,42796),(42798,42798),(42802,42802),(42804,42804),(42806,42806),(42808,42808),(42810,42810),(42812,42812),(42814,42814),(42816,42816),(42818,42818),(42820,42820),(42822,42822),(42824,42824),(42826,42826),(42828,42828),(42830,42830),(42832,42832),(42834,42834),(42836,42836),(42838,42838),(42840,42840),(42842,42842),(42844,42844),(42846,42846),(42848,42848),(42850,42850),(42852,42852),(42854,42854),(42856,42856),(42858,42858),(42860,42860),(42862,42862),(42873,42873),(42875,42875),(42877,42878),(42880,42880),(42882,42882),(42884,42884),(42886,42886),(42891,42891),(42893,42893),(42896,42896),(42898,42898),(42902,42902),(42904,42904),(42906,42906),(42908,42908),(42910,42910),(42912,42912),(42914,42914),(42916,42916),(42918,42918),(42920,42920),(42922,42925),(42928,42929),(65313,65338),(66560,66599),(71840,71871),(119808,119833),(119860,119885),(119912,119937),(119964,119964),(119966,119967),(119970,119970),(119973,119974),(119977,119980),(119982,119989),(120016,120041),(120068,120069),(120071,120074),(120077,120084),(120086,120092),(120120,120121),(120123,120126),(120128,120132),(120134,120134),(120138,120144),(120172,120197),(120224,120249),(120276,120301),(120328,120353),(120380,120405),(120432,120457),(120488,120512),(120546,120570),(120604,120628),(120662,120686),(120720,120744),(120778,120778)] then 3
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,122),(181,181),(223,246),(248,255),(257,257),(259,259),(261,261),(263,263),(265,265),(267,267),(269,269),(271,271),(273,273),(275,275),(277,277),(279,279),(281,281),(283,283),(285,285),(287,287),(289,289),(291,291),(293,293),(295,295),(297,297),(299,299),(301,301),(303,303),(305,305),(307,307),(309,309),(311,312),(314,314),(316,316),(318,318),(320,320),(322,322),(324,324),(326,326),(328,329),(331,331),(333,333),(335,335),(337,337),(339,339),(341,341),(343,343),(345,345),(347,347),(349,349),(351,351),(353,353),(355,355),(357,357),(359,359),(361,361),(363,363),(365,365),(367,367),(369,369),(371,371),(373,373),(375,375),(378,378),(380,380),(382,384),(387,387),(389,389),(392,392),(396,397),(402,402),(405,405),(409,411),(414,414),(417,417),(419,419),(421,421),(424,424),(426,427),(429,429),(432,432),(436,436),(438,438),(441,442),(445,447),(454,454),(457,457),(460,460),(462,462),(464,464),(466,466),(468,468),(470,470),(472,472),(474,474),(476,477),(479,479),(481,481),(483,483),(485,485),(487,487),(489,489),(491,491),(493,493),(495,496),(499,499),(501,501),(505,505),(507,507),(509,509),(511,511),(513,513),(515,515),(517,517),(519,519),(521,521),(523,523),(525,525),(527,527),(529,529),(531,531),(533,533),(535,535),(537,537),(539,539),(541,541),(543,543),(545,545),(547,547),(549,549),(551,551),(553,553),(555,555),(557,557),(559,559),(561,561),(563,569),(572,572),(575,576),(578,578),(583,583),(585,585),(587,587),(589,589),(591,659),(661,687),(881,881),(883,883),(887,887),(891,893),(912,912),(940,974),(976,977),(981,983),(985,985),(987,987),(989,989),(991,991),(993,993),(995,995),(997,997),(999,999),(1001,1001),(1003,1003),(1005,1005),(1007,1011),(1013,1013),(1016,1016),(1019,1020),(1072,1119),(1121,1121),(1123,1123),(1125,1125),(1127,1127),(1129,1129),(1131,1131),(1133,1133),(1135,1135),(1137,1137),(1139,1139),(1141,1141),(1143,1143),(1145,1145),(1147,1147),(1149,1149),(1151,1151),(1153,1153),(1163,1163),(1165,1165),(1167,1167),(1169,1169),(1171,1171),(1173,1173),(1175,1175),(1177,1177),(1179,1179),(1181,1181),(1183,1183),(1185,1185),(1187,1187),(1189,1189),(1191,1191),(1193,1193),(1195,1195),(1197,1197),(1199,1199),(1201,1201),(1203,1203),(1205,1205),(1207,1207),(1209,1209),(1211,1211),(1213,1213),(1215,1215),(1218,1218),(1220,1220),(1222,1222),(1224,1224),(1226,1226),(1228,1228),(1230,1231),(1233,1233),(1235,1235),(1237,1237),(1239,1239),(1241,1241),(1243,1243),(1245,1245),(1247,1247),(1249,1249),(1251,1251),(1253,1253),(1255,1255),(1257,1257),(1259,1259),(1261,1261),(1263,1263),(1265,1265),(1267,1267),(1269,1269),(1271,1271),(1273,1273),(1275,1275),(1277,1277),(1279,1279),(1281,1281),(1283,1283),(1285,1285),(1287,1287),(1289,1289),(1291,1291),(1293,1293),(1295,1295),(1297,1297),(1299,1299),(1301,1301),(1303,1303),(1305,1305),(1307,1307),(1309,1309),(1311,1311),(1313,1313),(1315,1315),(1317,1317),(1319,1319),(1321,1321),(1323,1323),(1325,1325),(1327,1327),(1377,1415),(7424,7467),(7531,7543),(7545,7578),(7681,7681),(7683,7683),(7685,7685),(7687,7687),(7689,7689),(7691,7691),(7693,7693),(7695,7695),(7697,7697),(7699,7699),(7701,7701),(7703,7703),(7705,7705),(7707,7707),(7709,7709),(7711,7711),(7713,7713),(7715,7715),(7717,7717),(7719,7719),(7721,7721),(7723,7723),(7725,7725),(7727,7727),(7729,7729),(7731,7731),(7733,7733),(7735,7735),(7737,7737),(7739,7739),(7741,7741),(7743,7743),(7745,7745),(7747,7747),(7749,7749),(7751,7751),(7753,7753),(7755,7755),(7757,7757),(7759,7759),(7761,7761),(7763,7763),(7765,7765),(7767,7767),(7769,7769),(7771,7771),(7773,7773),(7775,7775),(7777,7777),(7779,7779),(7781,7781),(7783,7783),(7785,7785),(7787,7787),(7789,7789),(7791,7791),(7793,7793),(7795,7795),(7797,7797),(7799,7799),(7801,7801),(7803,7803),(7805,7805),(7807,7807),(7809,7809),(7811,7811),(7813,7813),(7815,7815),(7817,7817),(7819,7819),(7821,7821),(7823,7823),(7825,7825),(7827,7827),(7829,7837),(7839,7839),(7841,7841),(7843,7843),(7845,7845),(7847,7847),(7849,7849),(7851,7851),(7853,7853),(7855,7855),(7857,7857),(7859,7859),(7861,7861),(7863,7863),(7865,7865),(7867,7867),(7869,7869),(7871,7871),(7873,7873),(7875,7875),(7877,7877),(7879,7879),(7881,7881),(7883,7883),(7885,7885),(7887,7887),(7889,7889),(7891,7891),(7893,7893),(7895,7895),(7897,7897),(7899,7899),(7901,7901),(7903,7903),(7905,7905),(7907,7907),(7909,7909),(7911,7911),(7913,7913),(7915,7915),(7917,7917),(7919,7919),(7921,7921),(7923,7923),(7925,7925),(7927,7927),(7929,7929),(7931,7931),(7933,7933),(7935,7943),(7952,7957),(7968,7975),(7984,7991),(8000,8005),(8016,8023),(8032,8039),(8048,8061),(8064,8071),(8080,8087),(8096,8103),(8112,8116),(8118,8119),(8126,8126),(8130,8132),(8134,8135),(8144,8147),(8150,8151),(8160,8167),(8178,8180),(8182,8183),(8458,8458),(8462,8463),(8467,8467),(8495,8495),(8500,8500),(8505,8505),(8508,8509),(8518,8521),(8526,8526),(8580,8580),(11312,11358),(11361,11361),(11365,11366),(11368,11368),(11370,11370),(11372,11372),(11377,11377),(11379,11380),(11382,11387),(11393,11393),(11395,11395),(11397,11397),(11399,11399),(11401,11401),(11403,11403),(11405,11405),(11407,11407),(11409,11409),(11411,11411),(11413,11413),(11415,11415),(11417,11417),(11419,11419),(11421,11421),(11423,11423),(11425,11425),(11427,11427),(11429,11429),(11431,11431),(11433,11433),(11435,11435),(11437,11437),(11439,11439),(11441,11441),(11443,11443),(11445,11445),(11447,11447),(11449,11449),(11451,11451),(11453,11453),(11455,11455),(11457,11457),(11459,11459),(11461,11461),(11463,11463),(11465,11465),(11467,11467),(11469,11469),(11471,11471),(11473,11473),(11475,11475),(11477,11477),(11479,11479),(11481,11481),(11483,11483),(11485,11485),(11487,11487),(11489,11489),(11491,11492),(11500,11500),(11502,11502),(11507,11507),(11520,11557),(11559,11559),(11565,11565),(42561,42561),(42563,42563),(42565,42565),(42567,42567),(42569,42569),(42571,42571),(42573,42573),(42575,42575),(42577,42577),(42579,42579),(42581,42581),(42583,42583),(42585,42585),(42587,42587),(42589,42589),(42591,42591),(42593,42593),(42595,42595),(42597,42597),(42599,42599),(42601,42601),(42603,42603),(42605,42605),(42625,42625),(42627,42627),(42629,42629),(42631,42631),(42633,42633),(42635,42635),(42637,42637),(42639,42639),(42641,42641),(42643,42643),(42645,42645),(42647,42647),(42649,42649),(42651,42651),(42787,42787),(42789,42789),(42791,42791),(42793,42793),(42795,42795),(42797,42797),(42799,42801),(42803,42803),(42805,42805),(42807,42807),(42809,42809),(42811,42811),(42813,42813),(42815,42815),(42817,42817),(42819,42819),(42821,42821),(42823,42823),(42825,42825),(42827,42827),(42829,42829),(42831,42831),(42833,42833),(42835,42835),(42837,42837),(42839,42839),(42841,42841),(42843,42843),(42845,42845),(42847,42847),(42849,42849),(42851,42851),(42853,42853),(42855,42855),(42857,42857),(42859,42859),(42861,42861),(42863,42863),(42865,42872),(42874,42874),(42876,42876),(42879,42879),(42881,42881),(42883,42883),(42885,42885),(42887,42887),(42892,42892),(42894,42894),(42897,42897),(42899,42901),(42903,42903),(42905,42905),(42907,42907),(42909,42909),(42911,42911),(42913,42913),(42915,42915),(42917,42917),(42919,42919),(42921,42921),(43002,43002),(43824,43866),(43876,43877),(64256,64262),(64275,64279),(65345,65370),(66600,66639),(71872,71903),(119834,119859),(119886,119892),(119894,119911),(119938,119963),(119990,119993),(119995,119995),(119997,120003),(120005,120015),(120042,120067),(120094,120119),(120146,120171),(120198,120223),(120250,120275),(120302,120327),(120354,120379),(120406,120431),(120458,120485),(120514,120538),(120540,120545),(120572,120596),(120598,120603),(120630,120654),(120656,120661),(120688,120712),(120714,120719),(120746,120770),(120772,120777),(120779,120779)] then 5
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(453,453),(456,456),(459,459),(498,498),(8072,8079),(8088,8095),(8104,8111),(8124,8124),(8140,8140),(8188,8188)] then 6
            else 0 in
    case (q, c') of
      (1, 3) -> 2
      (1, 6) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
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
dfa56FinalStates = [2]

dfa56Transition :: Int -> Char -> Int
dfa56Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(32,32),(160,160),(5760,5760),(8192,8202),(8239,8239),(8287,8287),(12288,12288)] then 1
            else 0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa57InitialState :: Int
dfa57InitialState = 1

dfa57FinalStates :: [Int]
dfa57FinalStates = [2,3]

dfa57Transition :: Int -> Char -> Int
dfa57Transition q c =
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

dfa58InitialState :: Int
dfa58InitialState = 1

dfa58FinalStates :: [Int]
dfa58FinalStates = [2]

dfa58Transition :: Int -> Char -> Int
dfa58Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          46 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa59InitialState :: Int
dfa59InitialState = 1

dfa59FinalStates :: [Int]
dfa59FinalStates = [3]

dfa59Transition :: Int -> Char -> Int
dfa59Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          76 -> 1
          117 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa60InitialState :: Int
dfa60InitialState = 1

dfa60FinalStates :: [Int]
dfa60FinalStates = [3]

dfa60Transition :: Int -> Char -> Int
dfa60Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          76 -> 1
          108 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa61InitialState :: Int
dfa61InitialState = 1

dfa61FinalStates :: [Int]
dfa61FinalStates = [3]

dfa61Transition :: Int -> Char -> Int
dfa61Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          76 -> 1
          116 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa62InitialState :: Int
dfa62InitialState = 1

dfa62FinalStates :: [Int]
dfa62FinalStates = [3]

dfa62Transition :: Int -> Char -> Int
dfa62Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          76 -> 1
          109 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa63InitialState :: Int
dfa63InitialState = 1

dfa63FinalStates :: [Int]
dfa63FinalStates = [3]

dfa63Transition :: Int -> Char -> Int
dfa63Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          76 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa64InitialState :: Int
dfa64InitialState = 1

dfa64FinalStates :: [Int]
dfa64FinalStates = [3]

dfa64Transition :: Int -> Char -> Int
dfa64Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          77 -> 1
          110 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa65InitialState :: Int
dfa65InitialState = 1

dfa65FinalStates :: [Int]
dfa65FinalStates = [3]

dfa65Transition :: Int -> Char -> Int
dfa65Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          77 -> 1
          99 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa66InitialState :: Int
dfa66InitialState = 1

dfa66FinalStates :: [Int]
dfa66FinalStates = [3]

dfa66Transition :: Int -> Char -> Int
dfa66Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          77 -> 1
          101 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa67InitialState :: Int
dfa67InitialState = 1

dfa67FinalStates :: [Int]
dfa67FinalStates = [3]

dfa67Transition :: Int -> Char -> Int
dfa67Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          78 -> 1
          100 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa68InitialState :: Int
dfa68InitialState = 1

dfa68FinalStates :: [Int]
dfa68FinalStates = [3]

dfa68Transition :: Int -> Char -> Int
dfa68Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          78 -> 1
          108 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa69InitialState :: Int
dfa69InitialState = 1

dfa69FinalStates :: [Int]
dfa69FinalStates = [3]

dfa69Transition :: Int -> Char -> Int
dfa69Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          78 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa70InitialState :: Int
dfa70InitialState = 1

dfa70FinalStates :: [Int]
dfa70FinalStates = [3]

dfa70Transition :: Int -> Char -> Int
dfa70Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          99 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa71InitialState :: Int
dfa71InitialState = 1

dfa71FinalStates :: [Int]
dfa71FinalStates = [3]

dfa71Transition :: Int -> Char -> Int
dfa71Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          100 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa72InitialState :: Int
dfa72InitialState = 1

dfa72FinalStates :: [Int]
dfa72FinalStates = [3]

dfa72Transition :: Int -> Char -> Int
dfa72Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          115 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa73InitialState :: Int
dfa73InitialState = 1

dfa73FinalStates :: [Int]
dfa73FinalStates = [3]

dfa73Transition :: Int -> Char -> Int
dfa73Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          101 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa74InitialState :: Int
dfa74InitialState = 1

dfa74FinalStates :: [Int]
dfa74FinalStates = [3]

dfa74Transition :: Int -> Char -> Int
dfa74Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          105 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa75InitialState :: Int
dfa75InitialState = 1

dfa75FinalStates :: [Int]
dfa75FinalStates = [3]

dfa75Transition :: Int -> Char -> Int
dfa75Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          102 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa76InitialState :: Int
dfa76InitialState = 1

dfa76FinalStates :: [Int]
dfa76FinalStates = [3]

dfa76Transition :: Int -> Char -> Int
dfa76Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          80 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa77InitialState :: Int
dfa77InitialState = 1

dfa77FinalStates :: [Int]
dfa77FinalStates = [3]

dfa77Transition :: Int -> Char -> Int
dfa77Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          83 -> 1
          109 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa78InitialState :: Int
dfa78InitialState = 1

dfa78FinalStates :: [Int]
dfa78FinalStates = [3]

dfa78Transition :: Int -> Char -> Int
dfa78Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          83 -> 1
          99 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa79InitialState :: Int
dfa79InitialState = 1

dfa79FinalStates :: [Int]
dfa79FinalStates = [3]

dfa79Transition :: Int -> Char -> Int
dfa79Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          83 -> 1
          107 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa80InitialState :: Int
dfa80InitialState = 1

dfa80FinalStates :: [Int]
dfa80FinalStates = [3]

dfa80Transition :: Int -> Char -> Int
dfa80Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          83 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa81InitialState :: Int
dfa81InitialState = 1

dfa81FinalStates :: [Int]
dfa81FinalStates = [3]

dfa81Transition :: Int -> Char -> Int
dfa81Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          90 -> 1
          115 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa82InitialState :: Int
dfa82InitialState = 1

dfa82FinalStates :: [Int]
dfa82FinalStates = [3]

dfa82Transition :: Int -> Char -> Int
dfa82Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          90 -> 1
          108 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa83InitialState :: Int
dfa83InitialState = 1

dfa83FinalStates :: [Int]
dfa83FinalStates = [3]

dfa83Transition :: Int -> Char -> Int
dfa83Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          90 -> 1
          112 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa84InitialState :: Int
dfa84InitialState = 1

dfa84FinalStates :: [Int]
dfa84FinalStates = [3]

dfa84Transition :: Int -> Char -> Int
dfa84Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          99 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa85InitialState :: Int
dfa85InitialState = 1

dfa85FinalStates :: [Int]
dfa85FinalStates = [3]

dfa85Transition :: Int -> Char -> Int
dfa85Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          102 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa86InitialState :: Int
dfa86InitialState = 1

dfa86FinalStates :: [Int]
dfa86FinalStates = [3]

dfa86Transition :: Int -> Char -> Int
dfa86Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          115 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa87InitialState :: Int
dfa87InitialState = 1

dfa87FinalStates :: [Int]
dfa87FinalStates = [3]

dfa87Transition :: Int -> Char -> Int
dfa87Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          111 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa88InitialState :: Int
dfa88InitialState = 1

dfa88FinalStates :: [Int]
dfa88FinalStates = [3]

dfa88Transition :: Int -> Char -> Int
dfa88Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          67 -> 1
          110 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa89InitialState :: Int
dfa89InitialState = 1

dfa89FinalStates :: [Int]
dfa89FinalStates = [2]

dfa89Transition :: Int -> Char -> Int
dfa89Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          94 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa90InitialState :: Int
dfa90InitialState = 1

dfa90FinalStates :: [Int]
dfa90FinalStates = [2]

dfa90Transition :: Int -> Char -> Int
dfa90Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          45 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa91InitialState :: Int
dfa91InitialState = 1

dfa91FinalStates :: [Int]
dfa91FinalStates = [2]

dfa91Transition :: Int -> Char -> Int
dfa91Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          91 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa92InitialState :: Int
dfa92InitialState = 1

dfa92FinalStates :: [Int]
dfa92FinalStates = [2]

dfa92Transition :: Int -> Char -> Int
dfa92Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          93 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa93InitialState :: Int
dfa93InitialState = 1

dfa93FinalStates :: [Int]
dfa93FinalStates = [2]

dfa93Transition :: Int -> Char -> Int
dfa93Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          44 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa94InitialState :: Int
dfa94InitialState = 1

dfa94FinalStates :: [Int]
dfa94FinalStates = [3]

dfa94Transition :: Int -> Char -> Int
dfa94Transition q c =
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

dfa95InitialState :: Int
dfa95InitialState = 1

dfa95FinalStates :: [Int]
dfa95FinalStates = [5]

dfa95Transition :: Int -> Char -> Int
dfa95Transition q c =
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

dfa96InitialState :: Int
dfa96InitialState = 1

dfa96FinalStates :: [Int]
dfa96FinalStates = [2]

dfa96Transition :: Int -> Char -> Int
dfa96Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          42 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa97InitialState :: Int
dfa97InitialState = 1

dfa97FinalStates :: [Int]
dfa97FinalStates = [2]

dfa97Transition :: Int -> Char -> Int
dfa97Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          43 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa98InitialState :: Int
dfa98InitialState = 1

dfa98FinalStates :: [Int]
dfa98FinalStates = [2]

dfa98Transition :: Int -> Char -> Int
dfa98Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          63 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa99InitialState :: Int
dfa99InitialState = 1

dfa99FinalStates :: [Int]
dfa99FinalStates = [2]

dfa99Transition :: Int -> Char -> Int
dfa99Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          40 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa100InitialState :: Int
dfa100InitialState = 1

dfa100FinalStates :: [Int]
dfa100FinalStates = [2]

dfa100Transition :: Int -> Char -> Int
dfa100Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          41 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa101InitialState :: Int
dfa101InitialState = 1

dfa101FinalStates :: [Int]
dfa101FinalStates = [3,4,7]

dfa101Transition :: Int -> Char -> Int
dfa101Transition q c =
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
      (2, 40) -> 5
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
      (3, 2) -> 6
      (3, 3) -> 6
      (3, 4) -> 6
      (3, 5) -> 6
      (3, 6) -> 6
      (3, 7) -> 6
      (3, 8) -> 7
      (3, 9) -> 2
      (3, 10) -> 2
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
      (3, 40) -> 8
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
      (3, 51) -> 6
      (5, 0) -> 6
      (5, 2) -> 9
      (5, 3) -> 9
      (5, 4) -> 9
      (5, 5) -> 9
      (5, 6) -> 9
      (5, 7) -> 9
      (5, 8) -> 2
      (5, 9) -> 2
      (5, 10) -> 2
      (5, 11) -> 2
      (5, 12) -> 2
      (5, 13) -> 2
      (5, 14) -> 2
      (5, 15) -> 2
      (5, 16) -> 2
      (5, 17) -> 10
      (5, 18) -> 11
      (5, 19) -> 12
      (5, 20) -> 13
      (5, 21) -> 14
      (5, 22) -> 15
      (5, 23) -> 16
      (5, 24) -> 17
      (5, 27) -> 18
      (5, 29) -> 19
      (5, 33) -> 16
      (5, 34) -> 20
      (5, 36) -> 16
      (5, 37) -> 17
      (5, 40) -> 2
      (5, 41) -> 2
      (5, 42) -> 2
      (5, 44) -> 2
      (5, 45) -> 2
      (5, 46) -> 21
      (5, 47) -> 2
      (5, 48) -> 2
      (5, 49) -> 2
      (5, 50) -> 22
      (5, 51) -> 9
      (6, 0) -> 3
      (6, 1) -> 2
      (6, 2) -> 6
      (6, 3) -> 6
      (6, 4) -> 6
      (6, 5) -> 6
      (6, 6) -> 6
      (6, 7) -> 6
      (6, 8) -> 4
      (6, 9) -> 2
      (6, 10) -> 2
      (6, 11) -> 2
      (6, 12) -> 2
      (6, 13) -> 2
      (6, 14) -> 2
      (6, 15) -> 2
      (6, 16) -> 2
      (6, 17) -> 2
      (6, 18) -> 2
      (6, 19) -> 2
      (6, 20) -> 2
      (6, 21) -> 2
      (6, 22) -> 2
      (6, 23) -> 2
      (6, 24) -> 2
      (6, 25) -> 2
      (6, 26) -> 2
      (6, 27) -> 2
      (6, 28) -> 2
      (6, 29) -> 2
      (6, 30) -> 2
      (6, 31) -> 2
      (6, 32) -> 2
      (6, 33) -> 2
      (6, 34) -> 2
      (6, 35) -> 2
      (6, 36) -> 2
      (6, 37) -> 2
      (6, 38) -> 2
      (6, 39) -> 2
      (6, 40) -> 8
      (6, 41) -> 2
      (6, 42) -> 2
      (6, 43) -> 2
      (6, 44) -> 2
      (6, 45) -> 2
      (6, 46) -> 2
      (6, 47) -> 2
      (6, 48) -> 2
      (6, 49) -> 2
      (6, 50) -> 2
      (6, 51) -> 6
      (7, 0) -> 3
      (7, 1) -> 2
      (7, 2) -> 2
      (7, 3) -> 2
      (7, 4) -> 2
      (7, 5) -> 2
      (7, 6) -> 2
      (7, 7) -> 2
      (7, 8) -> 4
      (7, 9) -> 2
      (7, 10) -> 2
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
      (7, 40) -> 5
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
      (7, 51) -> 2
      (8, 0) -> 3
      (8, 1) -> 2
      (8, 2) -> 6
      (8, 3) -> 6
      (8, 4) -> 6
      (8, 5) -> 6
      (8, 6) -> 6
      (8, 7) -> 6
      (8, 8) -> 7
      (8, 9) -> 2
      (8, 10) -> 2
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
      (8, 40) -> 8
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
      (8, 51) -> 6
      (9, 0) -> 6
      (9, 2) -> 9
      (9, 3) -> 9
      (9, 4) -> 9
      (9, 5) -> 9
      (9, 6) -> 9
      (9, 7) -> 9
      (9, 40) -> 2
      (9, 51) -> 9
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

dfa102InitialState :: Int
dfa102InitialState = 1

dfa102FinalStates :: [Int]
dfa102FinalStates = [6,7,22,24,36,52,53]

dfa102Transition :: Int -> Char -> Int
dfa102Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          34 -> 2
          39 -> 3
          49 -> 5
          50 -> 6
          51 -> 7
          52 -> 8
          65 -> 10
          66 -> 11
          67 -> 12
          68 -> 13
          69 -> 14
          70 -> 15
          71 -> 16
          72 -> 17
          73 -> 18
          75 -> 19
          76 -> 20
          77 -> 21
          78 -> 22
          79 -> 23
          80 -> 24
          81 -> 25
          82 -> 26
          83 -> 27
          84 -> 28
          85 -> 29
          86 -> 30
          88 -> 31
          89 -> 32
          92 -> 33
          97 -> 34
          98 -> 35
          102 -> 37
          110 -> 38
          111 -> 39
          114 -> 40
          116 -> 41
          118 -> 42
          120 -> 43
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(0,33),(35,38),(40,47),(58,64),(74,74),(87,87),(90,91),(93,96),(103,109),(112,113),(115,115),(117,117),(119,119),(121,1631),(1642,1775),(1786,1983),(1994,2405),(2416,2533),(2544,2661),(2672,2789),(2800,2917),(2928,3045),(3056,3173),(3184,3301),(3312,3429),(3440,3557),(3568,3663),(3674,3791),(3802,3871),(3882,4159),(4170,4239),(4250,6111),(6122,6159),(6170,6469),(6480,6607),(6618,6783),(6794,6799),(6810,6991),(7002,7087),(7098,7231),(7242,7247),(7258,42527),(42538,43215),(43226,43263),(43274,43471),(43482,43503),(43514,43599),(43610,44015),(44026,65295),(65306,66719),(66730,69733),(69744,69871),(69882,69941),(69952,70095),(70106,70383),(70394,70863),(70874,71247),(71258,71359),(71370,71903),(71914,92767),(92778,93007),(93018,120781),(120832,1114111)] then 1
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,48),(53,55)] then 4
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(56,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 9
            else if 99 <= c'' && c'' <= 101 then 36
            else 0 in
    case (q, c') of
      (1, 0) -> 2
      (1, 3) -> 2
      (2, 0) -> 3
      (2, 1) -> 4
      (2, 2) -> 4
      (2, 4) -> 4
      (2, 5) -> 4
      (2, 6) -> 4
      (2, 7) -> 4
      (2, 8) -> 4
      (2, 9) -> 4
      (2, 10) -> 4
      (2, 11) -> 4
      (2, 12) -> 4
      (2, 13) -> 4
      (2, 14) -> 4
      (2, 15) -> 4
      (2, 16) -> 4
      (2, 17) -> 4
      (2, 18) -> 4
      (2, 19) -> 4
      (2, 20) -> 4
      (2, 21) -> 4
      (2, 22) -> 4
      (2, 23) -> 4
      (2, 24) -> 4
      (2, 25) -> 4
      (2, 26) -> 4
      (2, 27) -> 4
      (2, 28) -> 4
      (2, 29) -> 4
      (2, 30) -> 4
      (2, 31) -> 4
      (2, 32) -> 4
      (2, 33) -> 5
      (2, 34) -> 4
      (2, 35) -> 4
      (2, 36) -> 4
      (2, 37) -> 4
      (2, 38) -> 4
      (2, 39) -> 4
      (2, 40) -> 4
      (2, 41) -> 4
      (2, 42) -> 4
      (2, 43) -> 4
      (3, 0) -> 6
      (3, 2) -> 4
      (3, 3) -> 7
      (3, 4) -> 8
      (3, 5) -> 8
      (3, 6) -> 8
      (3, 7) -> 8
      (3, 8) -> 8
      (3, 9) -> 8
      (3, 10) -> 9
      (3, 11) -> 10
      (3, 12) -> 11
      (3, 13) -> 12
      (3, 14) -> 13
      (3, 15) -> 14
      (3, 16) -> 15
      (3, 17) -> 16
      (3, 20) -> 17
      (3, 22) -> 18
      (3, 26) -> 15
      (3, 27) -> 19
      (3, 29) -> 15
      (3, 30) -> 16
      (3, 33) -> 4
      (3, 34) -> 4
      (3, 35) -> 4
      (3, 37) -> 4
      (3, 38) -> 4
      (3, 39) -> 20
      (3, 40) -> 4
      (3, 41) -> 4
      (3, 42) -> 4
      (3, 43) -> 21
      (4, 0) -> 22
      (4, 3) -> 22
      (5, 0) -> 23
      (5, 2) -> 4
      (5, 3) -> 4
      (5, 4) -> 8
      (5, 5) -> 8
      (5, 6) -> 8
      (5, 7) -> 8
      (5, 8) -> 8
      (5, 9) -> 8
      (5, 10) -> 9
      (5, 11) -> 10
      (5, 12) -> 11
      (5, 13) -> 12
      (5, 14) -> 13
      (5, 15) -> 14
      (5, 16) -> 15
      (5, 17) -> 16
      (5, 20) -> 17
      (5, 22) -> 18
      (5, 26) -> 15
      (5, 27) -> 19
      (5, 29) -> 15
      (5, 30) -> 16
      (5, 33) -> 4
      (5, 34) -> 4
      (5, 35) -> 4
      (5, 37) -> 4
      (5, 38) -> 4
      (5, 39) -> 20
      (5, 40) -> 4
      (5, 41) -> 4
      (5, 42) -> 4
      (5, 43) -> 21
      (6, 0) -> 24
      (6, 3) -> 22
      (6, 4) -> 25
      (6, 5) -> 25
      (6, 6) -> 25
      (6, 7) -> 25
      (6, 8) -> 25
      (6, 9) -> 25
      (6, 10) -> 26
      (6, 11) -> 25
      (6, 12) -> 27
      (6, 13) -> 25
      (6, 14) -> 28
      (6, 15) -> 25
      (6, 18) -> 4
      (6, 20) -> 29
      (6, 21) -> 4
      (6, 22) -> 30
      (6, 23) -> 31
      (6, 24) -> 4
      (6, 26) -> 4
      (6, 27) -> 32
      (6, 28) -> 33
      (6, 29) -> 34
      (6, 32) -> 35
      (6, 34) -> 25
      (6, 35) -> 25
      (6, 36) -> 25
      (6, 37) -> 25
      (7, 0) -> 22
      (7, 3) -> 22
      (8, 0) -> 36
      (8, 3) -> 22
      (8, 4) -> 8
      (8, 5) -> 8
      (8, 6) -> 8
      (8, 7) -> 8
      (8, 8) -> 8
      (8, 9) -> 8
      (9, 0) -> 37
      (9, 12) -> 37
      (10, 0) -> 38
      (10, 14) -> 39
      (10, 27) -> 4
      (11, 0) -> 40
      (11, 10) -> 35
      (11, 26) -> 4
      (12, 0) -> 41
      (12, 12) -> 42
      (12, 14) -> 39
      (12, 20) -> 29
      (13, 0) -> 43
      (13, 21) -> 4
      (13, 22) -> 30
      (13, 23) -> 16
      (13, 27) -> 44
      (13, 28) -> 45
      (14, 0) -> 4
      (14, 15) -> 4
      (14, 27) -> 4
      (15, 0) -> 4
      (15, 27) -> 4
      (16, 0) -> 4
      (16, 28) -> 4
      (17, 0) -> 4
      (17, 15) -> 4
      (18, 0) -> 46
      (18, 10) -> 37
      (18, 29) -> 39
      (19, 0) -> 47
      (19, 18) -> 4
      (19, 23) -> 48
      (19, 24) -> 4
      (19, 28) -> 49
      (19, 29) -> 50
      (19, 32) -> 35
      (20, 0) -> 51
      (20, 4) -> 51
      (20, 5) -> 51
      (20, 6) -> 51
      (20, 7) -> 51
      (20, 8) -> 51
      (21, 0) -> 25
      (21, 4) -> 25
      (21, 5) -> 25
      (21, 6) -> 25
      (21, 7) -> 25
      (21, 8) -> 25
      (21, 9) -> 25
      (21, 10) -> 25
      (21, 11) -> 25
      (21, 12) -> 25
      (21, 13) -> 25
      (21, 14) -> 25
      (21, 15) -> 25
      (21, 34) -> 25
      (21, 35) -> 25
      (21, 36) -> 25
      (21, 37) -> 25
      (23, 0) -> 24
      (23, 3) -> 22
      (23, 4) -> 25
      (23, 5) -> 25
      (23, 6) -> 25
      (23, 7) -> 25
      (23, 8) -> 25
      (23, 9) -> 25
      (23, 10) -> 26
      (23, 11) -> 25
      (23, 12) -> 27
      (23, 13) -> 25
      (23, 14) -> 28
      (23, 15) -> 25
      (23, 18) -> 4
      (23, 20) -> 29
      (23, 21) -> 4
      (23, 22) -> 30
      (23, 23) -> 31
      (23, 24) -> 4
      (23, 26) -> 4
      (23, 27) -> 32
      (23, 28) -> 33
      (23, 29) -> 34
      (23, 32) -> 35
      (23, 34) -> 25
      (23, 35) -> 25
      (23, 36) -> 25
      (23, 37) -> 25
      (24, 0) -> 52
      (24, 3) -> 22
      (24, 4) -> 25
      (24, 5) -> 25
      (24, 6) -> 25
      (24, 7) -> 25
      (24, 8) -> 25
      (24, 9) -> 25
      (24, 10) -> 25
      (24, 11) -> 25
      (24, 12) -> 25
      (24, 13) -> 25
      (24, 14) -> 25
      (24, 15) -> 25
      (24, 17) -> 4
      (24, 19) -> 4
      (24, 20) -> 4
      (24, 22) -> 4
      (24, 25) -> 4
      (24, 28) -> 4
      (24, 31) -> 4
      (24, 34) -> 25
      (24, 35) -> 25
      (24, 36) -> 25
      (24, 37) -> 25
      (25, 0) -> 52
      (25, 3) -> 22
      (25, 4) -> 25
      (25, 5) -> 25
      (25, 6) -> 25
      (25, 7) -> 25
      (25, 8) -> 25
      (25, 9) -> 25
      (25, 10) -> 25
      (25, 11) -> 25
      (25, 12) -> 25
      (25, 13) -> 25
      (25, 14) -> 25
      (25, 15) -> 25
      (25, 34) -> 25
      (25, 35) -> 25
      (25, 36) -> 25
      (25, 37) -> 25
      (26, 0) -> 52
      (26, 3) -> 22
      (26, 4) -> 25
      (26, 5) -> 25
      (26, 6) -> 25
      (26, 7) -> 25
      (26, 8) -> 25
      (26, 9) -> 25
      (26, 10) -> 25
      (26, 11) -> 25
      (26, 12) -> 25
      (26, 13) -> 25
      (26, 14) -> 25
      (26, 15) -> 25
      (26, 19) -> 4
      (26, 22) -> 4
      (26, 34) -> 25
      (26, 35) -> 25
      (26, 36) -> 25
      (26, 37) -> 25
      (27, 0) -> 52
      (27, 3) -> 22
      (27, 4) -> 25
      (27, 5) -> 25
      (27, 6) -> 25
      (27, 7) -> 25
      (27, 8) -> 25
      (27, 9) -> 25
      (27, 10) -> 25
      (27, 11) -> 25
      (27, 12) -> 25
      (27, 13) -> 25
      (27, 14) -> 25
      (27, 15) -> 25
      (27, 19) -> 4
      (27, 34) -> 25
      (27, 35) -> 25
      (27, 36) -> 25
      (27, 37) -> 25
      (28, 0) -> 52
      (28, 3) -> 22
      (28, 4) -> 25
      (28, 5) -> 25
      (28, 6) -> 25
      (28, 7) -> 25
      (28, 8) -> 25
      (28, 9) -> 25
      (28, 10) -> 25
      (28, 11) -> 25
      (28, 12) -> 25
      (28, 13) -> 25
      (28, 14) -> 25
      (28, 15) -> 25
      (28, 20) -> 4
      (28, 34) -> 25
      (28, 35) -> 25
      (28, 36) -> 25
      (28, 37) -> 25
      (29, 0) -> 4
      (29, 14) -> 4
      (30, 0) -> 4
      (30, 25) -> 4
      (31, 0) -> 7
      (31, 3) -> 22
      (31, 17) -> 4
      (31, 28) -> 4
      (32, 0) -> 7
      (32, 3) -> 22
      (32, 12) -> 4
      (33, 0) -> 7
      (33, 3) -> 22
      (33, 11) -> 4
      (33, 31) -> 4
      (34, 0) -> 4
      (34, 11) -> 4
      (34, 20) -> 4
      (35, 0) -> 4
      (35, 22) -> 4
      (36, 0) -> 36
      (36, 3) -> 22
      (36, 4) -> 8
      (36, 5) -> 8
      (36, 6) -> 8
      (36, 7) -> 8
      (36, 8) -> 8
      (36, 9) -> 8
      (37, 0) -> 4
      (37, 19) -> 4
      (38, 0) -> 7
      (38, 3) -> 22
      (38, 20) -> 4
      (39, 0) -> 4
      (39, 20) -> 4
      (40, 0) -> 7
      (40, 3) -> 22
      (40, 22) -> 4
      (41, 0) -> 4
      (41, 5) -> 4
      (41, 6) -> 4
      (41, 7) -> 4
      (41, 8) -> 4
      (41, 14) -> 4
      (41, 20) -> 4
      (42, 0) -> 4
      (42, 5) -> 4
      (42, 6) -> 4
      (42, 7) -> 4
      (42, 8) -> 4
      (43, 0) -> 7
      (43, 3) -> 22
      (43, 11) -> 4
      (43, 12) -> 4
      (43, 25) -> 4
      (43, 28) -> 4
      (43, 31) -> 4
      (44, 0) -> 4
      (44, 12) -> 4
      (45, 0) -> 4
      (45, 11) -> 4
      (45, 31) -> 4
      (46, 0) -> 4
      (46, 19) -> 4
      (46, 20) -> 4
      (47, 0) -> 7
      (47, 3) -> 22
      (47, 11) -> 4
      (47, 17) -> 4
      (47, 22) -> 4
      (47, 31) -> 4
      (48, 0) -> 7
      (48, 3) -> 22
      (48, 17) -> 4
      (49, 0) -> 4
      (49, 31) -> 4
      (50, 0) -> 4
      (50, 11) -> 4
      (51, 0) -> 53
      (51, 3) -> 22
      (51, 4) -> 51
      (51, 5) -> 51
      (51, 6) -> 51
      (51, 7) -> 51
      (51, 8) -> 51
      (52, 0) -> 52
      (52, 3) -> 22
      (52, 4) -> 25
      (52, 5) -> 25
      (52, 6) -> 25
      (52, 7) -> 25
      (52, 8) -> 25
      (52, 9) -> 25
      (52, 10) -> 25
      (52, 11) -> 25
      (52, 12) -> 25
      (52, 13) -> 25
      (52, 14) -> 25
      (52, 15) -> 25
      (52, 34) -> 25
      (52, 35) -> 25
      (52, 36) -> 25
      (52, 37) -> 25
      (53, 0) -> 53
      (53, 3) -> 22
      (53, 4) -> 51
      (53, 5) -> 51
      (53, 6) -> 51
      (53, 7) -> 51
      (53, 8) -> 51
      _ -> 0

dfa103InitialState :: Int
dfa103InitialState = 1

dfa103FinalStates :: [Int]
dfa103FinalStates = [3]

dfa103Transition :: Int -> Char -> Int
dfa103Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          123 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa104InitialState :: Int
dfa104InitialState = 1

dfa104FinalStates :: [Int]
dfa104FinalStates = [8]

dfa104Transition :: Int -> Char -> Int
dfa104Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          100 -> 2
          101 -> 3
          108 -> 4
          109 -> 5
          111 -> 6
          117 -> 7
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 5) -> 3
      (3, 6) -> 4
      (4, 2) -> 5
      (5, 7) -> 6
      (6, 4) -> 7
      (7, 3) -> 8
      _ -> 0

dfa105InitialState :: Int
dfa105InitialState = 1

dfa105FinalStates :: [Int]
dfa105FinalStates = [3]

dfa105Transition :: Int -> Char -> Int
dfa105Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 1) -> 3
      _ -> 0

dfa106InitialState :: Int
dfa106InitialState = 1

dfa106FinalStates :: [Int]
dfa106FinalStates = [2]

dfa106Transition :: Int -> Char -> Int
dfa106Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          124 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      _ -> 0

dfa107InitialState :: Int
dfa107InitialState = 1

dfa107FinalStates :: [Int]
dfa107FinalStates = [7]

dfa107Transition :: Int -> Char -> Int
dfa107Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          101 -> 2
          104 -> 3
          114 -> 4
          119 -> 5
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 5) -> 3
      (3, 3) -> 4
      (4, 2) -> 5
      (5, 4) -> 6
      (6, 2) -> 7
      _ -> 0

dfa108InitialState :: Int
dfa108InitialState = 1

dfa108FinalStates :: [Int]
dfa108FinalStates = [3]

dfa108Transition :: Int -> Char -> Int
dfa108Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          125 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa109InitialState :: Int
dfa109InitialState = 1

dfa109FinalStates :: [Int]
dfa109FinalStates = [2]

dfa109Transition :: Int -> Char -> Int
dfa109Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          39 -> 1
          95 -> 4
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 2
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(65,90),(192,214),(216,222),(256,256),(258,258),(260,260),(262,262),(264,264),(266,266),(268,268),(270,270),(272,272),(274,274),(276,276),(278,278),(280,280),(282,282),(284,284),(286,286),(288,288),(290,290),(292,292),(294,294),(296,296),(298,298),(300,300),(302,302),(304,304),(306,306),(308,308),(310,310),(313,313),(315,315),(317,317),(319,319),(321,321),(323,323),(325,325),(327,327),(330,330),(332,332),(334,334),(336,336),(338,338),(340,340),(342,342),(344,344),(346,346),(348,348),(350,350),(352,352),(354,354),(356,356),(358,358),(360,360),(362,362),(364,364),(366,366),(368,368),(370,370),(372,372),(374,374),(376,377),(379,379),(381,381),(385,386),(388,388),(390,391),(393,395),(398,401),(403,404),(406,408),(412,413),(415,416),(418,418),(420,420),(422,423),(425,425),(428,428),(430,431),(433,435),(437,437),(439,440),(444,444),(452,452),(455,455),(458,458),(461,461),(463,463),(465,465),(467,467),(469,469),(471,471),(473,473),(475,475),(478,478),(480,480),(482,482),(484,484),(486,486),(488,488),(490,490),(492,492),(494,494),(497,497),(500,500),(502,504),(506,506),(508,508),(510,510),(512,512),(514,514),(516,516),(518,518),(520,520),(522,522),(524,524),(526,526),(528,528),(530,530),(532,532),(534,534),(536,536),(538,538),(540,540),(542,542),(544,544),(546,546),(548,548),(550,550),(552,552),(554,554),(556,556),(558,558),(560,560),(562,562),(570,571),(573,574),(577,577),(579,582),(584,584),(586,586),(588,588),(590,590),(880,880),(882,882),(886,886),(895,895),(902,902),(904,906),(908,908),(910,911),(913,929),(931,939),(975,975),(978,980),(984,984),(986,986),(988,988),(990,990),(992,992),(994,994),(996,996),(998,998),(1000,1000),(1002,1002),(1004,1004),(1006,1006),(1012,1012),(1015,1015),(1017,1018),(1021,1071),(1120,1120),(1122,1122),(1124,1124),(1126,1126),(1128,1128),(1130,1130),(1132,1132),(1134,1134),(1136,1136),(1138,1138),(1140,1140),(1142,1142),(1144,1144),(1146,1146),(1148,1148),(1150,1150),(1152,1152),(1162,1162),(1164,1164),(1166,1166),(1168,1168),(1170,1170),(1172,1172),(1174,1174),(1176,1176),(1178,1178),(1180,1180),(1182,1182),(1184,1184),(1186,1186),(1188,1188),(1190,1190),(1192,1192),(1194,1194),(1196,1196),(1198,1198),(1200,1200),(1202,1202),(1204,1204),(1206,1206),(1208,1208),(1210,1210),(1212,1212),(1214,1214),(1216,1217),(1219,1219),(1221,1221),(1223,1223),(1225,1225),(1227,1227),(1229,1229),(1232,1232),(1234,1234),(1236,1236),(1238,1238),(1240,1240),(1242,1242),(1244,1244),(1246,1246),(1248,1248),(1250,1250),(1252,1252),(1254,1254),(1256,1256),(1258,1258),(1260,1260),(1262,1262),(1264,1264),(1266,1266),(1268,1268),(1270,1270),(1272,1272),(1274,1274),(1276,1276),(1278,1278),(1280,1280),(1282,1282),(1284,1284),(1286,1286),(1288,1288),(1290,1290),(1292,1292),(1294,1294),(1296,1296),(1298,1298),(1300,1300),(1302,1302),(1304,1304),(1306,1306),(1308,1308),(1310,1310),(1312,1312),(1314,1314),(1316,1316),(1318,1318),(1320,1320),(1322,1322),(1324,1324),(1326,1326),(1329,1366),(4256,4293),(4295,4295),(4301,4301),(7680,7680),(7682,7682),(7684,7684),(7686,7686),(7688,7688),(7690,7690),(7692,7692),(7694,7694),(7696,7696),(7698,7698),(7700,7700),(7702,7702),(7704,7704),(7706,7706),(7708,7708),(7710,7710),(7712,7712),(7714,7714),(7716,7716),(7718,7718),(7720,7720),(7722,7722),(7724,7724),(7726,7726),(7728,7728),(7730,7730),(7732,7732),(7734,7734),(7736,7736),(7738,7738),(7740,7740),(7742,7742),(7744,7744),(7746,7746),(7748,7748),(7750,7750),(7752,7752),(7754,7754),(7756,7756),(7758,7758),(7760,7760),(7762,7762),(7764,7764),(7766,7766),(7768,7768),(7770,7770),(7772,7772),(7774,7774),(7776,7776),(7778,7778),(7780,7780),(7782,7782),(7784,7784),(7786,7786),(7788,7788),(7790,7790),(7792,7792),(7794,7794),(7796,7796),(7798,7798),(7800,7800),(7802,7802),(7804,7804),(7806,7806),(7808,7808),(7810,7810),(7812,7812),(7814,7814),(7816,7816),(7818,7818),(7820,7820),(7822,7822),(7824,7824),(7826,7826),(7828,7828),(7838,7838),(7840,7840),(7842,7842),(7844,7844),(7846,7846),(7848,7848),(7850,7850),(7852,7852),(7854,7854),(7856,7856),(7858,7858),(7860,7860),(7862,7862),(7864,7864),(7866,7866),(7868,7868),(7870,7870),(7872,7872),(7874,7874),(7876,7876),(7878,7878),(7880,7880),(7882,7882),(7884,7884),(7886,7886),(7888,7888),(7890,7890),(7892,7892),(7894,7894),(7896,7896),(7898,7898),(7900,7900),(7902,7902),(7904,7904),(7906,7906),(7908,7908),(7910,7910),(7912,7912),(7914,7914),(7916,7916),(7918,7918),(7920,7920),(7922,7922),(7924,7924),(7926,7926),(7928,7928),(7930,7930),(7932,7932),(7934,7934),(7944,7951),(7960,7965),(7976,7983),(7992,7999),(8008,8013),(8025,8025),(8027,8027),(8029,8029),(8031,8031),(8040,8047),(8120,8123),(8136,8139),(8152,8155),(8168,8172),(8184,8187),(8450,8450),(8455,8455),(8459,8461),(8464,8466),(8469,8469),(8473,8477),(8484,8484),(8486,8486),(8488,8488),(8490,8493),(8496,8499),(8510,8511),(8517,8517),(8579,8579),(11264,11310),(11360,11360),(11362,11364),(11367,11367),(11369,11369),(11371,11371),(11373,11376),(11378,11378),(11381,11381),(11390,11392),(11394,11394),(11396,11396),(11398,11398),(11400,11400),(11402,11402),(11404,11404),(11406,11406),(11408,11408),(11410,11410),(11412,11412),(11414,11414),(11416,11416),(11418,11418),(11420,11420),(11422,11422),(11424,11424),(11426,11426),(11428,11428),(11430,11430),(11432,11432),(11434,11434),(11436,11436),(11438,11438),(11440,11440),(11442,11442),(11444,11444),(11446,11446),(11448,11448),(11450,11450),(11452,11452),(11454,11454),(11456,11456),(11458,11458),(11460,11460),(11462,11462),(11464,11464),(11466,11466),(11468,11468),(11470,11470),(11472,11472),(11474,11474),(11476,11476),(11478,11478),(11480,11480),(11482,11482),(11484,11484),(11486,11486),(11488,11488),(11490,11490),(11499,11499),(11501,11501),(11506,11506),(42560,42560),(42562,42562),(42564,42564),(42566,42566),(42568,42568),(42570,42570),(42572,42572),(42574,42574),(42576,42576),(42578,42578),(42580,42580),(42582,42582),(42584,42584),(42586,42586),(42588,42588),(42590,42590),(42592,42592),(42594,42594),(42596,42596),(42598,42598),(42600,42600),(42602,42602),(42604,42604),(42624,42624),(42626,42626),(42628,42628),(42630,42630),(42632,42632),(42634,42634),(42636,42636),(42638,42638),(42640,42640),(42642,42642),(42644,42644),(42646,42646),(42648,42648),(42650,42650),(42786,42786),(42788,42788),(42790,42790),(42792,42792),(42794,42794),(42796,42796),(42798,42798),(42802,42802),(42804,42804),(42806,42806),(42808,42808),(42810,42810),(42812,42812),(42814,42814),(42816,42816),(42818,42818),(42820,42820),(42822,42822),(42824,42824),(42826,42826),(42828,42828),(42830,42830),(42832,42832),(42834,42834),(42836,42836),(42838,42838),(42840,42840),(42842,42842),(42844,42844),(42846,42846),(42848,42848),(42850,42850),(42852,42852),(42854,42854),(42856,42856),(42858,42858),(42860,42860),(42862,42862),(42873,42873),(42875,42875),(42877,42878),(42880,42880),(42882,42882),(42884,42884),(42886,42886),(42891,42891),(42893,42893),(42896,42896),(42898,42898),(42902,42902),(42904,42904),(42906,42906),(42908,42908),(42910,42910),(42912,42912),(42914,42914),(42916,42916),(42918,42918),(42920,42920),(42922,42925),(42928,42929),(65313,65338),(66560,66599),(71840,71871),(119808,119833),(119860,119885),(119912,119937),(119964,119964),(119966,119967),(119970,119970),(119973,119974),(119977,119980),(119982,119989),(120016,120041),(120068,120069),(120071,120074),(120077,120084),(120086,120092),(120120,120121),(120123,120126),(120128,120132),(120134,120134),(120138,120144),(120172,120197),(120224,120249),(120276,120301),(120328,120353),(120380,120405),(120432,120457),(120488,120512),(120546,120570),(120604,120628),(120662,120686),(120720,120744),(120778,120778)] then 3
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,122),(181,181),(223,246),(248,255),(257,257),(259,259),(261,261),(263,263),(265,265),(267,267),(269,269),(271,271),(273,273),(275,275),(277,277),(279,279),(281,281),(283,283),(285,285),(287,287),(289,289),(291,291),(293,293),(295,295),(297,297),(299,299),(301,301),(303,303),(305,305),(307,307),(309,309),(311,312),(314,314),(316,316),(318,318),(320,320),(322,322),(324,324),(326,326),(328,329),(331,331),(333,333),(335,335),(337,337),(339,339),(341,341),(343,343),(345,345),(347,347),(349,349),(351,351),(353,353),(355,355),(357,357),(359,359),(361,361),(363,363),(365,365),(367,367),(369,369),(371,371),(373,373),(375,375),(378,378),(380,380),(382,384),(387,387),(389,389),(392,392),(396,397),(402,402),(405,405),(409,411),(414,414),(417,417),(419,419),(421,421),(424,424),(426,427),(429,429),(432,432),(436,436),(438,438),(441,442),(445,447),(454,454),(457,457),(460,460),(462,462),(464,464),(466,466),(468,468),(470,470),(472,472),(474,474),(476,477),(479,479),(481,481),(483,483),(485,485),(487,487),(489,489),(491,491),(493,493),(495,496),(499,499),(501,501),(505,505),(507,507),(509,509),(511,511),(513,513),(515,515),(517,517),(519,519),(521,521),(523,523),(525,525),(527,527),(529,529),(531,531),(533,533),(535,535),(537,537),(539,539),(541,541),(543,543),(545,545),(547,547),(549,549),(551,551),(553,553),(555,555),(557,557),(559,559),(561,561),(563,569),(572,572),(575,576),(578,578),(583,583),(585,585),(587,587),(589,589),(591,659),(661,687),(881,881),(883,883),(887,887),(891,893),(912,912),(940,974),(976,977),(981,983),(985,985),(987,987),(989,989),(991,991),(993,993),(995,995),(997,997),(999,999),(1001,1001),(1003,1003),(1005,1005),(1007,1011),(1013,1013),(1016,1016),(1019,1020),(1072,1119),(1121,1121),(1123,1123),(1125,1125),(1127,1127),(1129,1129),(1131,1131),(1133,1133),(1135,1135),(1137,1137),(1139,1139),(1141,1141),(1143,1143),(1145,1145),(1147,1147),(1149,1149),(1151,1151),(1153,1153),(1163,1163),(1165,1165),(1167,1167),(1169,1169),(1171,1171),(1173,1173),(1175,1175),(1177,1177),(1179,1179),(1181,1181),(1183,1183),(1185,1185),(1187,1187),(1189,1189),(1191,1191),(1193,1193),(1195,1195),(1197,1197),(1199,1199),(1201,1201),(1203,1203),(1205,1205),(1207,1207),(1209,1209),(1211,1211),(1213,1213),(1215,1215),(1218,1218),(1220,1220),(1222,1222),(1224,1224),(1226,1226),(1228,1228),(1230,1231),(1233,1233),(1235,1235),(1237,1237),(1239,1239),(1241,1241),(1243,1243),(1245,1245),(1247,1247),(1249,1249),(1251,1251),(1253,1253),(1255,1255),(1257,1257),(1259,1259),(1261,1261),(1263,1263),(1265,1265),(1267,1267),(1269,1269),(1271,1271),(1273,1273),(1275,1275),(1277,1277),(1279,1279),(1281,1281),(1283,1283),(1285,1285),(1287,1287),(1289,1289),(1291,1291),(1293,1293),(1295,1295),(1297,1297),(1299,1299),(1301,1301),(1303,1303),(1305,1305),(1307,1307),(1309,1309),(1311,1311),(1313,1313),(1315,1315),(1317,1317),(1319,1319),(1321,1321),(1323,1323),(1325,1325),(1327,1327),(1377,1415),(7424,7467),(7531,7543),(7545,7578),(7681,7681),(7683,7683),(7685,7685),(7687,7687),(7689,7689),(7691,7691),(7693,7693),(7695,7695),(7697,7697),(7699,7699),(7701,7701),(7703,7703),(7705,7705),(7707,7707),(7709,7709),(7711,7711),(7713,7713),(7715,7715),(7717,7717),(7719,7719),(7721,7721),(7723,7723),(7725,7725),(7727,7727),(7729,7729),(7731,7731),(7733,7733),(7735,7735),(7737,7737),(7739,7739),(7741,7741),(7743,7743),(7745,7745),(7747,7747),(7749,7749),(7751,7751),(7753,7753),(7755,7755),(7757,7757),(7759,7759),(7761,7761),(7763,7763),(7765,7765),(7767,7767),(7769,7769),(7771,7771),(7773,7773),(7775,7775),(7777,7777),(7779,7779),(7781,7781),(7783,7783),(7785,7785),(7787,7787),(7789,7789),(7791,7791),(7793,7793),(7795,7795),(7797,7797),(7799,7799),(7801,7801),(7803,7803),(7805,7805),(7807,7807),(7809,7809),(7811,7811),(7813,7813),(7815,7815),(7817,7817),(7819,7819),(7821,7821),(7823,7823),(7825,7825),(7827,7827),(7829,7837),(7839,7839),(7841,7841),(7843,7843),(7845,7845),(7847,7847),(7849,7849),(7851,7851),(7853,7853),(7855,7855),(7857,7857),(7859,7859),(7861,7861),(7863,7863),(7865,7865),(7867,7867),(7869,7869),(7871,7871),(7873,7873),(7875,7875),(7877,7877),(7879,7879),(7881,7881),(7883,7883),(7885,7885),(7887,7887),(7889,7889),(7891,7891),(7893,7893),(7895,7895),(7897,7897),(7899,7899),(7901,7901),(7903,7903),(7905,7905),(7907,7907),(7909,7909),(7911,7911),(7913,7913),(7915,7915),(7917,7917),(7919,7919),(7921,7921),(7923,7923),(7925,7925),(7927,7927),(7929,7929),(7931,7931),(7933,7933),(7935,7943),(7952,7957),(7968,7975),(7984,7991),(8000,8005),(8016,8023),(8032,8039),(8048,8061),(8064,8071),(8080,8087),(8096,8103),(8112,8116),(8118,8119),(8126,8126),(8130,8132),(8134,8135),(8144,8147),(8150,8151),(8160,8167),(8178,8180),(8182,8183),(8458,8458),(8462,8463),(8467,8467),(8495,8495),(8500,8500),(8505,8505),(8508,8509),(8518,8521),(8526,8526),(8580,8580),(11312,11358),(11361,11361),(11365,11366),(11368,11368),(11370,11370),(11372,11372),(11377,11377),(11379,11380),(11382,11387),(11393,11393),(11395,11395),(11397,11397),(11399,11399),(11401,11401),(11403,11403),(11405,11405),(11407,11407),(11409,11409),(11411,11411),(11413,11413),(11415,11415),(11417,11417),(11419,11419),(11421,11421),(11423,11423),(11425,11425),(11427,11427),(11429,11429),(11431,11431),(11433,11433),(11435,11435),(11437,11437),(11439,11439),(11441,11441),(11443,11443),(11445,11445),(11447,11447),(11449,11449),(11451,11451),(11453,11453),(11455,11455),(11457,11457),(11459,11459),(11461,11461),(11463,11463),(11465,11465),(11467,11467),(11469,11469),(11471,11471),(11473,11473),(11475,11475),(11477,11477),(11479,11479),(11481,11481),(11483,11483),(11485,11485),(11487,11487),(11489,11489),(11491,11492),(11500,11500),(11502,11502),(11507,11507),(11520,11557),(11559,11559),(11565,11565),(42561,42561),(42563,42563),(42565,42565),(42567,42567),(42569,42569),(42571,42571),(42573,42573),(42575,42575),(42577,42577),(42579,42579),(42581,42581),(42583,42583),(42585,42585),(42587,42587),(42589,42589),(42591,42591),(42593,42593),(42595,42595),(42597,42597),(42599,42599),(42601,42601),(42603,42603),(42605,42605),(42625,42625),(42627,42627),(42629,42629),(42631,42631),(42633,42633),(42635,42635),(42637,42637),(42639,42639),(42641,42641),(42643,42643),(42645,42645),(42647,42647),(42649,42649),(42651,42651),(42787,42787),(42789,42789),(42791,42791),(42793,42793),(42795,42795),(42797,42797),(42799,42801),(42803,42803),(42805,42805),(42807,42807),(42809,42809),(42811,42811),(42813,42813),(42815,42815),(42817,42817),(42819,42819),(42821,42821),(42823,42823),(42825,42825),(42827,42827),(42829,42829),(42831,42831),(42833,42833),(42835,42835),(42837,42837),(42839,42839),(42841,42841),(42843,42843),(42845,42845),(42847,42847),(42849,42849),(42851,42851),(42853,42853),(42855,42855),(42857,42857),(42859,42859),(42861,42861),(42863,42863),(42865,42872),(42874,42874),(42876,42876),(42879,42879),(42881,42881),(42883,42883),(42885,42885),(42887,42887),(42892,42892),(42894,42894),(42897,42897),(42899,42901),(42903,42903),(42905,42905),(42907,42907),(42909,42909),(42911,42911),(42913,42913),(42915,42915),(42917,42917),(42919,42919),(42921,42921),(43002,43002),(43824,43866),(43876,43877),(64256,64262),(64275,64279),(65345,65370),(66600,66639),(71872,71903),(119834,119859),(119886,119892),(119894,119911),(119938,119963),(119990,119993),(119995,119995),(119997,120003),(120005,120015),(120042,120067),(120094,120119),(120146,120171),(120198,120223),(120250,120275),(120302,120327),(120354,120379),(120406,120431),(120458,120485),(120514,120538),(120540,120545),(120572,120596),(120598,120603),(120630,120654),(120656,120661),(120688,120712),(120714,120719),(120746,120770),(120772,120777),(120779,120779)] then 5
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(453,453),(456,456),(459,459),(498,498),(8072,8079),(8088,8095),(8104,8111),(8124,8124),(8140,8140),(8188,8188)] then 6
            else 0 in
    case (q, c') of
      (1, 4) -> 2
      (1, 5) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
      _ -> 0

dfa110InitialState :: Int
dfa110InitialState = 1

dfa110FinalStates :: [Int]
dfa110FinalStates = [2]

dfa110Transition :: Int -> Char -> Int
dfa110Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          39 -> 1
          95 -> 4
          c'' ->
            if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(48,57),(1632,1641),(1776,1785),(1984,1993),(2406,2415),(2534,2543),(2662,2671),(2790,2799),(2918,2927),(3046,3055),(3174,3183),(3302,3311),(3430,3439),(3558,3567),(3664,3673),(3792,3801),(3872,3881),(4160,4169),(4240,4249),(6112,6121),(6160,6169),(6470,6479),(6608,6617),(6784,6793),(6800,6809),(6992,7001),(7088,7097),(7232,7241),(7248,7257),(42528,42537),(43216,43225),(43264,43273),(43472,43481),(43504,43513),(43600,43609),(44016,44025),(65296,65305),(66720,66729),(69734,69743),(69872,69881),(69942,69951),(70096,70105),(70384,70393),(70864,70873),(71248,71257),(71360,71369),(71904,71913),(92768,92777),(93008,93017),(120782,120831)] then 2
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(65,90),(192,214),(216,222),(256,256),(258,258),(260,260),(262,262),(264,264),(266,266),(268,268),(270,270),(272,272),(274,274),(276,276),(278,278),(280,280),(282,282),(284,284),(286,286),(288,288),(290,290),(292,292),(294,294),(296,296),(298,298),(300,300),(302,302),(304,304),(306,306),(308,308),(310,310),(313,313),(315,315),(317,317),(319,319),(321,321),(323,323),(325,325),(327,327),(330,330),(332,332),(334,334),(336,336),(338,338),(340,340),(342,342),(344,344),(346,346),(348,348),(350,350),(352,352),(354,354),(356,356),(358,358),(360,360),(362,362),(364,364),(366,366),(368,368),(370,370),(372,372),(374,374),(376,377),(379,379),(381,381),(385,386),(388,388),(390,391),(393,395),(398,401),(403,404),(406,408),(412,413),(415,416),(418,418),(420,420),(422,423),(425,425),(428,428),(430,431),(433,435),(437,437),(439,440),(444,444),(452,452),(455,455),(458,458),(461,461),(463,463),(465,465),(467,467),(469,469),(471,471),(473,473),(475,475),(478,478),(480,480),(482,482),(484,484),(486,486),(488,488),(490,490),(492,492),(494,494),(497,497),(500,500),(502,504),(506,506),(508,508),(510,510),(512,512),(514,514),(516,516),(518,518),(520,520),(522,522),(524,524),(526,526),(528,528),(530,530),(532,532),(534,534),(536,536),(538,538),(540,540),(542,542),(544,544),(546,546),(548,548),(550,550),(552,552),(554,554),(556,556),(558,558),(560,560),(562,562),(570,571),(573,574),(577,577),(579,582),(584,584),(586,586),(588,588),(590,590),(880,880),(882,882),(886,886),(895,895),(902,902),(904,906),(908,908),(910,911),(913,929),(931,939),(975,975),(978,980),(984,984),(986,986),(988,988),(990,990),(992,992),(994,994),(996,996),(998,998),(1000,1000),(1002,1002),(1004,1004),(1006,1006),(1012,1012),(1015,1015),(1017,1018),(1021,1071),(1120,1120),(1122,1122),(1124,1124),(1126,1126),(1128,1128),(1130,1130),(1132,1132),(1134,1134),(1136,1136),(1138,1138),(1140,1140),(1142,1142),(1144,1144),(1146,1146),(1148,1148),(1150,1150),(1152,1152),(1162,1162),(1164,1164),(1166,1166),(1168,1168),(1170,1170),(1172,1172),(1174,1174),(1176,1176),(1178,1178),(1180,1180),(1182,1182),(1184,1184),(1186,1186),(1188,1188),(1190,1190),(1192,1192),(1194,1194),(1196,1196),(1198,1198),(1200,1200),(1202,1202),(1204,1204),(1206,1206),(1208,1208),(1210,1210),(1212,1212),(1214,1214),(1216,1217),(1219,1219),(1221,1221),(1223,1223),(1225,1225),(1227,1227),(1229,1229),(1232,1232),(1234,1234),(1236,1236),(1238,1238),(1240,1240),(1242,1242),(1244,1244),(1246,1246),(1248,1248),(1250,1250),(1252,1252),(1254,1254),(1256,1256),(1258,1258),(1260,1260),(1262,1262),(1264,1264),(1266,1266),(1268,1268),(1270,1270),(1272,1272),(1274,1274),(1276,1276),(1278,1278),(1280,1280),(1282,1282),(1284,1284),(1286,1286),(1288,1288),(1290,1290),(1292,1292),(1294,1294),(1296,1296),(1298,1298),(1300,1300),(1302,1302),(1304,1304),(1306,1306),(1308,1308),(1310,1310),(1312,1312),(1314,1314),(1316,1316),(1318,1318),(1320,1320),(1322,1322),(1324,1324),(1326,1326),(1329,1366),(4256,4293),(4295,4295),(4301,4301),(7680,7680),(7682,7682),(7684,7684),(7686,7686),(7688,7688),(7690,7690),(7692,7692),(7694,7694),(7696,7696),(7698,7698),(7700,7700),(7702,7702),(7704,7704),(7706,7706),(7708,7708),(7710,7710),(7712,7712),(7714,7714),(7716,7716),(7718,7718),(7720,7720),(7722,7722),(7724,7724),(7726,7726),(7728,7728),(7730,7730),(7732,7732),(7734,7734),(7736,7736),(7738,7738),(7740,7740),(7742,7742),(7744,7744),(7746,7746),(7748,7748),(7750,7750),(7752,7752),(7754,7754),(7756,7756),(7758,7758),(7760,7760),(7762,7762),(7764,7764),(7766,7766),(7768,7768),(7770,7770),(7772,7772),(7774,7774),(7776,7776),(7778,7778),(7780,7780),(7782,7782),(7784,7784),(7786,7786),(7788,7788),(7790,7790),(7792,7792),(7794,7794),(7796,7796),(7798,7798),(7800,7800),(7802,7802),(7804,7804),(7806,7806),(7808,7808),(7810,7810),(7812,7812),(7814,7814),(7816,7816),(7818,7818),(7820,7820),(7822,7822),(7824,7824),(7826,7826),(7828,7828),(7838,7838),(7840,7840),(7842,7842),(7844,7844),(7846,7846),(7848,7848),(7850,7850),(7852,7852),(7854,7854),(7856,7856),(7858,7858),(7860,7860),(7862,7862),(7864,7864),(7866,7866),(7868,7868),(7870,7870),(7872,7872),(7874,7874),(7876,7876),(7878,7878),(7880,7880),(7882,7882),(7884,7884),(7886,7886),(7888,7888),(7890,7890),(7892,7892),(7894,7894),(7896,7896),(7898,7898),(7900,7900),(7902,7902),(7904,7904),(7906,7906),(7908,7908),(7910,7910),(7912,7912),(7914,7914),(7916,7916),(7918,7918),(7920,7920),(7922,7922),(7924,7924),(7926,7926),(7928,7928),(7930,7930),(7932,7932),(7934,7934),(7944,7951),(7960,7965),(7976,7983),(7992,7999),(8008,8013),(8025,8025),(8027,8027),(8029,8029),(8031,8031),(8040,8047),(8120,8123),(8136,8139),(8152,8155),(8168,8172),(8184,8187),(8450,8450),(8455,8455),(8459,8461),(8464,8466),(8469,8469),(8473,8477),(8484,8484),(8486,8486),(8488,8488),(8490,8493),(8496,8499),(8510,8511),(8517,8517),(8579,8579),(11264,11310),(11360,11360),(11362,11364),(11367,11367),(11369,11369),(11371,11371),(11373,11376),(11378,11378),(11381,11381),(11390,11392),(11394,11394),(11396,11396),(11398,11398),(11400,11400),(11402,11402),(11404,11404),(11406,11406),(11408,11408),(11410,11410),(11412,11412),(11414,11414),(11416,11416),(11418,11418),(11420,11420),(11422,11422),(11424,11424),(11426,11426),(11428,11428),(11430,11430),(11432,11432),(11434,11434),(11436,11436),(11438,11438),(11440,11440),(11442,11442),(11444,11444),(11446,11446),(11448,11448),(11450,11450),(11452,11452),(11454,11454),(11456,11456),(11458,11458),(11460,11460),(11462,11462),(11464,11464),(11466,11466),(11468,11468),(11470,11470),(11472,11472),(11474,11474),(11476,11476),(11478,11478),(11480,11480),(11482,11482),(11484,11484),(11486,11486),(11488,11488),(11490,11490),(11499,11499),(11501,11501),(11506,11506),(42560,42560),(42562,42562),(42564,42564),(42566,42566),(42568,42568),(42570,42570),(42572,42572),(42574,42574),(42576,42576),(42578,42578),(42580,42580),(42582,42582),(42584,42584),(42586,42586),(42588,42588),(42590,42590),(42592,42592),(42594,42594),(42596,42596),(42598,42598),(42600,42600),(42602,42602),(42604,42604),(42624,42624),(42626,42626),(42628,42628),(42630,42630),(42632,42632),(42634,42634),(42636,42636),(42638,42638),(42640,42640),(42642,42642),(42644,42644),(42646,42646),(42648,42648),(42650,42650),(42786,42786),(42788,42788),(42790,42790),(42792,42792),(42794,42794),(42796,42796),(42798,42798),(42802,42802),(42804,42804),(42806,42806),(42808,42808),(42810,42810),(42812,42812),(42814,42814),(42816,42816),(42818,42818),(42820,42820),(42822,42822),(42824,42824),(42826,42826),(42828,42828),(42830,42830),(42832,42832),(42834,42834),(42836,42836),(42838,42838),(42840,42840),(42842,42842),(42844,42844),(42846,42846),(42848,42848),(42850,42850),(42852,42852),(42854,42854),(42856,42856),(42858,42858),(42860,42860),(42862,42862),(42873,42873),(42875,42875),(42877,42878),(42880,42880),(42882,42882),(42884,42884),(42886,42886),(42891,42891),(42893,42893),(42896,42896),(42898,42898),(42902,42902),(42904,42904),(42906,42906),(42908,42908),(42910,42910),(42912,42912),(42914,42914),(42916,42916),(42918,42918),(42920,42920),(42922,42925),(42928,42929),(65313,65338),(66560,66599),(71840,71871),(119808,119833),(119860,119885),(119912,119937),(119964,119964),(119966,119967),(119970,119970),(119973,119974),(119977,119980),(119982,119989),(120016,120041),(120068,120069),(120071,120074),(120077,120084),(120086,120092),(120120,120121),(120123,120126),(120128,120132),(120134,120134),(120138,120144),(120172,120197),(120224,120249),(120276,120301),(120328,120353),(120380,120405),(120432,120457),(120488,120512),(120546,120570),(120604,120628),(120662,120686),(120720,120744),(120778,120778)] then 3
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(97,122),(181,181),(223,246),(248,255),(257,257),(259,259),(261,261),(263,263),(265,265),(267,267),(269,269),(271,271),(273,273),(275,275),(277,277),(279,279),(281,281),(283,283),(285,285),(287,287),(289,289),(291,291),(293,293),(295,295),(297,297),(299,299),(301,301),(303,303),(305,305),(307,307),(309,309),(311,312),(314,314),(316,316),(318,318),(320,320),(322,322),(324,324),(326,326),(328,329),(331,331),(333,333),(335,335),(337,337),(339,339),(341,341),(343,343),(345,345),(347,347),(349,349),(351,351),(353,353),(355,355),(357,357),(359,359),(361,361),(363,363),(365,365),(367,367),(369,369),(371,371),(373,373),(375,375),(378,378),(380,380),(382,384),(387,387),(389,389),(392,392),(396,397),(402,402),(405,405),(409,411),(414,414),(417,417),(419,419),(421,421),(424,424),(426,427),(429,429),(432,432),(436,436),(438,438),(441,442),(445,447),(454,454),(457,457),(460,460),(462,462),(464,464),(466,466),(468,468),(470,470),(472,472),(474,474),(476,477),(479,479),(481,481),(483,483),(485,485),(487,487),(489,489),(491,491),(493,493),(495,496),(499,499),(501,501),(505,505),(507,507),(509,509),(511,511),(513,513),(515,515),(517,517),(519,519),(521,521),(523,523),(525,525),(527,527),(529,529),(531,531),(533,533),(535,535),(537,537),(539,539),(541,541),(543,543),(545,545),(547,547),(549,549),(551,551),(553,553),(555,555),(557,557),(559,559),(561,561),(563,569),(572,572),(575,576),(578,578),(583,583),(585,585),(587,587),(589,589),(591,659),(661,687),(881,881),(883,883),(887,887),(891,893),(912,912),(940,974),(976,977),(981,983),(985,985),(987,987),(989,989),(991,991),(993,993),(995,995),(997,997),(999,999),(1001,1001),(1003,1003),(1005,1005),(1007,1011),(1013,1013),(1016,1016),(1019,1020),(1072,1119),(1121,1121),(1123,1123),(1125,1125),(1127,1127),(1129,1129),(1131,1131),(1133,1133),(1135,1135),(1137,1137),(1139,1139),(1141,1141),(1143,1143),(1145,1145),(1147,1147),(1149,1149),(1151,1151),(1153,1153),(1163,1163),(1165,1165),(1167,1167),(1169,1169),(1171,1171),(1173,1173),(1175,1175),(1177,1177),(1179,1179),(1181,1181),(1183,1183),(1185,1185),(1187,1187),(1189,1189),(1191,1191),(1193,1193),(1195,1195),(1197,1197),(1199,1199),(1201,1201),(1203,1203),(1205,1205),(1207,1207),(1209,1209),(1211,1211),(1213,1213),(1215,1215),(1218,1218),(1220,1220),(1222,1222),(1224,1224),(1226,1226),(1228,1228),(1230,1231),(1233,1233),(1235,1235),(1237,1237),(1239,1239),(1241,1241),(1243,1243),(1245,1245),(1247,1247),(1249,1249),(1251,1251),(1253,1253),(1255,1255),(1257,1257),(1259,1259),(1261,1261),(1263,1263),(1265,1265),(1267,1267),(1269,1269),(1271,1271),(1273,1273),(1275,1275),(1277,1277),(1279,1279),(1281,1281),(1283,1283),(1285,1285),(1287,1287),(1289,1289),(1291,1291),(1293,1293),(1295,1295),(1297,1297),(1299,1299),(1301,1301),(1303,1303),(1305,1305),(1307,1307),(1309,1309),(1311,1311),(1313,1313),(1315,1315),(1317,1317),(1319,1319),(1321,1321),(1323,1323),(1325,1325),(1327,1327),(1377,1415),(7424,7467),(7531,7543),(7545,7578),(7681,7681),(7683,7683),(7685,7685),(7687,7687),(7689,7689),(7691,7691),(7693,7693),(7695,7695),(7697,7697),(7699,7699),(7701,7701),(7703,7703),(7705,7705),(7707,7707),(7709,7709),(7711,7711),(7713,7713),(7715,7715),(7717,7717),(7719,7719),(7721,7721),(7723,7723),(7725,7725),(7727,7727),(7729,7729),(7731,7731),(7733,7733),(7735,7735),(7737,7737),(7739,7739),(7741,7741),(7743,7743),(7745,7745),(7747,7747),(7749,7749),(7751,7751),(7753,7753),(7755,7755),(7757,7757),(7759,7759),(7761,7761),(7763,7763),(7765,7765),(7767,7767),(7769,7769),(7771,7771),(7773,7773),(7775,7775),(7777,7777),(7779,7779),(7781,7781),(7783,7783),(7785,7785),(7787,7787),(7789,7789),(7791,7791),(7793,7793),(7795,7795),(7797,7797),(7799,7799),(7801,7801),(7803,7803),(7805,7805),(7807,7807),(7809,7809),(7811,7811),(7813,7813),(7815,7815),(7817,7817),(7819,7819),(7821,7821),(7823,7823),(7825,7825),(7827,7827),(7829,7837),(7839,7839),(7841,7841),(7843,7843),(7845,7845),(7847,7847),(7849,7849),(7851,7851),(7853,7853),(7855,7855),(7857,7857),(7859,7859),(7861,7861),(7863,7863),(7865,7865),(7867,7867),(7869,7869),(7871,7871),(7873,7873),(7875,7875),(7877,7877),(7879,7879),(7881,7881),(7883,7883),(7885,7885),(7887,7887),(7889,7889),(7891,7891),(7893,7893),(7895,7895),(7897,7897),(7899,7899),(7901,7901),(7903,7903),(7905,7905),(7907,7907),(7909,7909),(7911,7911),(7913,7913),(7915,7915),(7917,7917),(7919,7919),(7921,7921),(7923,7923),(7925,7925),(7927,7927),(7929,7929),(7931,7931),(7933,7933),(7935,7943),(7952,7957),(7968,7975),(7984,7991),(8000,8005),(8016,8023),(8032,8039),(8048,8061),(8064,8071),(8080,8087),(8096,8103),(8112,8116),(8118,8119),(8126,8126),(8130,8132),(8134,8135),(8144,8147),(8150,8151),(8160,8167),(8178,8180),(8182,8183),(8458,8458),(8462,8463),(8467,8467),(8495,8495),(8500,8500),(8505,8505),(8508,8509),(8518,8521),(8526,8526),(8580,8580),(11312,11358),(11361,11361),(11365,11366),(11368,11368),(11370,11370),(11372,11372),(11377,11377),(11379,11380),(11382,11387),(11393,11393),(11395,11395),(11397,11397),(11399,11399),(11401,11401),(11403,11403),(11405,11405),(11407,11407),(11409,11409),(11411,11411),(11413,11413),(11415,11415),(11417,11417),(11419,11419),(11421,11421),(11423,11423),(11425,11425),(11427,11427),(11429,11429),(11431,11431),(11433,11433),(11435,11435),(11437,11437),(11439,11439),(11441,11441),(11443,11443),(11445,11445),(11447,11447),(11449,11449),(11451,11451),(11453,11453),(11455,11455),(11457,11457),(11459,11459),(11461,11461),(11463,11463),(11465,11465),(11467,11467),(11469,11469),(11471,11471),(11473,11473),(11475,11475),(11477,11477),(11479,11479),(11481,11481),(11483,11483),(11485,11485),(11487,11487),(11489,11489),(11491,11492),(11500,11500),(11502,11502),(11507,11507),(11520,11557),(11559,11559),(11565,11565),(42561,42561),(42563,42563),(42565,42565),(42567,42567),(42569,42569),(42571,42571),(42573,42573),(42575,42575),(42577,42577),(42579,42579),(42581,42581),(42583,42583),(42585,42585),(42587,42587),(42589,42589),(42591,42591),(42593,42593),(42595,42595),(42597,42597),(42599,42599),(42601,42601),(42603,42603),(42605,42605),(42625,42625),(42627,42627),(42629,42629),(42631,42631),(42633,42633),(42635,42635),(42637,42637),(42639,42639),(42641,42641),(42643,42643),(42645,42645),(42647,42647),(42649,42649),(42651,42651),(42787,42787),(42789,42789),(42791,42791),(42793,42793),(42795,42795),(42797,42797),(42799,42801),(42803,42803),(42805,42805),(42807,42807),(42809,42809),(42811,42811),(42813,42813),(42815,42815),(42817,42817),(42819,42819),(42821,42821),(42823,42823),(42825,42825),(42827,42827),(42829,42829),(42831,42831),(42833,42833),(42835,42835),(42837,42837),(42839,42839),(42841,42841),(42843,42843),(42845,42845),(42847,42847),(42849,42849),(42851,42851),(42853,42853),(42855,42855),(42857,42857),(42859,42859),(42861,42861),(42863,42863),(42865,42872),(42874,42874),(42876,42876),(42879,42879),(42881,42881),(42883,42883),(42885,42885),(42887,42887),(42892,42892),(42894,42894),(42897,42897),(42899,42901),(42903,42903),(42905,42905),(42907,42907),(42909,42909),(42911,42911),(42913,42913),(42915,42915),(42917,42917),(42919,42919),(42921,42921),(43002,43002),(43824,43866),(43876,43877),(64256,64262),(64275,64279),(65345,65370),(66600,66639),(71872,71903),(119834,119859),(119886,119892),(119894,119911),(119938,119963),(119990,119993),(119995,119995),(119997,120003),(120005,120015),(120042,120067),(120094,120119),(120146,120171),(120198,120223),(120250,120275),(120302,120327),(120354,120379),(120406,120431),(120458,120485),(120514,120538),(120540,120545),(120572,120596),(120598,120603),(120630,120654),(120656,120661),(120688,120712),(120714,120719),(120746,120770),(120772,120777),(120779,120779)] then 5
            else if any (\(c1, c2) -> c1 <= c'' && c'' <= c2) [(453,453),(456,456),(459,459),(498,498),(8072,8079),(8088,8095),(8104,8111),(8124,8124),(8140,8140),(8188,8188)] then 6
            else 0 in
    case (q, c') of
      (1, 3) -> 2
      (1, 6) -> 2
      (2, 1) -> 2
      (2, 2) -> 2
      (2, 3) -> 2
      (2, 4) -> 2
      (2, 5) -> 2
      (2, 6) -> 2
      _ -> 0

dfa111InitialState :: Int
dfa111InitialState = 1

dfa111FinalStates :: [Int]
dfa111FinalStates = [2]

dfa111Transition :: Int -> Char -> Int
dfa111Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          c'' ->
            0 in
    case (q, c') of
      (1, 0) -> 2
      _ -> 0

dfa112InitialState :: Int
dfa112InitialState = 1

dfa112FinalStates :: [Int]
dfa112FinalStates = [3]

dfa112Transition :: Int -> Char -> Int
dfa112Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          123 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa113InitialState :: Int
dfa113InitialState = 1

dfa113FinalStates :: [Int]
dfa113FinalStates = [8]

dfa113Transition :: Int -> Char -> Int
dfa113Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          100 -> 2
          101 -> 3
          108 -> 4
          109 -> 5
          111 -> 6
          117 -> 7
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 5) -> 3
      (3, 6) -> 4
      (4, 2) -> 5
      (5, 7) -> 6
      (6, 4) -> 7
      (7, 3) -> 8
      _ -> 0

dfa114InitialState :: Int
dfa114InitialState = 1

dfa114FinalStates :: [Int]
dfa114FinalStates = [3]

dfa114Transition :: Int -> Char -> Int
dfa114Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 1) -> 3
      _ -> 0

dfa115InitialState :: Int
dfa115InitialState = 1

dfa115FinalStates :: [Int]
dfa115FinalStates = [7]

dfa115Transition :: Int -> Char -> Int
dfa115Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          101 -> 2
          104 -> 3
          114 -> 4
          119 -> 5
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 5) -> 3
      (3, 3) -> 4
      (4, 2) -> 5
      (5, 4) -> 6
      (6, 2) -> 7
      _ -> 0

dfa116InitialState :: Int
dfa116InitialState = 1

dfa116FinalStates :: [Int]
dfa116FinalStates = [3]

dfa116Transition :: Int -> Char -> Int
dfa116Transition q c =
  let c' :: Int
      c' =
        case Char.ord c of
          37 -> 1
          125 -> 2
          c'' ->
            0 in
    case (q, c') of
      (1, 1) -> 2
      (2, 2) -> 3
      _ -> 0

dfa117InitialState :: Int
dfa117InitialState = 1

dfa117FinalStates :: [Int]
dfa117FinalStates = [2]

dfa117Transition :: Int -> Char -> Int
dfa117Transition q c =
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
      case max (match dfa0InitialState dfa0FinalStates dfa0Transition s, -0) $ max (match dfa1InitialState dfa1FinalStates dfa1Transition s, -1) $ max (match dfa2InitialState dfa2FinalStates dfa2Transition s, -2) $ max (match dfa3InitialState dfa3FinalStates dfa3Transition s, -3) $ max (match dfa4InitialState dfa4FinalStates dfa4Transition s, -4) $ max (match dfa5InitialState dfa5FinalStates dfa5Transition s, -5) $ max (match dfa6InitialState dfa6FinalStates dfa6Transition s, -6) $ max (match dfa7InitialState dfa7FinalStates dfa7Transition s, -7) $ max (match dfa8InitialState dfa8FinalStates dfa8Transition s, -8) $ max (match dfa9InitialState dfa9FinalStates dfa9Transition s, -9) $ max (match dfa10InitialState dfa10FinalStates dfa10Transition s, -10) $ max (match dfa11InitialState dfa11FinalStates dfa11Transition s, -11) $ max (match dfa12InitialState dfa12FinalStates dfa12Transition s, -12) $ max (match dfa13InitialState dfa13FinalStates dfa13Transition s, -13) $ max (match dfa14InitialState dfa14FinalStates dfa14Transition s, -14) $ max (match dfa15InitialState dfa15FinalStates dfa15Transition s, -15) $ max (match dfa16InitialState dfa16FinalStates dfa16Transition s, -16) $ max (match dfa17InitialState dfa17FinalStates dfa17Transition s, -17) $ max (match dfa18InitialState dfa18FinalStates dfa18Transition s, -18) $ max (match dfa19InitialState dfa19FinalStates dfa19Transition s, -19) $ max (match dfa20InitialState dfa20FinalStates dfa20Transition s, -20) $ max (match dfa21InitialState dfa21FinalStates dfa21Transition s, -21) $ max (match dfa22InitialState dfa22FinalStates dfa22Transition s, -22) $ max (match dfa23InitialState dfa23FinalStates dfa23Transition s, -23) $ max (match dfa24InitialState dfa24FinalStates dfa24Transition s, -24) $ max (match dfa25InitialState dfa25FinalStates dfa25Transition s, -25) $ max (match dfa26InitialState dfa26FinalStates dfa26Transition s, -26) $ max (match dfa27InitialState dfa27FinalStates dfa27Transition s, -27) $ max (match dfa28InitialState dfa28FinalStates dfa28Transition s, -28) $ max (match dfa29InitialState dfa29FinalStates dfa29Transition s, -29) $ max (match dfa30InitialState dfa30FinalStates dfa30Transition s, -30) $ max (match dfa31InitialState dfa31FinalStates dfa31Transition s, -31) $ max (match dfa32InitialState dfa32FinalStates dfa32Transition s, -32) $ max (match dfa33InitialState dfa33FinalStates dfa33Transition s, -33) $ max (match dfa34InitialState dfa34FinalStates dfa34Transition s, -34) $ max (match dfa35InitialState dfa35FinalStates dfa35Transition s, -35) $ max (match dfa36InitialState dfa36FinalStates dfa36Transition s, -36) $ max (match dfa37InitialState dfa37FinalStates dfa37Transition s, -37) $ max (match dfa38InitialState dfa38FinalStates dfa38Transition s, -38) $ max (match dfa39InitialState dfa39FinalStates dfa39Transition s, -39) $ max (match dfa40InitialState dfa40FinalStates dfa40Transition s, -40) $ max (match dfa41InitialState dfa41FinalStates dfa41Transition s, -41) $ max (match dfa42InitialState dfa42FinalStates dfa42Transition s, -42) $ max (match dfa43InitialState dfa43FinalStates dfa43Transition s, -43) $ max (match dfa44InitialState dfa44FinalStates dfa44Transition s, -44) $ max (match dfa45InitialState dfa45FinalStates dfa45Transition s, -45) $ max (match dfa46InitialState dfa46FinalStates dfa46Transition s, -46) $ max (match dfa47InitialState dfa47FinalStates dfa47Transition s, -47) $ max (match dfa48InitialState dfa48FinalStates dfa48Transition s, -48) $ max (match dfa49InitialState dfa49FinalStates dfa49Transition s, -49) $ max (match dfa50InitialState dfa50FinalStates dfa50Transition s, -50) $ max (match dfa51InitialState dfa51FinalStates dfa51Transition s, -51) $ max (match dfa52InitialState dfa52FinalStates dfa52Transition s, -52) $ max (match dfa53InitialState dfa53FinalStates dfa53Transition s, -53) $ max (match dfa54InitialState dfa54FinalStates dfa54Transition s, -54) $ max (match dfa55InitialState dfa55FinalStates dfa55Transition s, -55) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -0 -> do
                x <- initialSpace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -1 -> do
                x <- initialNewline actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -2 -> do
                x <- initialAny actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -3 -> do
                x <- initialLu actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -4 -> do
                x <- initialLl actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -5 -> do
                x <- initialLt actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -6 -> do
                x <- initialLm actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -7 -> do
                x <- initialLo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -8 -> do
                x <- initialMn actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -9 -> do
                x <- initialMc actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -10 -> do
                x <- initialMe actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -11 -> do
                x <- initialNd actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -12 -> do
                x <- initialNl actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -13 -> do
                x <- initialNo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -14 -> do
                x <- initialPc actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -15 -> do
                x <- initialPd actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -16 -> do
                x <- initialPs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -17 -> do
                x <- initialPe actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -18 -> do
                x <- initialPi actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -19 -> do
                x <- initialPf actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -20 -> do
                x <- initialPo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -21 -> do
                x <- initialSm actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -22 -> do
                x <- initialSc actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -23 -> do
                x <- initialSk actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -24 -> do
                x <- initialSo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -25 -> do
                x <- initialZs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -26 -> do
                x <- initialZl actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -27 -> do
                x <- initialZp actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -28 -> do
                x <- initialCc actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -29 -> do
                x <- initialCf actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -30 -> do
                x <- initialCs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -31 -> do
                x <- initialCo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -32 -> do
                x <- initialCn actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -33 -> do
                x <- initialHat actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -34 -> do
                x <- initialHyphen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -35 -> do
                x <- initialLBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -36 -> do
                x <- initialRBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -37 -> do
                x <- initialComma actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -38 -> do
                x <- initialDArrow actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -39 -> do
                x <- initialCase actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -40 -> do
                x <- initialStar actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -41 -> do
                x <- initialPlus actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -42 -> do
                x <- initialQues actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -43 -> do
                x <- initialLParen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -44 -> do
                x <- initialRParen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -45 -> do
                x <- initialString actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -46 -> do
                x <- initialChar actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -47 -> do
                x <- initialPLBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -48 -> do
                x <- initialPModule actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -49 -> do
                x <- initialPP actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -50 -> do
                x <- initialPipe actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -51 -> do
                x <- initialPWhere actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -52 -> do
                x <- initialPRBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -53 -> do
                x <- initialSemanticAction actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -54 -> do
                x <- initialLexingState actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -55 -> do
                x <- initialCode actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == Rule then
      case max (match dfa56InitialState dfa56FinalStates dfa56Transition s, -56) $ max (match dfa57InitialState dfa57FinalStates dfa57Transition s, -57) $ max (match dfa58InitialState dfa58FinalStates dfa58Transition s, -58) $ max (match dfa59InitialState dfa59FinalStates dfa59Transition s, -59) $ max (match dfa60InitialState dfa60FinalStates dfa60Transition s, -60) $ max (match dfa61InitialState dfa61FinalStates dfa61Transition s, -61) $ max (match dfa62InitialState dfa62FinalStates dfa62Transition s, -62) $ max (match dfa63InitialState dfa63FinalStates dfa63Transition s, -63) $ max (match dfa64InitialState dfa64FinalStates dfa64Transition s, -64) $ max (match dfa65InitialState dfa65FinalStates dfa65Transition s, -65) $ max (match dfa66InitialState dfa66FinalStates dfa66Transition s, -66) $ max (match dfa67InitialState dfa67FinalStates dfa67Transition s, -67) $ max (match dfa68InitialState dfa68FinalStates dfa68Transition s, -68) $ max (match dfa69InitialState dfa69FinalStates dfa69Transition s, -69) $ max (match dfa70InitialState dfa70FinalStates dfa70Transition s, -70) $ max (match dfa71InitialState dfa71FinalStates dfa71Transition s, -71) $ max (match dfa72InitialState dfa72FinalStates dfa72Transition s, -72) $ max (match dfa73InitialState dfa73FinalStates dfa73Transition s, -73) $ max (match dfa74InitialState dfa74FinalStates dfa74Transition s, -74) $ max (match dfa75InitialState dfa75FinalStates dfa75Transition s, -75) $ max (match dfa76InitialState dfa76FinalStates dfa76Transition s, -76) $ max (match dfa77InitialState dfa77FinalStates dfa77Transition s, -77) $ max (match dfa78InitialState dfa78FinalStates dfa78Transition s, -78) $ max (match dfa79InitialState dfa79FinalStates dfa79Transition s, -79) $ max (match dfa80InitialState dfa80FinalStates dfa80Transition s, -80) $ max (match dfa81InitialState dfa81FinalStates dfa81Transition s, -81) $ max (match dfa82InitialState dfa82FinalStates dfa82Transition s, -82) $ max (match dfa83InitialState dfa83FinalStates dfa83Transition s, -83) $ max (match dfa84InitialState dfa84FinalStates dfa84Transition s, -84) $ max (match dfa85InitialState dfa85FinalStates dfa85Transition s, -85) $ max (match dfa86InitialState dfa86FinalStates dfa86Transition s, -86) $ max (match dfa87InitialState dfa87FinalStates dfa87Transition s, -87) $ max (match dfa88InitialState dfa88FinalStates dfa88Transition s, -88) $ max (match dfa89InitialState dfa89FinalStates dfa89Transition s, -89) $ max (match dfa90InitialState dfa90FinalStates dfa90Transition s, -90) $ max (match dfa91InitialState dfa91FinalStates dfa91Transition s, -91) $ max (match dfa92InitialState dfa92FinalStates dfa92Transition s, -92) $ max (match dfa93InitialState dfa93FinalStates dfa93Transition s, -93) $ max (match dfa94InitialState dfa94FinalStates dfa94Transition s, -94) $ max (match dfa95InitialState dfa95FinalStates dfa95Transition s, -95) $ max (match dfa96InitialState dfa96FinalStates dfa96Transition s, -96) $ max (match dfa97InitialState dfa97FinalStates dfa97Transition s, -97) $ max (match dfa98InitialState dfa98FinalStates dfa98Transition s, -98) $ max (match dfa99InitialState dfa99FinalStates dfa99Transition s, -99) $ max (match dfa100InitialState dfa100FinalStates dfa100Transition s, -100) $ max (match dfa101InitialState dfa101FinalStates dfa101Transition s, -101) $ max (match dfa102InitialState dfa102FinalStates dfa102Transition s, -102) $ max (match dfa103InitialState dfa103FinalStates dfa103Transition s, -103) $ max (match dfa104InitialState dfa104FinalStates dfa104Transition s, -104) $ max (match dfa105InitialState dfa105FinalStates dfa105Transition s, -105) $ max (match dfa106InitialState dfa106FinalStates dfa106Transition s, -106) $ max (match dfa107InitialState dfa107FinalStates dfa107Transition s, -107) $ max (match dfa108InitialState dfa108FinalStates dfa108Transition s, -108) $ max (match dfa109InitialState dfa109FinalStates dfa109Transition s, -109) $ max (match dfa110InitialState dfa110FinalStates dfa110Transition s, -110) $ max (match dfa111InitialState dfa111FinalStates dfa111Transition s, -111) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -56 -> do
                x <- ruleSpace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -57 -> do
                x <- ruleNewline actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -58 -> do
                x <- ruleAny actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -59 -> do
                x <- ruleLu actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -60 -> do
                x <- ruleLl actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -61 -> do
                x <- ruleLt actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -62 -> do
                x <- ruleLm actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -63 -> do
                x <- ruleLo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -64 -> do
                x <- ruleMn actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -65 -> do
                x <- ruleMc actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -66 -> do
                x <- ruleMe actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -67 -> do
                x <- ruleNd actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -68 -> do
                x <- ruleNl actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -69 -> do
                x <- ruleNo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -70 -> do
                x <- rulePc actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -71 -> do
                x <- rulePd actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -72 -> do
                x <- rulePs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -73 -> do
                x <- rulePe actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -74 -> do
                x <- rulePi actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -75 -> do
                x <- rulePf actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -76 -> do
                x <- rulePo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -77 -> do
                x <- ruleSm actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -78 -> do
                x <- ruleSc actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -79 -> do
                x <- ruleSk actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -80 -> do
                x <- ruleSo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -81 -> do
                x <- ruleZs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -82 -> do
                x <- ruleZl actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -83 -> do
                x <- ruleZp actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -84 -> do
                x <- ruleCc actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -85 -> do
                x <- ruleCf actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -86 -> do
                x <- ruleCs actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -87 -> do
                x <- ruleCo actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -88 -> do
                x <- ruleCn actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -89 -> do
                x <- ruleHat actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -90 -> do
                x <- ruleHyphen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -91 -> do
                x <- ruleLBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -92 -> do
                x <- ruleRBracket actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -93 -> do
                x <- ruleComma actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -94 -> do
                x <- ruleDArrow actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -95 -> do
                x <- ruleCase actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -96 -> do
                x <- ruleStar actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -97 -> do
                x <- rulePlus actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -98 -> do
                x <- ruleQues actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -99 -> do
                x <- ruleLParen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -100 -> do
                x <- ruleRParen actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -101 -> do
                x <- ruleString actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -102 -> do
                x <- ruleChar actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -103 -> do
                x <- rulePLBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -104 -> do
                x <- rulePModule actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -105 -> do
                x <- rulePP actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -106 -> do
                x <- rulePipe actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -107 -> do
                x <- rulePWhere actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -108 -> do
                x <- rulePRBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -109 -> do
                x <- ruleSemanticAction actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -110 -> do
                x <- ruleLexingState actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -111 -> do
                x <- ruleCode actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else if p == Code then
      case max (match dfa112InitialState dfa112FinalStates dfa112Transition s, -112) $ max (match dfa113InitialState dfa113FinalStates dfa113Transition s, -113) $ max (match dfa114InitialState dfa114FinalStates dfa114Transition s, -114) $ max (match dfa115InitialState dfa115FinalStates dfa115Transition s, -115) $ max (match dfa116InitialState dfa116FinalStates dfa116Transition s, -116) $ max (match dfa117InitialState dfa117FinalStates dfa117Transition s, -117) $ (Nothing, 1 :: Int) of
        (Nothing, _) ->
          return ([], s)
        (Just 0, _) ->
          return ([], s)
        (Just i, j) ->
          let (yytext, s') = splitAt i s in
            case j of
              -112 -> do
                x <- codePLBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -113 -> do
                x <- codePModule actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -114 -> do
                x <- codePP actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -115 -> do
                x <- codePWhere actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -116 -> do
                x <- codePRBrace actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              -117 -> do
                x <- codeCode actions yytext
                (xs, s'') <- lex' s'
                return (x : xs, s'')
              _ ->
                return ([], s)
    else
      return ([], s)



semanticActions :: Monad m => SemanticActions m (Maybe Parsing.Token)
semanticActions = SemanticActions
  { initialSpace = const $ return Nothing
  , initialNewline = const $ return Nothing
  , initialAny = const $ return $ Just $ Parsing.ANY ()
  , initialLu = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.UppercaseLetter
  , initialLl = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.LowercaseLetter
  , initialLt = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.TitlecaseLetter
  , initialLm = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ModifierLetter
  , initialLo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OtherLetter
  , initialMn = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.NonSpacingMark
  , initialMc = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.SpacingCombiningMark
  , initialMe = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.EnclosingMark
  , initialNd = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.DecimalNumber
  , initialNl = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.LetterNumber
  , initialNo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OtherNumber
  , initialPc = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ConnectorPunctuation
  , initialPd = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.DashPunctuation
  , initialPs = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OpenPunctuation
  , initialPe = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ClosePunctuation
  , initialPi = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.InitialQuote
  , initialPf = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.FinalQuote
  , initialPo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OtherPunctuation
  , initialSm = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.MathSymbol
  , initialSc = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.CurrencySymbol
  , initialSk = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ModifierSymbol
  , initialSo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OtherSymbol
  , initialZs = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.Space
  , initialZl = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.LineSeparator
  , initialZp = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ParagraphSeparator
  , initialCc = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.Control
  , initialCf = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.Format
  , initialCs = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.Surrogate
  , initialCo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.PrivateUse
  , initialCn = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.NotAssigned
  , initialHat = const $ return $ Just $ Parsing.HAT ()
  , initialHyphen = const $ return $ Just $ Parsing.HYPHEN ()
  , initialLBracket = const $ return $ Just $ Parsing.LBRACKET ()
  , initialRBracket = const $ return $ Just $ Parsing.RBRACKET ()
  , initialComma = const $ return $ Just $ Parsing.COMMA ()
  , initialDArrow = const $ return $ Just $ Parsing.DARROW ()
  , initialCase = const $ return $ Just $ Parsing.CASE ()
  , initialStar = const $ return $ Just $ Parsing.STAR ()
  , initialPlus = const $ return $ Just $ Parsing.PLUS ()
  , initialQues = const $ return $ Just $ Parsing.QUES ()
  , initialLParen = const $ return $ Just $ Parsing.LPAREN ()
  , initialRParen = const $ return $ Just $ Parsing.RPAREN ()
  , initialString = return . Just . Parsing.STRING . read
  , initialChar = return . Just . Parsing.CHAR . read
  , initialPLBrace = const $ do { yybegin Code; return $ Just $ Parsing.PLBRACE () }
  , initialPModule = const $ do { yybegin Code; return $ Just $ Parsing.PMODULE () }
  , initialPP = const $ do { yybegin Rule; return $ Just $ Parsing.PP () }
  , initialPipe = const $ return $ Just $ Parsing.PIPE ()
  , initialPWhere = const $ return $ Just $ Parsing.PWHERE ()
  , initialPRBrace = const $ return $ Just $ Parsing.PRBRACE ()
  , initialSemanticAction = return . Just . Parsing.SEMANTIC_ACTION
  , initialLexingState = return . Just . Parsing.LEXING_STATE
  , initialCode = return . Just . Parsing.CODE . head
  , ruleSpace = const $ return Nothing
  , ruleNewline = const $ return Nothing
  , ruleAny = const $ return $ Just $ Parsing.ANY ()
  , ruleLu = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.UppercaseLetter
  , ruleLl = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.LowercaseLetter
  , ruleLt = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.TitlecaseLetter
  , ruleLm = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ModifierLetter
  , ruleLo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OtherLetter
  , ruleMn = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.NonSpacingMark
  , ruleMc = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.SpacingCombiningMark
  , ruleMe = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.EnclosingMark
  , ruleNd = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.DecimalNumber
  , ruleNl = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.LetterNumber
  , ruleNo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OtherNumber
  , rulePc = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ConnectorPunctuation
  , rulePd = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.DashPunctuation
  , rulePs = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OpenPunctuation
  , rulePe = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ClosePunctuation
  , rulePi = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.InitialQuote
  , rulePf = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.FinalQuote
  , rulePo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OtherPunctuation
  , ruleSm = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.MathSymbol
  , ruleSc = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.CurrencySymbol
  , ruleSk = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ModifierSymbol
  , ruleSo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.OtherSymbol
  , ruleZs = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.Space
  , ruleZl = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.LineSeparator
  , ruleZp = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.ParagraphSeparator
  , ruleCc = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.Control
  , ruleCf = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.Format
  , ruleCs = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.Surrogate
  , ruleCo = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.PrivateUse
  , ruleCn = const $ return $ Just $ Parsing.GENERAL_CATEGORY Char.NotAssigned
  , ruleHat = const $ return $ Just $ Parsing.HAT ()
  , ruleHyphen = const $ return $ Just $ Parsing.HYPHEN ()
  , ruleLBracket = const $ return $ Just $ Parsing.LBRACKET ()
  , ruleRBracket = const $ return $ Just $ Parsing.RBRACKET ()
  , ruleComma = const $ return $ Just $ Parsing.COMMA ()
  , ruleDArrow = const $ return $ Just $ Parsing.DARROW ()
  , ruleCase = const $ return $ Just $ Parsing.CASE ()
  , ruleStar = const $ return $ Just $ Parsing.STAR ()
  , rulePlus = const $ return $ Just $ Parsing.PLUS ()
  , ruleQues = const $ return $ Just $ Parsing.QUES ()
  , ruleLParen = const $ return $ Just $ Parsing.LPAREN ()
  , ruleRParen = const $ return $ Just $ Parsing.RPAREN ()
  , ruleString = return . Just . Parsing.STRING . read
  , ruleChar = return . Just . Parsing.CHAR . read
  , rulePLBrace = const $ do { yybegin Code; return $ Just $ Parsing.PLBRACE () }
  , rulePModule = const $ do { yybegin Code; return $ Just $ Parsing.PMODULE () }
  , rulePP = const $ do { yybegin Code; return $ Just $ Parsing.PP () }
  , rulePipe = const $ return $ Just $ Parsing.PIPE ()
  , rulePWhere = const $ return $ Just $ Parsing.PWHERE ()
  , rulePRBrace = const $ return $ Just $ Parsing.PRBRACE ()
  , ruleSemanticAction = return . Just . Parsing.SEMANTIC_ACTION
  , ruleLexingState = return . Just . Parsing.LEXING_STATE
  , ruleCode = return . Just . Parsing.CODE . head
  , codePLBrace = const $ do { yybegin Code; return $ Just $ Parsing.PLBRACE () }
  , codePModule = const $ do { yybegin Code; return $ Just $ Parsing.PMODULE () }
  , codePP = const $ return $ Just $ Parsing.PP ()
  , codePWhere = const $ do { yybegin Initial; return $ Just $ Parsing.PWHERE () }
  , codePRBrace = const $ do { yybegin Initial; return $ Just $ Parsing.PRBRACE () }
  , codeCode = return . Just . Parsing.CODE . head }


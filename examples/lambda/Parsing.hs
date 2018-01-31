module  Parsing  where
import qualified Control.Monad as Monad


type Pos = Int
type LAMBDA = Pos
type DOT = Pos
type LPAREN = Pos
type RPAREN = Pos
type ID = (Pos, String)

type Id = ID
type Ids = [Id]
type Var = Exp
type App = Exp
type Abs = Exp

data Exp =
    Var ID
  | App Exp Exp
  | Abs ID Exp
  deriving (Eq, Ord, Read, Show)

data Token =
    DOT DOT
  | ID ID
  | LAMBDA LAMBDA
  | LPAREN LPAREN
  | RPAREN RPAREN
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
type GotoState = Int
type GotoSymbol = Int

data StackValue =
    StackValue_EOF
  | StackValue_ID ID
  | StackValue_LPAREN LPAREN
  | StackValue_RPAREN RPAREN
  | StackValue_LAMBDA LAMBDA
  | StackValue_DOT DOT
  | StackValue_id Id
  | StackValue_ids Ids
  | StackValue_var Var
  | StackValue_abs Abs
  | StackValue_app App

data SemanticActions m = SemanticActions
  { id_implies_ID :: ID -> m Id
  , ids_implies_id :: Id -> m Ids
  , ids_implies_id_ids :: Id -> Ids -> m Ids
  , var_implies_id :: Id -> m Var
  , var_implies_LPAREN_abs_RPAREN :: LPAREN -> Abs -> RPAREN -> m Var
  , app_implies_var :: Var -> m App
  , app_implies_app_var :: App -> Var -> m App
  , abs_implies_app :: App -> m Abs
  , abs_implies_LAMBDA_ids_DOT_app :: LAMBDA -> Ids -> DOT -> App -> m Abs }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token (ID _)) -> Just (Shift 8)
    (0, Token (LPAREN _)) -> Just (Shift 5)
    (0, Token (LAMBDA _)) -> Just (Shift 7)
    (1, EOF) -> Just (Accept)
    (2, Token (ID _)) -> Just (Shift 8)
    (2, Token (LPAREN _)) -> Just (Shift 5)
    (3, EOF) -> Just (Reduce 1 7)
    (3, Token (ID _)) -> Just (Shift 8)
    (3, Token (LPAREN _)) -> Just (Shift 5)
    (3, Token (RPAREN _)) -> Just (Reduce 1 7)
    (4, EOF) -> Just (Reduce 4 8)
    (4, Token (ID _)) -> Just (Shift 8)
    (4, Token (LPAREN _)) -> Just (Shift 5)
    (4, Token (RPAREN _)) -> Just (Reduce 4 8)
    (5, Token (ID _)) -> Just (Shift 8)
    (5, Token (LPAREN _)) -> Just (Shift 5)
    (5, Token (LAMBDA _)) -> Just (Shift 7)
    (6, Token (ID _)) -> Just (Shift 8)
    (6, Token (DOT _)) -> Just (Reduce 1 1)
    (7, Token (ID _)) -> Just (Shift 8)
    (8, EOF) -> Just (Reduce 1 0)
    (8, Token (ID _)) -> Just (Reduce 1 0)
    (8, Token (LPAREN _)) -> Just (Reduce 1 0)
    (8, Token (RPAREN _)) -> Just (Reduce 1 0)
    (8, Token (LAMBDA _)) -> Just (Reduce 1 0)
    (8, Token (DOT _)) -> Just (Reduce 1 0)
    (9, Token (DOT _)) -> Just (Reduce 2 2)
    (10, EOF) -> Just (Reduce 3 4)
    (10, Token (ID _)) -> Just (Reduce 3 4)
    (10, Token (LPAREN _)) -> Just (Reduce 3 4)
    (10, Token (RPAREN _)) -> Just (Reduce 3 4)
    (10, Token (LAMBDA _)) -> Just (Reduce 3 4)
    (11, EOF) -> Just (Reduce 1 3)
    (11, Token (ID _)) -> Just (Reduce 1 3)
    (11, Token (LPAREN _)) -> Just (Reduce 1 3)
    (11, Token (RPAREN _)) -> Just (Reduce 1 3)
    (11, Token (LAMBDA _)) -> Just (Reduce 1 3)
    (12, Token (RPAREN _)) -> Just (Shift 10)
    (13, Token (DOT _)) -> Just (Shift 2)
    (14, EOF) -> Just (Reduce 1 5)
    (14, Token (ID _)) -> Just (Reduce 1 5)
    (14, Token (LPAREN _)) -> Just (Reduce 1 5)
    (14, Token (RPAREN _)) -> Just (Reduce 1 5)
    (14, Token (LAMBDA _)) -> Just (Reduce 1 5)
    (15, EOF) -> Just (Reduce 2 6)
    (15, Token (ID _)) -> Just (Reduce 2 6)
    (15, Token (LPAREN _)) -> Just (Reduce 2 6)
    (15, Token (RPAREN _)) -> Just (Reduce 2 6)
    (15, Token (LAMBDA _)) -> Just (Reduce 2 6)
    (_, _) -> Nothing

production :: Int -> Int
production 0 = 0
production 1 = 1
production 2 = 1
production 3 = 2
production 4 = 2
production 5 = 4
production 6 = 4
production 7 = 3
production 8 = 3

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 11
    (0, 2) -> Just 14
    (0, 3) -> Just 1
    (0, 4) -> Just 3
    (2, 0) -> Just 11
    (2, 2) -> Just 14
    (2, 4) -> Just 4
    (3, 0) -> Just 11
    (3, 2) -> Just 15
    (4, 0) -> Just 11
    (4, 2) -> Just 15
    (5, 0) -> Just 11
    (5, 2) -> Just 14
    (5, 3) -> Just 12
    (5, 4) -> Just 3
    (6, 0) -> Just 6
    (6, 1) -> Just 9
    (7, 0) -> Just 6
    (7, 1) -> Just 13
    (_, _) -> Nothing

parse :: Monad m => SemanticActions m -> [Token] -> m (Either (Maybe Token) (Abs, [Token]))
parse actions = parse' [] where
  parse' stack tokens =
    let p =
          case stack of
            [] -> 0
            ((q, _) : _) -> q in
    let symbol =
          case tokens of
            [] -> EOF
            (token : _) -> Token token in do
      case dfaActionTransition p symbol of
        Nothing ->
          case tokens of
            [] -> return $ Left $ Nothing
            (token : _) -> return $ Left $ Just token
        Just (Shift n) ->
          let value =
                case symbol of
                  EOF ->
                    StackValue_EOF
                  Token (ID semanticValue) ->
                    StackValue_ID semanticValue
                  Token (LPAREN semanticValue) ->
                    StackValue_LPAREN semanticValue
                  Token (RPAREN semanticValue) ->
                    StackValue_RPAREN semanticValue
                  Token (LAMBDA semanticValue) ->
                    StackValue_LAMBDA semanticValue
                  Token (DOT semanticValue) ->
                    StackValue_DOT semanticValue
          in parse' ((n, value) : stack) (tail tokens)
        Just (Reduce n m) ->
          let (pop, stack') = splitAt n stack in
            case
              case stack' of
                [] -> dfaGotoTransition 0 m
                ((q', _) : _) -> dfaGotoTransition q' m of
              Nothing ->
                case tokens of
                  [] -> return $ Left $ Nothing
                  (token : _) -> return $ Left $ Just token
              Just q -> do
                value <-
                  case m of
                    0 ->
                      Monad.liftM StackValue_id $ id_implies_ID actions (case snd (pop !! 0) of { StackValue_ID value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_ids $ ids_implies_id actions (case snd (pop !! 0) of { StackValue_id value -> value; _ -> undefined })
                    2 ->
                      Monad.liftM StackValue_ids $ ids_implies_id_ids actions (case snd (pop !! 1) of { StackValue_id value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ids value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_var $ var_implies_id actions (case snd (pop !! 0) of { StackValue_id value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_abs_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_abs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_app $ app_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_app $ app_implies_app_var actions (case snd (pop !! 1) of { StackValue_app value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_abs $ abs_implies_app actions (case snd (pop !! 0) of { StackValue_app value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_abs $ abs_implies_LAMBDA_ids_DOT_app actions (case snd (pop !! 3) of { StackValue_LAMBDA value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_ids value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_app value -> value; _ -> undefined })
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_abs value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { id_implies_ID =
      return
  , ids_implies_id = \id' ->
      return [id']
  , ids_implies_id_ids = \id' ids ->
      return $ id' : ids
  , var_implies_id = \id' ->
      return $ Var id'
  , var_implies_LPAREN_abs_RPAREN = \_ exp _ ->
      return exp
  , app_implies_var =
      return
  , app_implies_app_var = \exp1 exp2 ->
      return $ App exp1 exp2
  , abs_implies_app =
      return
  , abs_implies_LAMBDA_ids_DOT_app = \_ ids _ exp ->
      return $ foldr Abs exp ids }


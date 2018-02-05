module  Parsing  where
import qualified Control.Monad as Monad


type Pos = (Int, Int)
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
  deriving (Eq, Ord, Read, Show)
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
  let s' =
        case s of
          EOF -> -1
          Token (DOT _) -> 4
          Token (ID _) -> 0
          Token (LAMBDA _) -> 3
          Token (LPAREN _) -> 1
          Token (RPAREN _) -> 2
  in
    case compare (q, s') (11, -1) of {
      LT ->
    case compare (q, s') (6, 0) of {
      LT ->
    case compare (q, s') (2, 1) of {
      LT ->
    case compare (q, s') (1, -1) of {
      LT ->
    case compare (q, s') (0, 3) of {
      LT ->
    case compare (q, s') (0, 1) of {
      LT ->
    case compare (q, s') (0, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 8);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 5);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 7);
      GT ->
        Nothing
    };
      EQ ->
        Just (Accept);
      GT ->
    case compare (q, s') (2, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 8);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 5);
      GT ->
    case compare (q, s') (4, -1) of {
      LT ->
    case compare (q, s') (3, 0) of {
      LT ->
    case compare (q, s') (3, -1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 7);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 8);
      GT ->
    case compare (q, s') (3, 1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 5);
      GT ->
    case compare (q, s') (3, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 7);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 4 8);
      GT ->
    case compare (q, s') (4, 1) of {
      LT ->
    case compare (q, s') (4, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 8);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 5);
      GT ->
    case compare (q, s') (5, 0) of {
      LT ->
    case compare (q, s') (4, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 4 8);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 8);
      GT ->
    case compare (q, s') (5, 1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 5);
      GT ->
    case compare (q, s') (5, 3) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 7);
      GT ->
        Nothing
    }
    }
    }
    }
    }
    };
      EQ ->
        Just (Shift 8);
      GT ->
    case compare (q, s') (10, 0) of {
      LT ->
    case compare (q, s') (8, 3) of {
      LT ->
    case compare (q, s') (8, 1) of {
      LT ->
    case compare (q, s') (8, -1) of {
      LT ->
    case compare (q, s') (7, 0) of {
      LT ->
    case compare (q, s') (6, 4) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 1);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 8);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 0);
      GT ->
    case compare (q, s') (8, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 0);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 0);
      GT ->
    case compare (q, s') (8, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 0);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 0);
      GT ->
    case compare (q, s') (9, 4) of {
      LT ->
    case compare (q, s') (8, 4) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 0);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 2);
      GT ->
    case compare (q, s') (10, -1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 4);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 3 4);
      GT ->
    case compare (q, s') (10, 2) of {
      LT ->
    case compare (q, s') (10, 1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 4);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 4);
      GT ->
    case compare (q, s') (10, 3) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 4);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Reduce 1 3);
      GT ->
    case compare (q, s') (14, 1) of {
      LT ->
    case compare (q, s') (11, 3) of {
      LT ->
    case compare (q, s') (11, 1) of {
      LT ->
    case compare (q, s') (11, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 3);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 3);
      GT ->
    case compare (q, s') (11, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 3);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 3);
      GT ->
    case compare (q, s') (13, 4) of {
      LT ->
    case compare (q, s') (12, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 10);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 2);
      GT ->
    case compare (q, s') (14, 0) of {
      LT ->
    case compare (q, s') (14, -1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 5);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 5);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 1 5);
      GT ->
    case compare (q, s') (15, 0) of {
      LT ->
    case compare (q, s') (14, 3) of {
      LT ->
    case compare (q, s') (14, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 5);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 5);
      GT ->
    case compare (q, s') (15, -1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 6);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 2 6);
      GT ->
    case compare (q, s') (15, 2) of {
      LT ->
    case compare (q, s') (15, 1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 6);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 6);
      GT ->
    case compare (q, s') (15, 3) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 6);
      GT ->
        Nothing
    }
    }
    }
    }
    }

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
  let s' = production s in
    case compare (q, s') (4, 0) of {
      LT ->
    case compare (q, s') (2, 0) of {
      LT ->
    case compare (q, s') (0, 3) of {
      LT ->
    case compare (q, s') (0, 2) of {
      LT ->
    case compare (q, s') (0, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just 11;
      GT ->
        Nothing
    };
      EQ ->
        Just 14;
      GT ->
        Nothing
    };
      EQ ->
        Just 1;
      GT ->
    case compare (q, s') (0, 4) of {
      LT ->
        Nothing;
      EQ ->
        Just 3;
      GT ->
        Nothing
    }
    };
      EQ ->
        Just 11;
      GT ->
    case compare (q, s') (3, 0) of {
      LT ->
    case compare (q, s') (2, 4) of {
      LT ->
    case compare (q, s') (2, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just 14;
      GT ->
        Nothing
    };
      EQ ->
        Just 4;
      GT ->
        Nothing
    };
      EQ ->
        Just 11;
      GT ->
    case compare (q, s') (3, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just 15;
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just 11;
      GT ->
    case compare (q, s') (5, 4) of {
      LT ->
    case compare (q, s') (5, 2) of {
      LT ->
    case compare (q, s') (5, 0) of {
      LT ->
    case compare (q, s') (4, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just 15;
      GT ->
        Nothing
    };
      EQ ->
        Just 11;
      GT ->
        Nothing
    };
      EQ ->
        Just 14;
      GT ->
    case compare (q, s') (5, 3) of {
      LT ->
        Nothing;
      EQ ->
        Just 12;
      GT ->
        Nothing
    }
    };
      EQ ->
        Just 3;
      GT ->
    case compare (q, s') (7, 0) of {
      LT ->
    case compare (q, s') (6, 1) of {
      LT ->
    case compare (q, s') (6, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just 6;
      GT ->
        Nothing
    };
      EQ ->
        Just 9;
      GT ->
        Nothing
    };
      EQ ->
        Just 6;
      GT ->
    case compare (q, s') (7, 1) of {
      LT ->
        Nothing;
      EQ ->
        Just 13;
      GT ->
        Nothing
    }
    }
    }
    }

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


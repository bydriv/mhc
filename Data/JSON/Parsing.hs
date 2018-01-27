module  Data.JSON.Parsing  where
import qualified Control.Monad as Monad


import qualified Data.RBMap as RBMap

type FALSE = ()
type NULL = ()
type TRUE = ()
type LBRACE = ()
type RBRACE = ()
type COMMA = ()
type COLON = ()
type LBRACKET = ()
type RBRACKET = ()
type NUMBER = Double
type STRING = String

data JSON =
    JSONFalse
  | JSONNull
  | JSONTrue
  | JSONObject (RBMap.RBMap STRING JSON)
  | JSONArray [JSON]
  | JSONNumber NUMBER
  | JSONString STRING
  deriving (Eq, Ord, Read, Show)

type Value = JSON
type False = JSON
type Null = JSON
type True = JSON
type Object = JSON
type Members = RBMap.RBMap STRING JSON
type Members_opt = Maybe Members
type Member = (STRING, JSON)
type Array = JSON
type Values = [JSON]
type Values_opt = Maybe Values
type Number = JSON
type String' = JSON

data Token =
    COLON COLON
  | COMMA COMMA
  | FALSE FALSE
  | LBRACE LBRACE
  | LBRACKET LBRACKET
  | NULL NULL
  | NUMBER NUMBER
  | RBRACE RBRACE
  | RBRACKET RBRACKET
  | STRING STRING
  | TRUE TRUE
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
type GotoState = Int
type GotoSymbol = Int

data StackValue =
    StackValue_EOF
  | StackValue_FALSE FALSE
  | StackValue_NULL NULL
  | StackValue_TRUE TRUE
  | StackValue_LBRACE LBRACE
  | StackValue_RBRACE RBRACE
  | StackValue_COMMA COMMA
  | StackValue_STRING STRING
  | StackValue_COLON COLON
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_NUMBER NUMBER
  | StackValue_value Value
  | StackValue_false False
  | StackValue_null Null
  | StackValue_true True
  | StackValue_object Object
  | StackValue_array Array
  | StackValue_number Number
  | StackValue_string' String'
  | StackValue_members Members
  | StackValue_member Member
  | StackValue_members_opt Members_opt
  | StackValue_values Values
  | StackValue_values_opt Values_opt

data SemanticActions m = SemanticActions
  { value_implies_false :: False -> m Value
  , value_implies_null :: Null -> m Value
  , value_implies_true :: True -> m Value
  , value_implies_object :: Object -> m Value
  , value_implies_array :: Array -> m Value
  , value_implies_number :: Number -> m Value
  , value_implies_string' :: String' -> m Value
  , false_implies_FALSE :: FALSE -> m False
  , null_implies_NULL :: NULL -> m Null
  , true_implies_TRUE :: TRUE -> m True
  , object_implies_LBRACE_members_RBRACE :: LBRACE -> Members -> RBRACE -> m Object
  , members_implies :: m Members
  , members_implies_member_members_opt :: Member -> Members_opt -> m Members
  , members_opt_implies :: m Members_opt
  , members_opt_implies_COMMA_members :: COMMA -> Members -> m Members_opt
  , member_implies_STRING_COLON_value :: STRING -> COLON -> Value -> m Member
  , array_implies_LBRACKET_values_RBRACKET :: LBRACKET -> Values -> RBRACKET -> m Array
  , values_implies :: m Values
  , values_implies_value_values_opt :: Value -> Values_opt -> m Values
  , values_opt_implies :: m Values_opt
  , values_opt_implies_COMMA_values :: COMMA -> Values -> m Values_opt
  , number_implies_NUMBER :: NUMBER -> m Number
  , string'_implies_STRING :: STRING -> m String' }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token (FALSE _)) -> Just (Shift 12)
    (0, Token (NULL _)) -> Just (Shift 13)
    (0, Token (TRUE _)) -> Just (Shift 14)
    (0, Token (LBRACE _)) -> Just (Shift 15)
    (0, Token (STRING _)) -> Just (Shift 21)
    (0, Token (LBRACKET _)) -> Just (Shift 2)
    (0, Token (NUMBER _)) -> Just (Shift 20)
    (1, EOF) -> Just (Accept)
    (2, Token (FALSE _)) -> Just (Shift 12)
    (2, Token (NULL _)) -> Just (Shift 13)
    (2, Token (TRUE _)) -> Just (Shift 14)
    (2, Token (LBRACE _)) -> Just (Shift 15)
    (2, Token (STRING _)) -> Just (Shift 21)
    (2, Token (LBRACKET _)) -> Just (Shift 2)
    (2, Token (RBRACKET _)) -> Just (Reduce 0 17)
    (2, Token (NUMBER _)) -> Just (Shift 20)
    (3, Token (FALSE _)) -> Just (Shift 12)
    (3, Token (NULL _)) -> Just (Shift 13)
    (3, Token (TRUE _)) -> Just (Shift 14)
    (3, Token (LBRACE _)) -> Just (Shift 15)
    (3, Token (STRING _)) -> Just (Shift 21)
    (3, Token (LBRACKET _)) -> Just (Shift 2)
    (3, Token (RBRACKET _)) -> Just (Reduce 0 17)
    (3, Token (NUMBER _)) -> Just (Shift 20)
    (4, Token (FALSE _)) -> Just (Shift 12)
    (4, Token (NULL _)) -> Just (Shift 13)
    (4, Token (TRUE _)) -> Just (Shift 14)
    (4, Token (LBRACE _)) -> Just (Shift 15)
    (4, Token (STRING _)) -> Just (Shift 21)
    (4, Token (LBRACKET _)) -> Just (Shift 2)
    (4, Token (NUMBER _)) -> Just (Shift 20)
    (5, EOF) -> Just (Reduce 1 0)
    (5, Token (FALSE _)) -> Just (Reduce 1 0)
    (5, Token (NULL _)) -> Just (Reduce 1 0)
    (5, Token (TRUE _)) -> Just (Reduce 1 0)
    (5, Token (LBRACE _)) -> Just (Reduce 1 0)
    (5, Token (RBRACE _)) -> Just (Reduce 1 0)
    (5, Token (COMMA _)) -> Just (Reduce 1 0)
    (5, Token (STRING _)) -> Just (Reduce 1 0)
    (5, Token (COLON _)) -> Just (Reduce 1 0)
    (5, Token (LBRACKET _)) -> Just (Reduce 1 0)
    (5, Token (RBRACKET _)) -> Just (Reduce 1 0)
    (5, Token (NUMBER _)) -> Just (Reduce 1 0)
    (6, EOF) -> Just (Reduce 1 1)
    (6, Token (FALSE _)) -> Just (Reduce 1 1)
    (6, Token (NULL _)) -> Just (Reduce 1 1)
    (6, Token (TRUE _)) -> Just (Reduce 1 1)
    (6, Token (LBRACE _)) -> Just (Reduce 1 1)
    (6, Token (RBRACE _)) -> Just (Reduce 1 1)
    (6, Token (COMMA _)) -> Just (Reduce 1 1)
    (6, Token (STRING _)) -> Just (Reduce 1 1)
    (6, Token (COLON _)) -> Just (Reduce 1 1)
    (6, Token (LBRACKET _)) -> Just (Reduce 1 1)
    (6, Token (RBRACKET _)) -> Just (Reduce 1 1)
    (6, Token (NUMBER _)) -> Just (Reduce 1 1)
    (7, EOF) -> Just (Reduce 1 2)
    (7, Token (FALSE _)) -> Just (Reduce 1 2)
    (7, Token (NULL _)) -> Just (Reduce 1 2)
    (7, Token (TRUE _)) -> Just (Reduce 1 2)
    (7, Token (LBRACE _)) -> Just (Reduce 1 2)
    (7, Token (RBRACE _)) -> Just (Reduce 1 2)
    (7, Token (COMMA _)) -> Just (Reduce 1 2)
    (7, Token (STRING _)) -> Just (Reduce 1 2)
    (7, Token (COLON _)) -> Just (Reduce 1 2)
    (7, Token (LBRACKET _)) -> Just (Reduce 1 2)
    (7, Token (RBRACKET _)) -> Just (Reduce 1 2)
    (7, Token (NUMBER _)) -> Just (Reduce 1 2)
    (8, EOF) -> Just (Reduce 1 3)
    (8, Token (FALSE _)) -> Just (Reduce 1 3)
    (8, Token (NULL _)) -> Just (Reduce 1 3)
    (8, Token (TRUE _)) -> Just (Reduce 1 3)
    (8, Token (LBRACE _)) -> Just (Reduce 1 3)
    (8, Token (RBRACE _)) -> Just (Reduce 1 3)
    (8, Token (COMMA _)) -> Just (Reduce 1 3)
    (8, Token (STRING _)) -> Just (Reduce 1 3)
    (8, Token (COLON _)) -> Just (Reduce 1 3)
    (8, Token (LBRACKET _)) -> Just (Reduce 1 3)
    (8, Token (RBRACKET _)) -> Just (Reduce 1 3)
    (8, Token (NUMBER _)) -> Just (Reduce 1 3)
    (9, EOF) -> Just (Reduce 1 4)
    (9, Token (FALSE _)) -> Just (Reduce 1 4)
    (9, Token (NULL _)) -> Just (Reduce 1 4)
    (9, Token (TRUE _)) -> Just (Reduce 1 4)
    (9, Token (LBRACE _)) -> Just (Reduce 1 4)
    (9, Token (RBRACE _)) -> Just (Reduce 1 4)
    (9, Token (COMMA _)) -> Just (Reduce 1 4)
    (9, Token (STRING _)) -> Just (Reduce 1 4)
    (9, Token (COLON _)) -> Just (Reduce 1 4)
    (9, Token (LBRACKET _)) -> Just (Reduce 1 4)
    (9, Token (RBRACKET _)) -> Just (Reduce 1 4)
    (9, Token (NUMBER _)) -> Just (Reduce 1 4)
    (10, EOF) -> Just (Reduce 1 5)
    (10, Token (FALSE _)) -> Just (Reduce 1 5)
    (10, Token (NULL _)) -> Just (Reduce 1 5)
    (10, Token (TRUE _)) -> Just (Reduce 1 5)
    (10, Token (LBRACE _)) -> Just (Reduce 1 5)
    (10, Token (RBRACE _)) -> Just (Reduce 1 5)
    (10, Token (COMMA _)) -> Just (Reduce 1 5)
    (10, Token (STRING _)) -> Just (Reduce 1 5)
    (10, Token (COLON _)) -> Just (Reduce 1 5)
    (10, Token (LBRACKET _)) -> Just (Reduce 1 5)
    (10, Token (RBRACKET _)) -> Just (Reduce 1 5)
    (10, Token (NUMBER _)) -> Just (Reduce 1 5)
    (11, EOF) -> Just (Reduce 1 6)
    (11, Token (FALSE _)) -> Just (Reduce 1 6)
    (11, Token (NULL _)) -> Just (Reduce 1 6)
    (11, Token (TRUE _)) -> Just (Reduce 1 6)
    (11, Token (LBRACE _)) -> Just (Reduce 1 6)
    (11, Token (RBRACE _)) -> Just (Reduce 1 6)
    (11, Token (COMMA _)) -> Just (Reduce 1 6)
    (11, Token (STRING _)) -> Just (Reduce 1 6)
    (11, Token (COLON _)) -> Just (Reduce 1 6)
    (11, Token (LBRACKET _)) -> Just (Reduce 1 6)
    (11, Token (RBRACKET _)) -> Just (Reduce 1 6)
    (11, Token (NUMBER _)) -> Just (Reduce 1 6)
    (12, EOF) -> Just (Reduce 1 7)
    (12, Token (FALSE _)) -> Just (Reduce 1 7)
    (12, Token (NULL _)) -> Just (Reduce 1 7)
    (12, Token (TRUE _)) -> Just (Reduce 1 7)
    (12, Token (LBRACE _)) -> Just (Reduce 1 7)
    (12, Token (RBRACE _)) -> Just (Reduce 1 7)
    (12, Token (COMMA _)) -> Just (Reduce 1 7)
    (12, Token (STRING _)) -> Just (Reduce 1 7)
    (12, Token (COLON _)) -> Just (Reduce 1 7)
    (12, Token (LBRACKET _)) -> Just (Reduce 1 7)
    (12, Token (RBRACKET _)) -> Just (Reduce 1 7)
    (12, Token (NUMBER _)) -> Just (Reduce 1 7)
    (13, EOF) -> Just (Reduce 1 8)
    (13, Token (FALSE _)) -> Just (Reduce 1 8)
    (13, Token (NULL _)) -> Just (Reduce 1 8)
    (13, Token (TRUE _)) -> Just (Reduce 1 8)
    (13, Token (LBRACE _)) -> Just (Reduce 1 8)
    (13, Token (RBRACE _)) -> Just (Reduce 1 8)
    (13, Token (COMMA _)) -> Just (Reduce 1 8)
    (13, Token (STRING _)) -> Just (Reduce 1 8)
    (13, Token (COLON _)) -> Just (Reduce 1 8)
    (13, Token (LBRACKET _)) -> Just (Reduce 1 8)
    (13, Token (RBRACKET _)) -> Just (Reduce 1 8)
    (13, Token (NUMBER _)) -> Just (Reduce 1 8)
    (14, EOF) -> Just (Reduce 1 9)
    (14, Token (FALSE _)) -> Just (Reduce 1 9)
    (14, Token (NULL _)) -> Just (Reduce 1 9)
    (14, Token (TRUE _)) -> Just (Reduce 1 9)
    (14, Token (LBRACE _)) -> Just (Reduce 1 9)
    (14, Token (RBRACE _)) -> Just (Reduce 1 9)
    (14, Token (COMMA _)) -> Just (Reduce 1 9)
    (14, Token (STRING _)) -> Just (Reduce 1 9)
    (14, Token (COLON _)) -> Just (Reduce 1 9)
    (14, Token (LBRACKET _)) -> Just (Reduce 1 9)
    (14, Token (RBRACKET _)) -> Just (Reduce 1 9)
    (14, Token (NUMBER _)) -> Just (Reduce 1 9)
    (15, Token (RBRACE _)) -> Just (Reduce 0 11)
    (15, Token (STRING _)) -> Just (Shift 25)
    (16, EOF) -> Just (Reduce 3 10)
    (16, Token (FALSE _)) -> Just (Reduce 3 10)
    (16, Token (NULL _)) -> Just (Reduce 3 10)
    (16, Token (TRUE _)) -> Just (Reduce 3 10)
    (16, Token (LBRACE _)) -> Just (Reduce 3 10)
    (16, Token (RBRACE _)) -> Just (Reduce 3 10)
    (16, Token (COMMA _)) -> Just (Reduce 3 10)
    (16, Token (STRING _)) -> Just (Reduce 3 10)
    (16, Token (COLON _)) -> Just (Reduce 3 10)
    (16, Token (LBRACKET _)) -> Just (Reduce 3 10)
    (16, Token (RBRACKET _)) -> Just (Reduce 3 10)
    (16, Token (NUMBER _)) -> Just (Reduce 3 10)
    (17, Token (RBRACE _)) -> Just (Shift 16)
    (18, EOF) -> Just (Reduce 3 16)
    (18, Token (FALSE _)) -> Just (Reduce 3 16)
    (18, Token (NULL _)) -> Just (Reduce 3 16)
    (18, Token (TRUE _)) -> Just (Reduce 3 16)
    (18, Token (LBRACE _)) -> Just (Reduce 3 16)
    (18, Token (RBRACE _)) -> Just (Reduce 3 16)
    (18, Token (COMMA _)) -> Just (Reduce 3 16)
    (18, Token (STRING _)) -> Just (Reduce 3 16)
    (18, Token (COLON _)) -> Just (Reduce 3 16)
    (18, Token (LBRACKET _)) -> Just (Reduce 3 16)
    (18, Token (RBRACKET _)) -> Just (Reduce 3 16)
    (18, Token (NUMBER _)) -> Just (Reduce 3 16)
    (19, Token (RBRACKET _)) -> Just (Shift 18)
    (20, EOF) -> Just (Reduce 1 21)
    (20, Token (FALSE _)) -> Just (Reduce 1 21)
    (20, Token (NULL _)) -> Just (Reduce 1 21)
    (20, Token (TRUE _)) -> Just (Reduce 1 21)
    (20, Token (LBRACE _)) -> Just (Reduce 1 21)
    (20, Token (RBRACE _)) -> Just (Reduce 1 21)
    (20, Token (COMMA _)) -> Just (Reduce 1 21)
    (20, Token (STRING _)) -> Just (Reduce 1 21)
    (20, Token (COLON _)) -> Just (Reduce 1 21)
    (20, Token (LBRACKET _)) -> Just (Reduce 1 21)
    (20, Token (RBRACKET _)) -> Just (Reduce 1 21)
    (20, Token (NUMBER _)) -> Just (Reduce 1 21)
    (21, EOF) -> Just (Reduce 1 22)
    (21, Token (FALSE _)) -> Just (Reduce 1 22)
    (21, Token (NULL _)) -> Just (Reduce 1 22)
    (21, Token (TRUE _)) -> Just (Reduce 1 22)
    (21, Token (LBRACE _)) -> Just (Reduce 1 22)
    (21, Token (RBRACE _)) -> Just (Reduce 1 22)
    (21, Token (COMMA _)) -> Just (Reduce 1 22)
    (21, Token (STRING _)) -> Just (Reduce 1 22)
    (21, Token (COLON _)) -> Just (Reduce 1 22)
    (21, Token (LBRACKET _)) -> Just (Reduce 1 22)
    (21, Token (RBRACKET _)) -> Just (Reduce 1 22)
    (21, Token (NUMBER _)) -> Just (Reduce 1 22)
    (22, Token (RBRACE _)) -> Just (Reduce 0 11)
    (22, Token (STRING _)) -> Just (Shift 25)
    (23, Token (RBRACE _)) -> Just (Reduce 0 13)
    (23, Token (COMMA _)) -> Just (Shift 22)
    (24, Token (RBRACE _)) -> Just (Reduce 2 12)
    (25, Token (COLON _)) -> Just (Shift 4)
    (26, Token (RBRACE _)) -> Just (Reduce 3 15)
    (26, Token (COMMA _)) -> Just (Reduce 3 15)
    (26, Token (STRING _)) -> Just (Reduce 3 15)
    (26, Token (COLON _)) -> Just (Reduce 3 15)
    (27, Token (RBRACE _)) -> Just (Reduce 2 14)
    (28, Token (COMMA _)) -> Just (Shift 3)
    (28, Token (RBRACKET _)) -> Just (Reduce 0 19)
    (29, Token (RBRACKET _)) -> Just (Reduce 2 18)
    (30, Token (RBRACKET _)) -> Just (Reduce 2 20)
    (_, _) -> Nothing

production :: Int -> Int
production 0 = 0
production 1 = 0
production 2 = 0
production 3 = 0
production 4 = 0
production 5 = 0
production 6 = 0
production 7 = 1
production 8 = 2
production 9 = 3
production 10 = 4
production 11 = 8
production 12 = 8
production 13 = 10
production 14 = 10
production 15 = 9
production 16 = 5
production 17 = 11
production 18 = 11
production 19 = 12
production 20 = 12
production 21 = 6
production 22 = 7

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 1) -> Just 5
    (0, 2) -> Just 6
    (0, 3) -> Just 7
    (0, 4) -> Just 8
    (0, 5) -> Just 9
    (0, 6) -> Just 10
    (0, 7) -> Just 11
    (2, 0) -> Just 28
    (2, 1) -> Just 5
    (2, 2) -> Just 6
    (2, 3) -> Just 7
    (2, 4) -> Just 8
    (2, 5) -> Just 9
    (2, 6) -> Just 10
    (2, 7) -> Just 11
    (2, 11) -> Just 19
    (3, 0) -> Just 28
    (3, 1) -> Just 5
    (3, 2) -> Just 6
    (3, 3) -> Just 7
    (3, 4) -> Just 8
    (3, 5) -> Just 9
    (3, 6) -> Just 10
    (3, 7) -> Just 11
    (3, 11) -> Just 30
    (4, 0) -> Just 26
    (4, 1) -> Just 5
    (4, 2) -> Just 6
    (4, 3) -> Just 7
    (4, 4) -> Just 8
    (4, 5) -> Just 9
    (4, 6) -> Just 10
    (4, 7) -> Just 11
    (15, 8) -> Just 17
    (15, 9) -> Just 23
    (22, 8) -> Just 27
    (22, 9) -> Just 23
    (23, 10) -> Just 24
    (28, 12) -> Just 29
    (_, _) -> Nothing

parse :: Monad m => SemanticActions m -> [Token] -> m (Maybe (Value, [Token]))
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
          return Nothing
        Just (Shift n) ->
          let value =
                case symbol of
                  EOF ->
                    StackValue_EOF
                  Token (FALSE semanticValue) ->
                    StackValue_FALSE semanticValue
                  Token (NULL semanticValue) ->
                    StackValue_NULL semanticValue
                  Token (TRUE semanticValue) ->
                    StackValue_TRUE semanticValue
                  Token (LBRACE semanticValue) ->
                    StackValue_LBRACE semanticValue
                  Token (RBRACE semanticValue) ->
                    StackValue_RBRACE semanticValue
                  Token (COMMA semanticValue) ->
                    StackValue_COMMA semanticValue
                  Token (STRING semanticValue) ->
                    StackValue_STRING semanticValue
                  Token (COLON semanticValue) ->
                    StackValue_COLON semanticValue
                  Token (LBRACKET semanticValue) ->
                    StackValue_LBRACKET semanticValue
                  Token (RBRACKET semanticValue) ->
                    StackValue_RBRACKET semanticValue
                  Token (NUMBER semanticValue) ->
                    StackValue_NUMBER semanticValue
          in parse' ((n, value) : stack) (tail tokens)
        Just (Reduce n m) ->
          let (pop, stack') = splitAt n stack in
            case
              case stack' of
                [] -> dfaGotoTransition 0 m
                ((q', _) : _) -> dfaGotoTransition q' m of
              Nothing ->
                return Nothing
              Just q -> do
                value <-
                  case m of
                    0 ->
                      Monad.liftM StackValue_value $ value_implies_false actions (case snd (pop !! 0) of { StackValue_false value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_value $ value_implies_null actions (case snd (pop !! 0) of { StackValue_null value -> value; _ -> undefined })
                    2 ->
                      Monad.liftM StackValue_value $ value_implies_true actions (case snd (pop !! 0) of { StackValue_true value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_value $ value_implies_object actions (case snd (pop !! 0) of { StackValue_object value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_value $ value_implies_array actions (case snd (pop !! 0) of { StackValue_array value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_value $ value_implies_number actions (case snd (pop !! 0) of { StackValue_number value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_value $ value_implies_string' actions (case snd (pop !! 0) of { StackValue_string' value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_false $ false_implies_FALSE actions (case snd (pop !! 0) of { StackValue_FALSE value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_null $ null_implies_NULL actions (case snd (pop !! 0) of { StackValue_NULL value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_true $ true_implies_TRUE actions (case snd (pop !! 0) of { StackValue_TRUE value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_object $ object_implies_LBRACE_members_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_members value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_members $ members_implies actions
                    12 ->
                      Monad.liftM StackValue_members $ members_implies_member_members_opt actions (case snd (pop !! 1) of { StackValue_member value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_members_opt value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_members_opt $ members_opt_implies actions
                    14 ->
                      Monad.liftM StackValue_members_opt $ members_opt_implies_COMMA_members actions (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_members value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_member $ member_implies_STRING_COLON_value actions (case snd (pop !! 2) of { StackValue_STRING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_value value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_array $ array_implies_LBRACKET_values_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_values value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_values $ values_implies actions
                    18 ->
                      Monad.liftM StackValue_values $ values_implies_value_values_opt actions (case snd (pop !! 1) of { StackValue_value value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_values_opt value -> value; _ -> undefined })
                    19 ->
                      Monad.liftM StackValue_values_opt $ values_opt_implies actions
                    20 ->
                      Monad.liftM StackValue_values_opt $ values_opt_implies_COMMA_values actions (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_values value -> value; _ -> undefined })
                    21 ->
                      Monad.liftM StackValue_number $ number_implies_NUMBER actions (case snd (pop !! 0) of { StackValue_NUMBER value -> value; _ -> undefined })
                    22 ->
                      Monad.liftM StackValue_string' $ string'_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_value value)] -> return $ Just (value, tokens); _ -> return Nothing }



semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { value_implies_false = return
  , value_implies_null = return
  , value_implies_true = return
  , value_implies_object = return
  , value_implies_array = return
  , value_implies_number = return
  , value_implies_string' = return
  , false_implies_FALSE = const $ return JSONFalse
  , null_implies_NULL = const $ return JSONNull
  , true_implies_TRUE = const $ return JSONTrue
  , object_implies_LBRACE_members_RBRACE = \() members () ->
      return $ JSONObject members
  , members_implies = return RBMap.empty
  , members_implies_member_members_opt = \(prop, value) members_opt ->
      return $ maybe (RBMap.singleton prop value) (RBMap.insert prop value) members_opt
  , members_opt_implies = return Nothing
  , members_opt_implies_COMMA_members = const (return . Just)
  , member_implies_STRING_COLON_value = \prop () value ->
      return (prop, value)
  , array_implies_LBRACKET_values_RBRACKET = \() values () ->
    return $ JSONArray values
  , values_implies_value_values_opt = \value values_opt ->
      return $ maybe [value] (value:) values_opt
  , values_opt_implies = return Nothing
  , values_opt_implies_COMMA_values = const (return . Just)
  , number_implies_NUMBER = return . JSONNumber
  , string'_implies_STRING = return . JSONString }


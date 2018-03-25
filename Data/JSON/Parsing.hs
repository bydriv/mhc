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
  deriving (Eq, Ord, Read, Show)
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
  let s' :: Int
      s' =
        case s of
          EOF -> -1
          Token (COLON _) -> 7
          Token (COMMA _) -> 5
          Token (FALSE _) -> 0
          Token (LBRACE _) -> 3
          Token (LBRACKET _) -> 8
          Token (NULL _) -> 1
          Token (NUMBER _) -> 10
          Token (RBRACE _) -> 4
          Token (RBRACKET _) -> 9
          Token (STRING _) -> 6
          Token (TRUE _) -> 2
  in case compare(q,s')(8,6)of{LT->case compare(q,s')(4,3)of{LT->case compare(q,s')(3,3)of{LT->case compare(q,s')(2,3)of{LT->case compare(q,s')(1,-1)of{LT->case compare(q,s')(0,3)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 13);GT->case compare(q,s')(0,2)of{LT->Nothing;EQ->Just(Shift 14);GT->Nothing}};EQ->Just(Shift 15);GT->case compare(q,s')(0,8)of{LT->case compare(q,s')(0,6)of{LT->Nothing;EQ->Just(Shift 21);GT->Nothing};EQ->Just(Shift 2);GT->case compare(q,s')(0,10)of{LT->Nothing;EQ->Just(Shift 20);GT->Nothing}}};EQ->Just(Accept);GT->case compare(q,s')(2,1)of{LT->case compare(q,s')(2,0)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 13);GT->case compare(q,s')(2,2)of{LT->Nothing;EQ->Just(Shift 14);GT->Nothing}}};EQ->Just(Shift 15);GT->case compare(q,s')(2,10)of{LT->case compare(q,s')(2,8)of{LT->case compare(q,s')(2,6)of{LT->Nothing;EQ->Just(Shift 21);GT->Nothing};EQ->Just(Shift 2);GT->case compare(q,s')(2,9)of{LT->Nothing;EQ->Just(Reduce 0 17);GT->Nothing}};EQ->Just(Shift 20);GT->case compare(q,s')(3,1)of{LT->case compare(q,s')(3,0)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 13);GT->case compare(q,s')(3,2)of{LT->Nothing;EQ->Just(Shift 14);GT->Nothing}}}};EQ->Just(Shift 15);GT->case compare(q,s')(3,10)of{LT->case compare(q,s')(3,8)of{LT->case compare(q,s')(3,6)of{LT->Nothing;EQ->Just(Shift 21);GT->Nothing};EQ->Just(Shift 2);GT->case compare(q,s')(3,9)of{LT->Nothing;EQ->Just(Reduce 0 17);GT->Nothing}};EQ->Just(Shift 20);GT->case compare(q,s')(4,1)of{LT->case compare(q,s')(4,0)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing};EQ->Just(Shift 13);GT->case compare(q,s')(4,2)of{LT->Nothing;EQ->Just(Shift 14);GT->Nothing}}}};EQ->Just(Shift 15);GT->case compare(q,s')(5,8)of{LT->case compare(q,s')(5,-1)of{LT->case compare(q,s')(4,8)of{LT->case compare(q,s')(4,6)of{LT->Nothing;EQ->Just(Shift 21);GT->Nothing};EQ->Just(Shift 2);GT->case compare(q,s')(4,10)of{LT->Nothing;EQ->Just(Shift 20);GT->Nothing}};EQ->Just(Reduce 1 0);GT->case compare(q,s')(5,2)of{LT->case compare(q,s')(5,1)of{LT->case compare(q,s')(5,0)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->case compare(q,s')(5,5)of{LT->case compare(q,s')(5,4)of{LT->case compare(q,s')(5,3)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->case compare(q,s')(5,7)of{LT->case compare(q,s')(5,6)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->Nothing}}}};EQ->Just(Reduce 1 0);GT->case compare(q,s')(6,7)of{LT->case compare(q,s')(6,2)of{LT->case compare(q,s')(6,-1)of{LT->case compare(q,s')(5,10)of{LT->case compare(q,s')(5,9)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 1);GT->case compare(q,s')(6,1)of{LT->case compare(q,s')(6,0)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->Nothing}};EQ->Just(Reduce 1 1);GT->case compare(q,s')(6,5)of{LT->case compare(q,s')(6,4)of{LT->case compare(q,s')(6,3)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->case compare(q,s')(6,6)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing}}};EQ->Just(Reduce 1 1);GT->case compare(q,s')(7,7)of{LT->case compare(q,s')(7,1)of{LT->case compare(q,s')(6,10)of{LT->case compare(q,s')(6,9)of{LT->case compare(q,s')(6,8)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->case compare(q,s')(7,0)of{LT->case compare(q,s')(7,-1)of{LT->Nothing;EQ->Just(Reduce 1 2);GT->Nothing};EQ->Just(Reduce 1 2);GT->Nothing}};EQ->Just(Reduce 1 2);GT->case compare(q,s')(7,4)of{LT->case compare(q,s')(7,3)of{LT->case compare(q,s')(7,2)of{LT->Nothing;EQ->Just(Reduce 1 2);GT->Nothing};EQ->Just(Reduce 1 2);GT->Nothing};EQ->Just(Reduce 1 2);GT->case compare(q,s')(7,6)of{LT->case compare(q,s')(7,5)of{LT->Nothing;EQ->Just(Reduce 1 2);GT->Nothing};EQ->Just(Reduce 1 2);GT->Nothing}}};EQ->Just(Reduce 1 2);GT->case compare(q,s')(8,1)of{LT->case compare(q,s')(7,10)of{LT->case compare(q,s')(7,9)of{LT->case compare(q,s')(7,8)of{LT->Nothing;EQ->Just(Reduce 1 2);GT->Nothing};EQ->Just(Reduce 1 2);GT->Nothing};EQ->Just(Reduce 1 2);GT->case compare(q,s')(8,0)of{LT->case compare(q,s')(8,-1)of{LT->Nothing;EQ->Just(Reduce 1 3);GT->Nothing};EQ->Just(Reduce 1 3);GT->Nothing}};EQ->Just(Reduce 1 3);GT->case compare(q,s')(8,4)of{LT->case compare(q,s')(8,3)of{LT->case compare(q,s')(8,2)of{LT->Nothing;EQ->Just(Reduce 1 3);GT->Nothing};EQ->Just(Reduce 1 3);GT->Nothing};EQ->Just(Reduce 1 3);GT->case compare(q,s')(8,5)of{LT->Nothing;EQ->Just(Reduce 1 3);GT->Nothing}}}}}}};EQ->Just(Reduce 1 3);GT->case compare(q,s')(18,1)of{LT->case compare(q,s')(12,4)of{LT->case compare(q,s')(10,5)of{LT->case compare(q,s')(9,6)of{LT->case compare(q,s')(9,0)of{LT->case compare(q,s')(8,9)of{LT->case compare(q,s')(8,8)of{LT->case compare(q,s')(8,7)of{LT->Nothing;EQ->Just(Reduce 1 3);GT->Nothing};EQ->Just(Reduce 1 3);GT->Nothing};EQ->Just(Reduce 1 3);GT->case compare(q,s')(9,-1)of{LT->case compare(q,s')(8,10)of{LT->Nothing;EQ->Just(Reduce 1 3);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing}};EQ->Just(Reduce 1 4);GT->case compare(q,s')(9,3)of{LT->case compare(q,s')(9,2)of{LT->case compare(q,s')(9,1)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->case compare(q,s')(9,5)of{LT->case compare(q,s')(9,4)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing}}};EQ->Just(Reduce 1 4);GT->case compare(q,s')(10,0)of{LT->case compare(q,s')(9,9)of{LT->case compare(q,s')(9,8)of{LT->case compare(q,s')(9,7)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->case compare(q,s')(10,-1)of{LT->case compare(q,s')(9,10)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing}};EQ->Just(Reduce 1 5);GT->case compare(q,s')(10,3)of{LT->case compare(q,s')(10,2)of{LT->case compare(q,s')(10,1)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(10,4)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing}}}};EQ->Just(Reduce 1 5);GT->case compare(q,s')(11,5)of{LT->case compare(q,s')(11,-1)of{LT->case compare(q,s')(10,8)of{LT->case compare(q,s')(10,7)of{LT->case compare(q,s')(10,6)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(10,10)of{LT->case compare(q,s')(10,9)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(11,2)of{LT->case compare(q,s')(11,1)of{LT->case compare(q,s')(11,0)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->case compare(q,s')(11,4)of{LT->case compare(q,s')(11,3)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing}}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(12,-1)of{LT->case compare(q,s')(11,8)of{LT->case compare(q,s')(11,7)of{LT->case compare(q,s')(11,6)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->case compare(q,s')(11,10)of{LT->case compare(q,s')(11,9)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing}};EQ->Just(Reduce 1 7);GT->case compare(q,s')(12,2)of{LT->case compare(q,s')(12,1)of{LT->case compare(q,s')(12,0)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->case compare(q,s')(12,3)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing}}}}};EQ->Just(Reduce 1 7);GT->case compare(q,s')(14,3)of{LT->case compare(q,s')(13,4)of{LT->case compare(q,s')(12,10)of{LT->case compare(q,s')(12,7)of{LT->case compare(q,s')(12,6)of{LT->case compare(q,s')(12,5)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->case compare(q,s')(12,9)of{LT->case compare(q,s')(12,8)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing}};EQ->Just(Reduce 1 7);GT->case compare(q,s')(13,1)of{LT->case compare(q,s')(13,0)of{LT->case compare(q,s')(13,-1)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->case compare(q,s')(13,3)of{LT->case compare(q,s')(13,2)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing}}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(13,10)of{LT->case compare(q,s')(13,7)of{LT->case compare(q,s')(13,6)of{LT->case compare(q,s')(13,5)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->case compare(q,s')(13,9)of{LT->case compare(q,s')(13,8)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(14,1)of{LT->case compare(q,s')(14,0)of{LT->case compare(q,s')(14,-1)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(14,2)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing}}}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(16,2)of{LT->case compare(q,s')(14,9)of{LT->case compare(q,s')(14,6)of{LT->case compare(q,s')(14,5)of{LT->case compare(q,s')(14,4)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(14,8)of{LT->case compare(q,s')(14,7)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(16,-1)of{LT->case compare(q,s')(15,4)of{LT->case compare(q,s')(14,10)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 0 11);GT->case compare(q,s')(15,6)of{LT->Nothing;EQ->Just(Shift 25);GT->Nothing}};EQ->Just(Reduce 3 10);GT->case compare(q,s')(16,1)of{LT->case compare(q,s')(16,0)of{LT->Nothing;EQ->Just(Reduce 3 10);GT->Nothing};EQ->Just(Reduce 3 10);GT->Nothing}}};EQ->Just(Reduce 3 10);GT->case compare(q,s')(16,8)of{LT->case compare(q,s')(16,5)of{LT->case compare(q,s')(16,4)of{LT->case compare(q,s')(16,3)of{LT->Nothing;EQ->Just(Reduce 3 10);GT->Nothing};EQ->Just(Reduce 3 10);GT->Nothing};EQ->Just(Reduce 3 10);GT->case compare(q,s')(16,7)of{LT->case compare(q,s')(16,6)of{LT->Nothing;EQ->Just(Reduce 3 10);GT->Nothing};EQ->Just(Reduce 3 10);GT->Nothing}};EQ->Just(Reduce 3 10);GT->case compare(q,s')(18,-1)of{LT->case compare(q,s')(16,10)of{LT->case compare(q,s')(16,9)of{LT->Nothing;EQ->Just(Reduce 3 10);GT->Nothing};EQ->Just(Reduce 3 10);GT->case compare(q,s')(17,4)of{LT->Nothing;EQ->Just(Shift 16);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(18,0)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing}}}}}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(22,4)of{LT->case compare(q,s')(20,1)of{LT->case compare(q,s')(18,10)of{LT->case compare(q,s')(18,7)of{LT->case compare(q,s')(18,4)of{LT->case compare(q,s')(18,3)of{LT->case compare(q,s')(18,2)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->case compare(q,s')(18,6)of{LT->case compare(q,s')(18,5)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(18,9)of{LT->case compare(q,s')(18,8)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(20,-1)of{LT->case compare(q,s')(19,9)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing};EQ->Just(Reduce 1 21);GT->case compare(q,s')(20,0)of{LT->Nothing;EQ->Just(Reduce 1 21);GT->Nothing}}};EQ->Just(Reduce 1 21);GT->case compare(q,s')(21,0)of{LT->case compare(q,s')(20,7)of{LT->case compare(q,s')(20,4)of{LT->case compare(q,s')(20,3)of{LT->case compare(q,s')(20,2)of{LT->Nothing;EQ->Just(Reduce 1 21);GT->Nothing};EQ->Just(Reduce 1 21);GT->Nothing};EQ->Just(Reduce 1 21);GT->case compare(q,s')(20,6)of{LT->case compare(q,s')(20,5)of{LT->Nothing;EQ->Just(Reduce 1 21);GT->Nothing};EQ->Just(Reduce 1 21);GT->Nothing}};EQ->Just(Reduce 1 21);GT->case compare(q,s')(20,10)of{LT->case compare(q,s')(20,9)of{LT->case compare(q,s')(20,8)of{LT->Nothing;EQ->Just(Reduce 1 21);GT->Nothing};EQ->Just(Reduce 1 21);GT->Nothing};EQ->Just(Reduce 1 21);GT->case compare(q,s')(21,-1)of{LT->Nothing;EQ->Just(Reduce 1 22);GT->Nothing}}};EQ->Just(Reduce 1 22);GT->case compare(q,s')(21,6)of{LT->case compare(q,s')(21,3)of{LT->case compare(q,s')(21,2)of{LT->case compare(q,s')(21,1)of{LT->Nothing;EQ->Just(Reduce 1 22);GT->Nothing};EQ->Just(Reduce 1 22);GT->Nothing};EQ->Just(Reduce 1 22);GT->case compare(q,s')(21,5)of{LT->case compare(q,s')(21,4)of{LT->Nothing;EQ->Just(Reduce 1 22);GT->Nothing};EQ->Just(Reduce 1 22);GT->Nothing}};EQ->Just(Reduce 1 22);GT->case compare(q,s')(21,9)of{LT->case compare(q,s')(21,8)of{LT->case compare(q,s')(21,7)of{LT->Nothing;EQ->Just(Reduce 1 22);GT->Nothing};EQ->Just(Reduce 1 22);GT->Nothing};EQ->Just(Reduce 1 22);GT->case compare(q,s')(21,10)of{LT->Nothing;EQ->Just(Reduce 1 22);GT->Nothing}}}}};EQ->Just(Reduce 0 11);GT->case compare(q,s')(28,5)of{LT->case compare(q,s')(26,4)of{LT->case compare(q,s')(23,5)of{LT->case compare(q,s')(23,4)of{LT->case compare(q,s')(22,6)of{LT->Nothing;EQ->Just(Shift 25);GT->Nothing};EQ->Just(Reduce 0 13);GT->Nothing};EQ->Just(Shift 22);GT->case compare(q,s')(24,4)of{LT->Nothing;EQ->Just(Reduce 2 12);GT->case compare(q,s')(25,7)of{LT->Nothing;EQ->Just(Shift 4);GT->Nothing}}};EQ->Just(Reduce 3 15);GT->case compare(q,s')(26,7)of{LT->case compare(q,s')(26,6)of{LT->case compare(q,s')(26,5)of{LT->Nothing;EQ->Just(Reduce 3 15);GT->Nothing};EQ->Just(Reduce 3 15);GT->Nothing};EQ->Just(Reduce 3 15);GT->case compare(q,s')(27,4)of{LT->Nothing;EQ->Just(Reduce 2 14);GT->Nothing}}};EQ->Just(Shift 3);GT->case compare(q,s')(29,9)of{LT->case compare(q,s')(28,9)of{LT->Nothing;EQ->Just(Reduce 0 19);GT->Nothing};EQ->Just(Reduce 2 18);GT->case compare(q,s')(30,9)of{LT->Nothing;EQ->Just(Reduce 2 20);GT->Nothing}}}}}}

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
production _ = undefined

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  let s' = production s in
    case compare(q,s')(3,3)of{LT->case compare(q,s')(2,2)of{LT->case compare(q,s')(0,5)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just 1;GT->Nothing};EQ->Just 5;GT->Nothing};EQ->Just 6;GT->case compare(q,s')(0,4)of{LT->case compare(q,s')(0,3)of{LT->Nothing;EQ->Just 7;GT->Nothing};EQ->Just 8;GT->Nothing}};EQ->Just 9;GT->case compare(q,s')(2,0)of{LT->case compare(q,s')(0,7)of{LT->case compare(q,s')(0,6)of{LT->Nothing;EQ->Just 10;GT->Nothing};EQ->Just 11;GT->Nothing};EQ->Just 28;GT->case compare(q,s')(2,1)of{LT->Nothing;EQ->Just 5;GT->Nothing}}};EQ->Just 6;GT->case compare(q,s')(2,7)of{LT->case compare(q,s')(2,5)of{LT->case compare(q,s')(2,4)of{LT->case compare(q,s')(2,3)of{LT->Nothing;EQ->Just 7;GT->Nothing};EQ->Just 8;GT->Nothing};EQ->Just 9;GT->case compare(q,s')(2,6)of{LT->Nothing;EQ->Just 10;GT->Nothing}};EQ->Just 11;GT->case compare(q,s')(3,1)of{LT->case compare(q,s')(3,0)of{LT->case compare(q,s')(2,11)of{LT->Nothing;EQ->Just 19;GT->Nothing};EQ->Just 28;GT->Nothing};EQ->Just 5;GT->case compare(q,s')(3,2)of{LT->Nothing;EQ->Just 6;GT->Nothing}}}};EQ->Just 7;GT->case compare(q,s')(4,4)of{LT->case compare(q,s')(3,11)of{LT->case compare(q,s')(3,6)of{LT->case compare(q,s')(3,5)of{LT->case compare(q,s')(3,4)of{LT->Nothing;EQ->Just 8;GT->Nothing};EQ->Just 9;GT->Nothing};EQ->Just 10;GT->case compare(q,s')(3,7)of{LT->Nothing;EQ->Just 11;GT->Nothing}};EQ->Just 30;GT->case compare(q,s')(4,2)of{LT->case compare(q,s')(4,1)of{LT->case compare(q,s')(4,0)of{LT->Nothing;EQ->Just 26;GT->Nothing};EQ->Just 5;GT->Nothing};EQ->Just 6;GT->case compare(q,s')(4,3)of{LT->Nothing;EQ->Just 7;GT->Nothing}}};EQ->Just 8;GT->case compare(q,s')(15,9)of{LT->case compare(q,s')(4,7)of{LT->case compare(q,s')(4,6)of{LT->case compare(q,s')(4,5)of{LT->Nothing;EQ->Just 9;GT->Nothing};EQ->Just 10;GT->Nothing};EQ->Just 11;GT->case compare(q,s')(15,8)of{LT->Nothing;EQ->Just 17;GT->Nothing}};EQ->Just 23;GT->case compare(q,s')(23,10)of{LT->case compare(q,s')(22,9)of{LT->case compare(q,s')(22,8)of{LT->Nothing;EQ->Just 27;GT->Nothing};EQ->Just 23;GT->Nothing};EQ->Just 24;GT->case compare(q,s')(28,12)of{LT->Nothing;EQ->Just 29;GT->Nothing}}}}}

parse :: Monad m => SemanticActions m -> [Token] -> m (Either (Maybe Token) (Value, [Token]))
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
                case tokens of
                  [] -> return $ Left $ Nothing
                  (token : _) -> return $ Left $ Just token
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
                    _ -> undefined
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_value value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



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
  , values_implies = return []
  , values_implies_value_values_opt = \value values_opt ->
      return $ maybe [value] (value:) values_opt
  , values_opt_implies = return Nothing
  , values_opt_implies_COMMA_values = const (return . Just)
  , number_implies_NUMBER = return . JSONNumber
  , string'_implies_STRING = return . JSONString }


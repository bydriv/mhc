module  Data.TOML.Parsing  where
import qualified Control.Monad as Monad


import Prelude hiding
  (lookup)
import qualified Data.Int   as Int
import qualified Data.Maybe as Maybe
import qualified Data.RBMap as RBMap

type Pos = (Int, Int)
type BASIC_STRING = (Pos, String)
type BOOLEAN = (Pos, Bool)
type COMMA = Pos
type DATE_TIME = (Pos, String)
type DOT = Pos
type EQUAL = Pos
type FLOAT = (Pos, Double)
type INTEGER = (Pos, Int.Int64)
type LBRACKET = Pos
type LBRACE = Pos
type LITERAL_STRING = (Pos, String)
type ML_BASIC_STRING = (Pos, String)
type ML_LITERAL_STRING = (Pos, String)
type NEWLINE = Pos
type RBRACKET = Pos
type RBRACE = Pos
type UNQUOTED_KEY = (Pos, String)

type TOML = RBMap.RBMap SimpleKey Val

data Val =
    TOMLString String
  | TOMLBoolean Bool
  | TOMLArray [Val]
  | TOMLTable (RBMap.RBMap SimpleKey Val)
  | TOMLDateTime String
  | TOMLFloat Double
  | TOMLInteger Int.Int64
  deriving (Eq, Ord, Read, Show)

type Toml = TOML
type Expressions = [Expression]
type Expression = Maybe (Either Keyval Table)
type Keyval = (Key, Val)
type Key = Either SimpleKey DottedKey
type SimpleKey = String
type QuotedKey = String
type DottedKey = [SimpleKey]
type String' = String
type Array = [Val]
type ArrayValues = [Val]
type Table = Either StdTable ArrayTable
type StdTable = Key
type ArrayTable = Key
type InlineTable = RBMap.RBMap SimpleKey Val
type InlineTableKeyVals = [Keyval]

data Token =
    BASIC_STRING BASIC_STRING
  | BOOLEAN BOOLEAN
  | COMMA COMMA
  | DATE_TIME DATE_TIME
  | DOT DOT
  | EQUAL EQUAL
  | FLOAT FLOAT
  | INTEGER INTEGER
  | LBRACE LBRACE
  | LBRACKET LBRACKET
  | LITERAL_STRING LITERAL_STRING
  | ML_BASIC_STRING ML_BASIC_STRING
  | ML_LITERAL_STRING ML_LITERAL_STRING
  | NEWLINE NEWLINE
  | RBRACE RBRACE
  | RBRACKET RBRACKET
  | UNQUOTED_KEY UNQUOTED_KEY
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
  deriving (Eq, Ord, Read, Show)
type GotoState = Int
type GotoSymbol = Int

data StackValue =
    StackValue_EOF
  | StackValue_NEWLINE NEWLINE
  | StackValue_EQUAL EQUAL
  | StackValue_UNQUOTED_KEY UNQUOTED_KEY
  | StackValue_BASIC_STRING BASIC_STRING
  | StackValue_LITERAL_STRING LITERAL_STRING
  | StackValue_DOT DOT
  | StackValue_BOOLEAN BOOLEAN
  | StackValue_DATE_TIME DATE_TIME
  | StackValue_FLOAT FLOAT
  | StackValue_INTEGER INTEGER
  | StackValue_ML_BASIC_STRING ML_BASIC_STRING
  | StackValue_ML_LITERAL_STRING ML_LITERAL_STRING
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_COMMA COMMA
  | StackValue_LBRACE LBRACE
  | StackValue_RBRACE RBRACE
  | StackValue_toml Toml
  | StackValue_expressions Expressions
  | StackValue_expression Expression
  | StackValue_keyval Keyval
  | StackValue_table Table
  | StackValue_key Key
  | StackValue_val Val
  | StackValue_simpleKey SimpleKey
  | StackValue_dottedKey DottedKey
  | StackValue_quotedKey QuotedKey
  | StackValue_string' String'
  | StackValue_array Array
  | StackValue_inlineTable InlineTable
  | StackValue_arrayValues ArrayValues
  | StackValue_stdTable StdTable
  | StackValue_arrayTable ArrayTable
  | StackValue_inlineTableKeyVals InlineTableKeyVals

data SemanticActions m = SemanticActions
  { toml_implies_expressions :: Expressions -> m Toml
  , expressions_implies_expression :: Expression -> m Expressions
  , expressions_implies_expression_NEWLINE_expressions :: Expression -> NEWLINE -> Expressions -> m Expressions
  , expression_implies :: m Expression
  , expression_implies_keyval :: Keyval -> m Expression
  , expression_implies_table :: Table -> m Expression
  , keyval_implies_key_EQUAL_val :: Key -> EQUAL -> Val -> m Keyval
  , key_implies_simpleKey :: SimpleKey -> m Key
  , key_implies_dottedKey :: DottedKey -> m Key
  , simpleKey_implies_quotedKey :: QuotedKey -> m SimpleKey
  , simpleKey_implies_UNQUOTED_KEY :: UNQUOTED_KEY -> m SimpleKey
  , quotedKey_implies_BASIC_STRING :: BASIC_STRING -> m QuotedKey
  , quotedKey_implies_LITERAL_STRING :: LITERAL_STRING -> m QuotedKey
  , dottedKey_implies_simpleKey_DOT_simpleKey :: SimpleKey -> DOT -> SimpleKey -> m DottedKey
  , dottedKey_implies_simpleKey_DOT_dottedKey :: SimpleKey -> DOT -> DottedKey -> m DottedKey
  , val_implies_string' :: String' -> m Val
  , val_implies_BOOLEAN :: BOOLEAN -> m Val
  , val_implies_array :: Array -> m Val
  , val_implies_inlineTable :: InlineTable -> m Val
  , val_implies_DATE_TIME :: DATE_TIME -> m Val
  , val_implies_FLOAT :: FLOAT -> m Val
  , val_implies_INTEGER :: INTEGER -> m Val
  , string'_implies_ML_BASIC_STRING :: ML_BASIC_STRING -> m String'
  , string'_implies_BASIC_STRING :: BASIC_STRING -> m String'
  , string'_implies_ML_LITERAL_STRING :: ML_LITERAL_STRING -> m String'
  , string'_implies_LITERAL_STRING :: LITERAL_STRING -> m String'
  , array_implies_LBRACKET_RBRACKET :: LBRACKET -> RBRACKET -> m Array
  , array_implies_LBRACKET_arrayValues_RBRACKET :: LBRACKET -> ArrayValues -> RBRACKET -> m Array
  , arrayValues_implies_val :: Val -> m ArrayValues
  , arrayValues_implies_val_COMMA :: Val -> COMMA -> m ArrayValues
  , arrayValues_implies_val_COMMA_arrayValues :: Val -> COMMA -> ArrayValues -> m ArrayValues
  , table_implies_stdTable :: StdTable -> m Table
  , table_implies_arrayTable :: ArrayTable -> m Table
  , stdTable_implies_LBRACKET_key_RBRACKET :: LBRACKET -> Key -> RBRACKET -> m StdTable
  , inlineTable_implies_LBRACE_RBRACE :: LBRACE -> RBRACE -> m InlineTable
  , inlineTable_implies_LBRACE_inlineTableKeyVals_RBRACE :: LBRACE -> InlineTableKeyVals -> RBRACE -> m InlineTable
  , inlineTableKeyVals_implies_keyval :: Keyval -> m InlineTableKeyVals
  , inlineTableKeyVals_implies_keyval_COMMA_inlineTableKeyVals :: Keyval -> COMMA -> InlineTableKeyVals -> m InlineTableKeyVals
  , arrayTable_implies_LBRACKET_LBRACKET_key_RBRACKET_RBRACKET :: LBRACKET -> LBRACKET -> Key -> RBRACKET -> RBRACKET -> m ArrayTable }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  let s' :: Int
      s' =
        case s of
          EOF -> -1
          Token (BASIC_STRING _) -> 3
          Token (BOOLEAN _) -> 6
          Token (COMMA _) -> 14
          Token (DATE_TIME _) -> 7
          Token (DOT _) -> 5
          Token (EQUAL _) -> 1
          Token (FLOAT _) -> 8
          Token (INTEGER _) -> 9
          Token (LBRACE _) -> 15
          Token (LBRACKET _) -> 12
          Token (LITERAL_STRING _) -> 4
          Token (ML_BASIC_STRING _) -> 10
          Token (ML_LITERAL_STRING _) -> 11
          Token (NEWLINE _) -> 0
          Token (RBRACE _) -> 16
          Token (RBRACKET _) -> 13
          Token (UNQUOTED_KEY _) -> 2
  in case compare(q,s')(38,13)of{LT->case compare(q,s')(27,14)of{LT->case compare(q,s')(20,8)of{LT->case compare(q,s')(10,3)of{LT->case compare(q,s')(3,2)of{LT->case compare(q,s')(0,12)of{LT->case compare(q,s')(0,3)of{LT->case compare(q,s')(0,0)of{LT->case compare(q,s')(0,-1)of{LT->Nothing;EQ->Just(Reduce 0 3);GT->Nothing};EQ->Just(Reduce 0 3);GT->case compare(q,s')(0,2)of{LT->Nothing;EQ->Just(Shift 29);GT->Nothing}};EQ->Just(Shift 33);GT->case compare(q,s')(0,4)of{LT->Nothing;EQ->Just(Shift 34);GT->Nothing}};EQ->Just(Shift 15);GT->case compare(q,s')(3,-1)of{LT->case compare(q,s')(1,-1)of{LT->Nothing;EQ->Just(Accept);GT->case compare(q,s')(2,-1)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing}};EQ->Just(Reduce 0 3);GT->case compare(q,s')(3,0)of{LT->Nothing;EQ->Just(Reduce 0 3);GT->Nothing}}};EQ->Just(Shift 29);GT->case compare(q,s')(8,2)of{LT->case compare(q,s')(5,0)of{LT->case compare(q,s')(3,12)of{LT->case compare(q,s')(3,4)of{LT->case compare(q,s')(3,3)of{LT->Nothing;EQ->Just(Shift 33);GT->Nothing};EQ->Just(Shift 34);GT->Nothing};EQ->Just(Shift 15);GT->case compare(q,s')(5,-1)of{LT->case compare(q,s')(4,-1)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing};EQ->Just(Reduce 1 1);GT->Nothing}};EQ->Just(Shift 3);GT->case compare(q,s')(7,-1)of{LT->case compare(q,s')(6,0)of{LT->case compare(q,s')(6,-1)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(7,0)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing}}};EQ->Just(Shift 29);GT->case compare(q,s')(8,16)of{LT->case compare(q,s')(8,3)of{LT->Nothing;EQ->Just(Shift 33);GT->case compare(q,s')(8,4)of{LT->Nothing;EQ->Just(Shift 34);GT->Nothing}};EQ->Just(Shift 42);GT->case compare(q,s')(9,3)of{LT->case compare(q,s')(9,2)of{LT->Nothing;EQ->Just(Shift 29);GT->Nothing};EQ->Just(Shift 33);GT->case compare(q,s')(9,4)of{LT->Nothing;EQ->Just(Shift 34);GT->Nothing}}}}};EQ->Just(Shift 35);GT->case compare(q,s')(14,0)of{LT->case compare(q,s')(12,16)of{LT->case compare(q,s')(10,15)of{LT->case compare(q,s')(10,9)of{LT->case compare(q,s')(10,7)of{LT->case compare(q,s')(10,6)of{LT->case compare(q,s')(10,4)of{LT->Nothing;EQ->Just(Shift 36);GT->Nothing};EQ->Just(Shift 21);GT->Nothing};EQ->Just(Shift 22);GT->case compare(q,s')(10,8)of{LT->Nothing;EQ->Just(Shift 23);GT->Nothing}};EQ->Just(Shift 24);GT->case compare(q,s')(10,11)of{LT->case compare(q,s')(10,10)of{LT->Nothing;EQ->Just(Shift 37);GT->Nothing};EQ->Just(Shift 38);GT->case compare(q,s')(10,12)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing}}};EQ->Just(Shift 8);GT->case compare(q,s')(12,0)of{LT->case compare(q,s')(11,1)of{LT->Nothing;EQ->Just(Shift 10);GT->case compare(q,s')(12,-1)of{LT->Nothing;EQ->Just(Reduce 3 6);GT->Nothing}};EQ->Just(Reduce 3 6);GT->case compare(q,s')(12,14)of{LT->Nothing;EQ->Just(Reduce 3 6);GT->Nothing}}};EQ->Just(Reduce 3 6);GT->case compare(q,s')(13,0)of{LT->case compare(q,s')(13,-1)of{LT->Nothing;EQ->Just(Reduce 1 31);GT->Nothing};EQ->Just(Reduce 1 31);GT->case compare(q,s')(14,-1)of{LT->Nothing;EQ->Just(Reduce 1 32);GT->Nothing}}};EQ->Just(Reduce 1 32);GT->case compare(q,s')(16,4)of{LT->case compare(q,s')(15,4)of{LT->case compare(q,s')(15,2)of{LT->Nothing;EQ->Just(Shift 29);GT->case compare(q,s')(15,3)of{LT->Nothing;EQ->Just(Shift 33);GT->Nothing}};EQ->Just(Shift 34);GT->case compare(q,s')(16,2)of{LT->case compare(q,s')(15,12)of{LT->Nothing;EQ->Just(Shift 16);GT->Nothing};EQ->Just(Shift 29);GT->case compare(q,s')(16,3)of{LT->Nothing;EQ->Just(Shift 33);GT->Nothing}}};EQ->Just(Shift 34);GT->case compare(q,s')(19,7)of{LT->case compare(q,s')(19,3)of{LT->case compare(q,s')(18,1)of{LT->case compare(q,s')(17,5)of{LT->case compare(q,s')(17,1)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Shift 28);GT->case compare(q,s')(17,13)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(18,13)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing}};EQ->Just(Shift 35);GT->case compare(q,s')(19,4)of{LT->Nothing;EQ->Just(Shift 36);GT->case compare(q,s')(19,6)of{LT->Nothing;EQ->Just(Shift 21);GT->Nothing}}};EQ->Just(Shift 22);GT->case compare(q,s')(19,11)of{LT->case compare(q,s')(19,9)of{LT->case compare(q,s')(19,8)of{LT->Nothing;EQ->Just(Shift 23);GT->Nothing};EQ->Just(Shift 24);GT->case compare(q,s')(19,10)of{LT->Nothing;EQ->Just(Shift 37);GT->Nothing}};EQ->Just(Shift 38);GT->case compare(q,s')(20,3)of{LT->case compare(q,s')(19,13)of{LT->case compare(q,s')(19,12)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing};EQ->Just(Shift 39);GT->case compare(q,s')(19,15)of{LT->Nothing;EQ->Just(Shift 8);GT->Nothing}};EQ->Just(Shift 35);GT->case compare(q,s')(20,6)of{LT->case compare(q,s')(20,4)of{LT->Nothing;EQ->Just(Shift 36);GT->Nothing};EQ->Just(Shift 21);GT->case compare(q,s')(20,7)of{LT->Nothing;EQ->Just(Shift 22);GT->Nothing}}}}}}}};EQ->Just(Shift 23);GT->case compare(q,s')(24,13)of{LT->case compare(q,s')(22,16)of{LT->case compare(q,s')(22,-1)of{LT->case compare(q,s')(21,0)of{LT->case compare(q,s')(20,15)of{LT->case compare(q,s')(20,12)of{LT->case compare(q,s')(20,11)of{LT->case compare(q,s')(20,10)of{LT->case compare(q,s')(20,9)of{LT->Nothing;EQ->Just(Shift 24);GT->Nothing};EQ->Just(Shift 37);GT->Nothing};EQ->Just(Shift 38);GT->Nothing};EQ->Just(Shift 19);GT->case compare(q,s')(20,13)of{LT->Nothing;EQ->Just(Reduce 2 29);GT->Nothing}};EQ->Just(Shift 8);GT->case compare(q,s')(21,-1)of{LT->Nothing;EQ->Just(Reduce 1 16);GT->Nothing}};EQ->Just(Reduce 1 16);GT->case compare(q,s')(21,14)of{LT->case compare(q,s')(21,13)of{LT->Nothing;EQ->Just(Reduce 1 16);GT->Nothing};EQ->Just(Reduce 1 16);GT->case compare(q,s')(21,16)of{LT->Nothing;EQ->Just(Reduce 1 16);GT->Nothing}}};EQ->Just(Reduce 1 19);GT->case compare(q,s')(22,13)of{LT->case compare(q,s')(22,0)of{LT->Nothing;EQ->Just(Reduce 1 19);GT->Nothing};EQ->Just(Reduce 1 19);GT->case compare(q,s')(22,14)of{LT->Nothing;EQ->Just(Reduce 1 19);GT->Nothing}}};EQ->Just(Reduce 1 19);GT->case compare(q,s')(23,14)of{LT->case compare(q,s')(23,0)of{LT->case compare(q,s')(23,-1)of{LT->Nothing;EQ->Just(Reduce 1 20);GT->Nothing};EQ->Just(Reduce 1 20);GT->case compare(q,s')(23,13)of{LT->Nothing;EQ->Just(Reduce 1 20);GT->Nothing}};EQ->Just(Reduce 1 20);GT->case compare(q,s')(24,-1)of{LT->case compare(q,s')(23,16)of{LT->Nothing;EQ->Just(Reduce 1 20);GT->Nothing};EQ->Just(Reduce 1 21);GT->case compare(q,s')(24,0)of{LT->Nothing;EQ->Just(Reduce 1 21);GT->Nothing}}}};EQ->Just(Reduce 1 21);GT->case compare(q,s')(26,-1)of{LT->case compare(q,s')(25,0)of{LT->case compare(q,s')(24,16)of{LT->case compare(q,s')(24,14)of{LT->Nothing;EQ->Just(Reduce 1 21);GT->Nothing};EQ->Just(Reduce 1 21);GT->case compare(q,s')(25,-1)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing}};EQ->Just(Reduce 1 15);GT->case compare(q,s')(25,14)of{LT->case compare(q,s')(25,13)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->case compare(q,s')(25,16)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing}}};EQ->Just(Reduce 1 17);GT->case compare(q,s')(26,16)of{LT->case compare(q,s')(26,13)of{LT->case compare(q,s')(26,0)of{LT->Nothing;EQ->Just(Reduce 1 17);GT->Nothing};EQ->Just(Reduce 1 17);GT->case compare(q,s')(26,14)of{LT->Nothing;EQ->Just(Reduce 1 17);GT->Nothing}};EQ->Just(Reduce 1 17);GT->case compare(q,s')(27,0)of{LT->case compare(q,s')(27,-1)of{LT->Nothing;EQ->Just(Reduce 1 18);GT->Nothing};EQ->Just(Reduce 1 18);GT->case compare(q,s')(27,13)of{LT->Nothing;EQ->Just(Reduce 1 18);GT->Nothing}}}}}};EQ->Just(Reduce 1 18);GT->case compare(q,s')(35,0)of{LT->case compare(q,s')(32,13)of{LT->case compare(q,s')(28,4)of{LT->case compare(q,s')(28,2)of{LT->case compare(q,s')(27,16)of{LT->Nothing;EQ->Just(Reduce 1 18);GT->Nothing};EQ->Just(Shift 29);GT->case compare(q,s')(28,3)of{LT->Nothing;EQ->Just(Shift 33);GT->Nothing}};EQ->Just(Shift 34);GT->case compare(q,s')(30,13)of{LT->case compare(q,s')(30,1)of{LT->case compare(q,s')(29,5)of{LT->case compare(q,s')(29,1)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->case compare(q,s')(29,13)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(30,5)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(31,5)of{LT->case compare(q,s')(31,1)of{LT->Nothing;EQ->Just(Reduce 3 13);GT->Nothing};EQ->Just(Shift 28);GT->case compare(q,s')(32,1)of{LT->case compare(q,s')(31,13)of{LT->Nothing;EQ->Just(Reduce 3 13);GT->Nothing};EQ->Just(Reduce 3 14);GT->Nothing}}}};EQ->Just(Reduce 3 14);GT->case compare(q,s')(34,1)of{LT->case compare(q,s')(33,5)of{LT->case compare(q,s')(33,1)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(33,13)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(34,13)of{LT->case compare(q,s')(34,5)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->case compare(q,s')(35,-1)of{LT->Nothing;EQ->Just(Reduce 1 23);GT->Nothing}}}};EQ->Just(Reduce 1 23);GT->case compare(q,s')(36,16)of{LT->case compare(q,s')(36,-1)of{LT->case compare(q,s')(35,14)of{LT->case compare(q,s')(35,13)of{LT->Nothing;EQ->Just(Reduce 1 23);GT->Nothing};EQ->Just(Reduce 1 23);GT->case compare(q,s')(35,16)of{LT->Nothing;EQ->Just(Reduce 1 23);GT->Nothing}};EQ->Just(Reduce 1 25);GT->case compare(q,s')(36,13)of{LT->case compare(q,s')(36,0)of{LT->Nothing;EQ->Just(Reduce 1 25);GT->Nothing};EQ->Just(Reduce 1 25);GT->case compare(q,s')(36,14)of{LT->Nothing;EQ->Just(Reduce 1 25);GT->Nothing}}};EQ->Just(Reduce 1 25);GT->case compare(q,s')(37,14)of{LT->case compare(q,s')(37,0)of{LT->case compare(q,s')(37,-1)of{LT->Nothing;EQ->Just(Reduce 1 22);GT->Nothing};EQ->Just(Reduce 1 22);GT->case compare(q,s')(37,13)of{LT->Nothing;EQ->Just(Reduce 1 22);GT->Nothing}};EQ->Just(Reduce 1 22);GT->case compare(q,s')(38,-1)of{LT->case compare(q,s')(37,16)of{LT->Nothing;EQ->Just(Reduce 1 22);GT->Nothing};EQ->Just(Reduce 1 24);GT->case compare(q,s')(38,0)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing}}}}}};EQ->Just(Reduce 1 24);GT->case compare(q,s')(42,14)of{LT->case compare(q,s')(40,-1)of{LT->case compare(q,s')(39,0)of{LT->case compare(q,s')(38,16)of{LT->case compare(q,s')(38,14)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing};EQ->Just(Reduce 1 24);GT->case compare(q,s')(39,-1)of{LT->Nothing;EQ->Just(Reduce 2 26);GT->Nothing}};EQ->Just(Reduce 2 26);GT->case compare(q,s')(39,14)of{LT->case compare(q,s')(39,13)of{LT->Nothing;EQ->Just(Reduce 2 26);GT->Nothing};EQ->Just(Reduce 2 26);GT->case compare(q,s')(39,16)of{LT->Nothing;EQ->Just(Reduce 2 26);GT->Nothing}}};EQ->Just(Reduce 3 27);GT->case compare(q,s')(40,16)of{LT->case compare(q,s')(40,13)of{LT->case compare(q,s')(40,0)of{LT->Nothing;EQ->Just(Reduce 3 27);GT->Nothing};EQ->Just(Reduce 3 27);GT->case compare(q,s')(40,14)of{LT->Nothing;EQ->Just(Reduce 3 27);GT->Nothing}};EQ->Just(Reduce 3 27);GT->case compare(q,s')(42,0)of{LT->case compare(q,s')(41,13)of{LT->Nothing;EQ->Just(Shift 40);GT->case compare(q,s')(42,-1)of{LT->Nothing;EQ->Just(Reduce 2 34);GT->Nothing}};EQ->Just(Reduce 2 34);GT->case compare(q,s')(42,13)of{LT->Nothing;EQ->Just(Reduce 2 34);GT->Nothing}}}};EQ->Just(Reduce 2 34);GT->case compare(q,s')(45,14)of{LT->case compare(q,s')(43,13)of{LT->case compare(q,s')(43,-1)of{LT->case compare(q,s')(42,16)of{LT->Nothing;EQ->Just(Reduce 2 34);GT->Nothing};EQ->Just(Reduce 3 35);GT->case compare(q,s')(43,0)of{LT->Nothing;EQ->Just(Reduce 3 35);GT->Nothing}};EQ->Just(Reduce 3 35);GT->case compare(q,s')(43,16)of{LT->case compare(q,s')(43,14)of{LT->Nothing;EQ->Just(Reduce 3 35);GT->Nothing};EQ->Just(Reduce 3 35);GT->case compare(q,s')(44,16)of{LT->Nothing;EQ->Just(Shift 43);GT->case compare(q,s')(45,13)of{LT->Nothing;EQ->Just(Reduce 1 28);GT->Nothing}}}};EQ->Just(Shift 20);GT->case compare(q,s')(48,13)of{LT->case compare(q,s')(47,-1)of{LT->case compare(q,s')(46,13)of{LT->Nothing;EQ->Just(Reduce 3 30);GT->Nothing};EQ->Just(Reduce 3 33);GT->case compare(q,s')(47,0)of{LT->Nothing;EQ->Just(Reduce 3 33);GT->Nothing}};EQ->Just(Shift 47);GT->case compare(q,s')(52,14)of{LT->case compare(q,s')(50,13)of{LT->case compare(q,s')(49,0)of{LT->case compare(q,s')(49,-1)of{LT->Nothing;EQ->Just(Reduce 5 38);GT->Nothing};EQ->Just(Reduce 5 38);GT->Nothing};EQ->Just(Shift 49);GT->case compare(q,s')(51,13)of{LT->Nothing;EQ->Just(Shift 50);GT->Nothing}};EQ->Just(Shift 9);GT->case compare(q,s')(53,16)of{LT->case compare(q,s')(52,16)of{LT->Nothing;EQ->Just(Reduce 1 36);GT->Nothing};EQ->Just(Reduce 3 37);GT->Nothing}}}}}}

production :: Int -> Int
production 0 = 0
production 1 = 1
production 2 = 1
production 3 = 2
production 4 = 2
production 5 = 2
production 6 = 3
production 7 = 5
production 8 = 5
production 9 = 7
production 10 = 7
production 11 = 9
production 12 = 9
production 13 = 8
production 14 = 8
production 15 = 6
production 16 = 6
production 17 = 6
production 18 = 6
production 19 = 6
production 20 = 6
production 21 = 6
production 22 = 10
production 23 = 10
production 24 = 10
production 25 = 10
production 26 = 11
production 27 = 11
production 28 = 13
production 29 = 13
production 30 = 13
production 31 = 4
production 32 = 4
production 33 = 14
production 34 = 12
production 35 = 12
production 36 = 16
production 37 = 16
production 38 = 15
production _ = undefined

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  let s' = production s in
    case compare(q,s')(9,7)of{LT->case compare(q,s')(3,4)of{LT->case compare(q,s')(0,8)of{LT->case compare(q,s')(0,3)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just 1;GT->Nothing};EQ->Just 2;GT->case compare(q,s')(0,2)of{LT->Nothing;EQ->Just 5;GT->Nothing}};EQ->Just 6;GT->case compare(q,s')(0,5)of{LT->case compare(q,s')(0,4)of{LT->Nothing;EQ->Just 7;GT->Nothing};EQ->Just 11;GT->case compare(q,s')(0,7)of{LT->Nothing;EQ->Just 17;GT->Nothing}}};EQ->Just 18;GT->case compare(q,s')(3,1)of{LT->case compare(q,s')(0,14)of{LT->case compare(q,s')(0,9)of{LT->Nothing;EQ->Just 30;GT->Nothing};EQ->Just 13;GT->case compare(q,s')(0,15)of{LT->Nothing;EQ->Just 14;GT->Nothing}};EQ->Just 4;GT->case compare(q,s')(3,3)of{LT->case compare(q,s')(3,2)of{LT->Nothing;EQ->Just 5;GT->Nothing};EQ->Just 6;GT->Nothing}}};EQ->Just 7;GT->case compare(q,s')(8,5)of{LT->case compare(q,s')(3,9)of{LT->case compare(q,s')(3,7)of{LT->case compare(q,s')(3,5)of{LT->Nothing;EQ->Just 11;GT->Nothing};EQ->Just 17;GT->case compare(q,s')(3,8)of{LT->Nothing;EQ->Just 18;GT->Nothing}};EQ->Just 30;GT->case compare(q,s')(3,15)of{LT->case compare(q,s')(3,14)of{LT->Nothing;EQ->Just 13;GT->Nothing};EQ->Just 14;GT->case compare(q,s')(8,3)of{LT->Nothing;EQ->Just 52;GT->Nothing}}};EQ->Just 11;GT->case compare(q,s')(8,16)of{LT->case compare(q,s')(8,8)of{LT->case compare(q,s')(8,7)of{LT->Nothing;EQ->Just 17;GT->Nothing};EQ->Just 18;GT->case compare(q,s')(8,9)of{LT->Nothing;EQ->Just 30;GT->Nothing}};EQ->Just 44;GT->case compare(q,s')(9,5)of{LT->case compare(q,s')(9,3)of{LT->Nothing;EQ->Just 52;GT->Nothing};EQ->Just 11;GT->Nothing}}}};EQ->Just 17;GT->case compare(q,s')(16,9)of{LT->case compare(q,s')(15,5)of{LT->case compare(q,s')(10,6)of{LT->case compare(q,s')(9,9)of{LT->case compare(q,s')(9,8)of{LT->Nothing;EQ->Just 18;GT->Nothing};EQ->Just 30;GT->case compare(q,s')(9,16)of{LT->Nothing;EQ->Just 53;GT->Nothing}};EQ->Just 12;GT->case compare(q,s')(10,11)of{LT->case compare(q,s')(10,10)of{LT->Nothing;EQ->Just 25;GT->Nothing};EQ->Just 26;GT->case compare(q,s')(10,12)of{LT->Nothing;EQ->Just 27;GT->Nothing}}};EQ->Just 48;GT->case compare(q,s')(16,5)of{LT->case compare(q,s')(15,8)of{LT->case compare(q,s')(15,7)of{LT->Nothing;EQ->Just 17;GT->Nothing};EQ->Just 18;GT->case compare(q,s')(15,9)of{LT->Nothing;EQ->Just 30;GT->Nothing}};EQ->Just 51;GT->case compare(q,s')(16,8)of{LT->case compare(q,s')(16,7)of{LT->Nothing;EQ->Just 17;GT->Nothing};EQ->Just 18;GT->Nothing}}};EQ->Just 30;GT->case compare(q,s')(20,10)of{LT->case compare(q,s')(19,12)of{LT->case compare(q,s')(19,10)of{LT->case compare(q,s')(19,6)of{LT->Nothing;EQ->Just 45;GT->Nothing};EQ->Just 25;GT->case compare(q,s')(19,11)of{LT->Nothing;EQ->Just 26;GT->Nothing}};EQ->Just 27;GT->case compare(q,s')(20,6)of{LT->case compare(q,s')(19,13)of{LT->Nothing;EQ->Just 41;GT->Nothing};EQ->Just 45;GT->Nothing}};EQ->Just 25;GT->case compare(q,s')(28,7)of{LT->case compare(q,s')(20,12)of{LT->case compare(q,s')(20,11)of{LT->Nothing;EQ->Just 26;GT->Nothing};EQ->Just 27;GT->case compare(q,s')(20,13)of{LT->Nothing;EQ->Just 46;GT->Nothing}};EQ->Just 31;GT->case compare(q,s')(28,9)of{LT->case compare(q,s')(28,8)of{LT->Nothing;EQ->Just 32;GT->Nothing};EQ->Just 30;GT->Nothing}}}}}

parse :: Monad m => SemanticActions m -> [Token] -> m (Either (Maybe Token) (Toml, [Token]))
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
                  Token (NEWLINE semanticValue) ->
                    StackValue_NEWLINE semanticValue
                  Token (EQUAL semanticValue) ->
                    StackValue_EQUAL semanticValue
                  Token (UNQUOTED_KEY semanticValue) ->
                    StackValue_UNQUOTED_KEY semanticValue
                  Token (BASIC_STRING semanticValue) ->
                    StackValue_BASIC_STRING semanticValue
                  Token (LITERAL_STRING semanticValue) ->
                    StackValue_LITERAL_STRING semanticValue
                  Token (DOT semanticValue) ->
                    StackValue_DOT semanticValue
                  Token (BOOLEAN semanticValue) ->
                    StackValue_BOOLEAN semanticValue
                  Token (DATE_TIME semanticValue) ->
                    StackValue_DATE_TIME semanticValue
                  Token (FLOAT semanticValue) ->
                    StackValue_FLOAT semanticValue
                  Token (INTEGER semanticValue) ->
                    StackValue_INTEGER semanticValue
                  Token (ML_BASIC_STRING semanticValue) ->
                    StackValue_ML_BASIC_STRING semanticValue
                  Token (ML_LITERAL_STRING semanticValue) ->
                    StackValue_ML_LITERAL_STRING semanticValue
                  Token (LBRACKET semanticValue) ->
                    StackValue_LBRACKET semanticValue
                  Token (RBRACKET semanticValue) ->
                    StackValue_RBRACKET semanticValue
                  Token (COMMA semanticValue) ->
                    StackValue_COMMA semanticValue
                  Token (LBRACE semanticValue) ->
                    StackValue_LBRACE semanticValue
                  Token (RBRACE semanticValue) ->
                    StackValue_RBRACE semanticValue
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
                      Monad.liftM StackValue_toml $ toml_implies_expressions actions (case snd (pop !! 0) of { StackValue_expressions value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_expressions $ expressions_implies_expression actions (case snd (pop !! 0) of { StackValue_expression value -> value; _ -> undefined })
                    2 ->
                      Monad.liftM StackValue_expressions $ expressions_implies_expression_NEWLINE_expressions actions (case snd (pop !! 2) of { StackValue_expression value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_NEWLINE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_expressions value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_expression $ expression_implies actions
                    4 ->
                      Monad.liftM StackValue_expression $ expression_implies_keyval actions (case snd (pop !! 0) of { StackValue_keyval value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_expression $ expression_implies_table actions (case snd (pop !! 0) of { StackValue_table value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_keyval $ keyval_implies_key_EQUAL_val actions (case snd (pop !! 2) of { StackValue_key value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_val value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_key $ key_implies_simpleKey actions (case snd (pop !! 0) of { StackValue_simpleKey value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_key $ key_implies_dottedKey actions (case snd (pop !! 0) of { StackValue_dottedKey value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_simpleKey $ simpleKey_implies_quotedKey actions (case snd (pop !! 0) of { StackValue_quotedKey value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_simpleKey $ simpleKey_implies_UNQUOTED_KEY actions (case snd (pop !! 0) of { StackValue_UNQUOTED_KEY value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_quotedKey $ quotedKey_implies_BASIC_STRING actions (case snd (pop !! 0) of { StackValue_BASIC_STRING value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_quotedKey $ quotedKey_implies_LITERAL_STRING actions (case snd (pop !! 0) of { StackValue_LITERAL_STRING value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_dottedKey $ dottedKey_implies_simpleKey_DOT_simpleKey actions (case snd (pop !! 2) of { StackValue_simpleKey value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_simpleKey value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_dottedKey $ dottedKey_implies_simpleKey_DOT_dottedKey actions (case snd (pop !! 2) of { StackValue_simpleKey value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dottedKey value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_val $ val_implies_string' actions (case snd (pop !! 0) of { StackValue_string' value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_val $ val_implies_BOOLEAN actions (case snd (pop !! 0) of { StackValue_BOOLEAN value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_val $ val_implies_array actions (case snd (pop !! 0) of { StackValue_array value -> value; _ -> undefined })
                    18 ->
                      Monad.liftM StackValue_val $ val_implies_inlineTable actions (case snd (pop !! 0) of { StackValue_inlineTable value -> value; _ -> undefined })
                    19 ->
                      Monad.liftM StackValue_val $ val_implies_DATE_TIME actions (case snd (pop !! 0) of { StackValue_DATE_TIME value -> value; _ -> undefined })
                    20 ->
                      Monad.liftM StackValue_val $ val_implies_FLOAT actions (case snd (pop !! 0) of { StackValue_FLOAT value -> value; _ -> undefined })
                    21 ->
                      Monad.liftM StackValue_val $ val_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    22 ->
                      Monad.liftM StackValue_string' $ string'_implies_ML_BASIC_STRING actions (case snd (pop !! 0) of { StackValue_ML_BASIC_STRING value -> value; _ -> undefined })
                    23 ->
                      Monad.liftM StackValue_string' $ string'_implies_BASIC_STRING actions (case snd (pop !! 0) of { StackValue_BASIC_STRING value -> value; _ -> undefined })
                    24 ->
                      Monad.liftM StackValue_string' $ string'_implies_ML_LITERAL_STRING actions (case snd (pop !! 0) of { StackValue_ML_LITERAL_STRING value -> value; _ -> undefined })
                    25 ->
                      Monad.liftM StackValue_string' $ string'_implies_LITERAL_STRING actions (case snd (pop !! 0) of { StackValue_LITERAL_STRING value -> value; _ -> undefined })
                    26 ->
                      Monad.liftM StackValue_array $ array_implies_LBRACKET_RBRACKET actions (case snd (pop !! 1) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    27 ->
                      Monad.liftM StackValue_array $ array_implies_LBRACKET_arrayValues_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_arrayValues value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    28 ->
                      Monad.liftM StackValue_arrayValues $ arrayValues_implies_val actions (case snd (pop !! 0) of { StackValue_val value -> value; _ -> undefined })
                    29 ->
                      Monad.liftM StackValue_arrayValues $ arrayValues_implies_val_COMMA actions (case snd (pop !! 1) of { StackValue_val value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_COMMA value -> value; _ -> undefined })
                    30 ->
                      Monad.liftM StackValue_arrayValues $ arrayValues_implies_val_COMMA_arrayValues actions (case snd (pop !! 2) of { StackValue_val value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_arrayValues value -> value; _ -> undefined })
                    31 ->
                      Monad.liftM StackValue_table $ table_implies_stdTable actions (case snd (pop !! 0) of { StackValue_stdTable value -> value; _ -> undefined })
                    32 ->
                      Monad.liftM StackValue_table $ table_implies_arrayTable actions (case snd (pop !! 0) of { StackValue_arrayTable value -> value; _ -> undefined })
                    33 ->
                      Monad.liftM StackValue_stdTable $ stdTable_implies_LBRACKET_key_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_key value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    34 ->
                      Monad.liftM StackValue_inlineTable $ inlineTable_implies_LBRACE_RBRACE actions (case snd (pop !! 1) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    35 ->
                      Monad.liftM StackValue_inlineTable $ inlineTable_implies_LBRACE_inlineTableKeyVals_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_inlineTableKeyVals value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    36 ->
                      Monad.liftM StackValue_inlineTableKeyVals $ inlineTableKeyVals_implies_keyval actions (case snd (pop !! 0) of { StackValue_keyval value -> value; _ -> undefined })
                    37 ->
                      Monad.liftM StackValue_inlineTableKeyVals $ inlineTableKeyVals_implies_keyval_COMMA_inlineTableKeyVals actions (case snd (pop !! 2) of { StackValue_keyval value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_inlineTableKeyVals value -> value; _ -> undefined })
                    38 ->
                      Monad.liftM StackValue_arrayTable $ arrayTable_implies_LBRACKET_LBRACKET_key_RBRACKET_RBRACKET actions (case snd (pop !! 4) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_key value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RBRACKET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    _ -> undefined
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_toml value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { toml_implies_expressions = \expressions ->
      let f _ [] m = m
          f (Right (k1, i)) (Right (Right k2) : exps) m
            | k1 == k2 = f (Right (k1, i + 1)) exps m
          f _ (Right (Left key) : exps) m = f (Left key) exps m
          f _ (Right (Right key) : exps) m = f (Right (key, 0)) exps m
          f (Left k) (Left (key, val) : exps) m =
            let dottedKey =
                  case k of
                    Left k' -> [k']
                    Right dkey -> dkey in
            let dottedKey1 =
                  case key of
                    Left k' -> [k']
                    Right dkey -> dkey in
            let m' = lookup dottedKey m in
            let m'' = insert dottedKey1 val m' in
              case dottedKey of
                [] ->
                  f (Left k) exps m''
                _ ->
                  f (Left k) exps (insert dottedKey (TOMLTable m'') m)
          f (Right (k, i)) (Left (key, val) : exps) m =
            let dottedKey =
                  case k of
                    Left k' -> [k']
                    Right dkey -> dkey in
            let dottedKey1 =
                  case key of
                    Left k' -> [k']
                    Right dkey -> dkey in
            let a = lookupArray dottedKey m in
            let m' =
                  if length a == i then
                    RBMap.empty
                  else
                    case head a of
                      TOMLTable m ->
                        m
                      _ ->
                        RBMap.empty in
            let m'' = insert dottedKey1 val m' in
            let a' =
                  if length a == i then
                    a ++ [TOMLTable m'']
                  else
                    init a ++ [TOMLTable m''] in
              f (Right (k, i)) exps (insert dottedKey (TOMLArray a') m)

          lookup [] m = m
          lookup (k : ks) m =
            case RBMap.lookup k m of
              Nothing ->
                RBMap.empty
              Just (TOMLTable m') ->
                lookup ks m'
              Just _ ->
                RBMap.empty

          lookupArray [] m = undefined
          lookupArray [k] m =
            case RBMap.lookup k m of
              Nothing ->
                []
              Just (TOMLArray a) ->
                a
              Just _ ->
                []
          lookupArray (k : ks) m =
            case RBMap.lookup k m of
              Nothing ->
                []
              Just (TOMLTable m') ->
                lookupArray ks m'
              Just _ ->
                []

          insert [] _ m = m
          insert [k] v m = RBMap.insert k v m
          insert (k : ks) v m =
            let m'' =
                  case RBMap.lookup k m of
                    Nothing ->
                      RBMap.empty
                    Just (TOMLTable m') ->
                      m'
                    Just _ ->
                      RBMap.empty in
              RBMap.insert k (TOMLTable (insert ks v m'')) m in
        return $ f (Left (Right [])) (Maybe.catMaybes expressions) RBMap.empty
  , expressions_implies_expression = \exp -> return [exp]
  , expressions_implies_expression_NEWLINE_expressions = \exp _ exps -> return $ exp : exps
  , expression_implies = return Nothing
  , expression_implies_keyval = return . Just . Left
  , expression_implies_table = return . Just . Right
  , keyval_implies_key_EQUAL_val = \key _ val ->
      return (key, val)
  , key_implies_simpleKey = return . Left
  , key_implies_dottedKey = return . Right
  , simpleKey_implies_quotedKey = return
  , simpleKey_implies_UNQUOTED_KEY = return . snd
  , quotedKey_implies_BASIC_STRING = return . snd
  , quotedKey_implies_LITERAL_STRING = return . snd
  , dottedKey_implies_simpleKey_DOT_simpleKey = \k1 _ k2 -> return [k1, k2]
  , dottedKey_implies_simpleKey_DOT_dottedKey = \k _ ks -> return $ k : ks
  , val_implies_string' = return . TOMLString
  , val_implies_BOOLEAN = return . TOMLBoolean . snd
  , val_implies_array = return . TOMLArray
  , val_implies_inlineTable = return . TOMLTable
  , val_implies_DATE_TIME = return . TOMLDateTime . snd
  , val_implies_FLOAT = return . TOMLFloat . snd
  , val_implies_INTEGER = return . TOMLInteger . snd
  , string'_implies_ML_BASIC_STRING = return . snd
  , string'_implies_BASIC_STRING = return . snd
  , string'_implies_ML_LITERAL_STRING = return . snd
  , string'_implies_LITERAL_STRING = return . snd
  , array_implies_LBRACKET_RBRACKET = \_ _ -> return []
  , array_implies_LBRACKET_arrayValues_RBRACKET = \_ vals _ -> return vals
  , arrayValues_implies_val = \val -> return [val]
  , arrayValues_implies_val_COMMA = \val _ -> return [val]
  , arrayValues_implies_val_COMMA_arrayValues = \val _ vals -> return $ val : vals
  , table_implies_stdTable = return . Left
  , table_implies_arrayTable = return . Right
  , stdTable_implies_LBRACKET_key_RBRACKET = \_ key _ -> return key
  , inlineTable_implies_LBRACE_RBRACE = \_ _ -> return RBMap.empty
  , inlineTable_implies_LBRACE_inlineTableKeyVals_RBRACE = \_ keyvals _ ->
      let f [] m = m
          f ((key, val) : kvs) m =
            case key of
              Left simpleKey ->
                f kvs (RBMap.insert simpleKey val m)
              Right dottedKey ->
                let parent = init dottedKey in
                let self = last dottedKey in
                let m' = lookup parent m in
                let m'' = RBMap.insert self val m' in
                  f kvs (insert parent (TOMLTable m'') m)

          lookup [] m = m
          lookup (k : ks) m =
            case RBMap.lookup k m of
              Nothing ->
                RBMap.empty
              Just (TOMLTable m') ->
                lookup ks m'
              Just _ ->
                RBMap.empty

          insert [] _ m = m
          insert [k] v m = RBMap.insert k v m
          insert (k : ks) v m =
            let m'' =
                  case RBMap.lookup k m of
                    Nothing ->
                      RBMap.empty
                    Just (TOMLTable m') ->
                      m'
                    Just _ ->
                      RBMap.empty in
              RBMap.insert k (TOMLTable (insert ks v m'')) m in
        return $ f keyvals RBMap.empty
  , inlineTableKeyVals_implies_keyval = \keyval -> return [keyval]
  , inlineTableKeyVals_implies_keyval_COMMA_inlineTableKeyVals = \keyval _ keyvals -> return $ keyval : keyvals
  , arrayTable_implies_LBRACKET_LBRACKET_key_RBRACKET_RBRACKET = \_ _ key _ _ -> return key
  }


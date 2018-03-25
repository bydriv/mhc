module  Data.Scheme.Parsing  where
import qualified Control.Monad as Monad


import qualified Data.Word as Word

type EQUAL = ()
type SHARP = ()
type BOOLEAN = Bool
type NUMBER = Double
type CHARACTER = Char
type STRING = String
type BYTEVECTOR = [Word.Word8]
type IDENTIFIER = String
type LPAREN = ()
type RPAREN = ()
type DOT = ()
type QUOTE = ()
type BACKQUOTE = ()
type COMMA = ()
type COMMAAT = ()
type UINTEGER10 = Word.Word64
type SHARP_SEMICOLON = ()

data Datum =
    Bool BOOLEAN
  | Number NUMBER
  | Character CHARACTER
  | String STRING
  | Symbol Symbol
  | ByteVector BYTEVECTOR
  | List0 Data0
  | List1 (Data1, Datum)
  | Vector Vector
  | Quote Datum
  | Backquote Datum
  | Comma Datum
  | CommaAt Datum
  | Labeling Label Datum
  | LabelRef Label
  | ExprComment Datum
  deriving (Eq, Ord, Read, Show)

type SimpleDatum = Datum
type Symbol = IDENTIFIER
type CompoundDatum = Datum
type List = Datum
type Data0 = [Datum]
type Data1 = [Datum]
type Abbreviation = Datum

data AbbrevPrefix =
    APQuote
  | APBackquote
  | APComma
  | APCommaAt
  deriving (Eq, Ord, Read, Show)

type Vector = Datum
type Label = UINTEGER10

data Token =
    BACKQUOTE BACKQUOTE
  | BOOLEAN BOOLEAN
  | BYTEVECTOR BYTEVECTOR
  | CHARACTER CHARACTER
  | COMMA COMMA
  | COMMAAT COMMAAT
  | DOT DOT
  | EQUAL EQUAL
  | IDENTIFIER IDENTIFIER
  | LPAREN LPAREN
  | NUMBER NUMBER
  | QUOTE QUOTE
  | RPAREN RPAREN
  | SHARP SHARP
  | SHARP_SEMICOLON SHARP_SEMICOLON
  | STRING STRING
  | UINTEGER10 UINTEGER10
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
  deriving (Eq, Ord, Read, Show)
type GotoState = Int
type GotoSymbol = Int

data StackValue =
    StackValue_EOF
  | StackValue_EQUAL EQUAL
  | StackValue_SHARP SHARP
  | StackValue_SHARP_SEMICOLON SHARP_SEMICOLON
  | StackValue_BOOLEAN BOOLEAN
  | StackValue_NUMBER NUMBER
  | StackValue_CHARACTER CHARACTER
  | StackValue_STRING STRING
  | StackValue_BYTEVECTOR BYTEVECTOR
  | StackValue_IDENTIFIER IDENTIFIER
  | StackValue_LPAREN LPAREN
  | StackValue_RPAREN RPAREN
  | StackValue_DOT DOT
  | StackValue_QUOTE QUOTE
  | StackValue_BACKQUOTE BACKQUOTE
  | StackValue_COMMA COMMA
  | StackValue_COMMAAT COMMAAT
  | StackValue_UINTEGER10 UINTEGER10
  | StackValue_datum Datum
  | StackValue_simpleDatum SimpleDatum
  | StackValue_compoundDatum CompoundDatum
  | StackValue_label Label
  | StackValue_symbol Symbol
  | StackValue_list List
  | StackValue_vector Vector
  | StackValue_abbreviation Abbreviation
  | StackValue_data0 Data0
  | StackValue_data1 Data1
  | StackValue_abbrevPrefix AbbrevPrefix

data SemanticActions m = SemanticActions
  { datum_implies_simpleDatum :: SimpleDatum -> m Datum
  , datum_implies_compoundDatum :: CompoundDatum -> m Datum
  , datum_implies_label_EQUAL_datum :: Label -> EQUAL -> Datum -> m Datum
  , datum_implies_label_SHARP :: Label -> SHARP -> m Datum
  , datum_implies_SHARP_SEMICOLON_datum :: SHARP_SEMICOLON -> Datum -> m Datum
  , simpleDatum_implies_BOOLEAN :: BOOLEAN -> m SimpleDatum
  , simpleDatum_implies_NUMBER :: NUMBER -> m SimpleDatum
  , simpleDatum_implies_CHARACTER :: CHARACTER -> m SimpleDatum
  , simpleDatum_implies_STRING :: STRING -> m SimpleDatum
  , simpleDatum_implies_symbol :: Symbol -> m SimpleDatum
  , simpleDatum_implies_BYTEVECTOR :: BYTEVECTOR -> m SimpleDatum
  , symbol_implies_IDENTIFIER :: IDENTIFIER -> m Symbol
  , symbol_implies_EQUAL :: EQUAL -> m Symbol
  , compoundDatum_implies_list :: List -> m CompoundDatum
  , compoundDatum_implies_vector :: Vector -> m CompoundDatum
  , compoundDatum_implies_abbreviation :: Abbreviation -> m CompoundDatum
  , list_implies_LPAREN_data0_RPAREN :: LPAREN -> Data0 -> RPAREN -> m List
  , list_implies_LPAREN_data1_DOT_datum_RPAREN :: LPAREN -> Data1 -> DOT -> Datum -> RPAREN -> m List
  , data0_implies :: m Data0
  , data0_implies_datum_data0 :: Datum -> Data0 -> m Data0
  , data1_implies_datum :: Datum -> m Data1
  , data1_implies_datum_data1 :: Datum -> Data1 -> m Data1
  , abbreviation_implies_abbrevPrefix_datum :: AbbrevPrefix -> Datum -> m Abbreviation
  , abbrevPrefix_implies_QUOTE :: QUOTE -> m AbbrevPrefix
  , abbrevPrefix_implies_BACKQUOTE :: BACKQUOTE -> m AbbrevPrefix
  , abbrevPrefix_implies_COMMA :: COMMA -> m AbbrevPrefix
  , abbrevPrefix_implies_COMMAAT :: COMMAAT -> m AbbrevPrefix
  , vector_implies_SHARP_LPAREN_datum_RPAREN :: SHARP -> LPAREN -> Datum -> RPAREN -> m Vector
  , label_implies_UINTEGER10 :: UINTEGER10 -> m Label }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  let s' =
        case s of
          EOF -> -1
          Token (BACKQUOTE _) -> 13
          Token (BOOLEAN _) -> 3
          Token (BYTEVECTOR _) -> 7
          Token (CHARACTER _) -> 5
          Token (COMMA _) -> 14
          Token (COMMAAT _) -> 15
          Token (DOT _) -> 11
          Token (EQUAL _) -> 0
          Token (IDENTIFIER _) -> 8
          Token (LPAREN _) -> 9
          Token (NUMBER _) -> 4
          Token (QUOTE _) -> 12
          Token (RPAREN _) -> 10
          Token (SHARP _) -> 1
          Token (SHARP_SEMICOLON _) -> 2
          Token (STRING _) -> 6
          Token (UINTEGER10 _) -> 16
  in case compare(q,s')(15,12)of{LT->case compare(q,s')(7,8)of{LT->case compare(q,s')(5,7)of{LT->case compare(q,s')(3,5)of{LT->case compare(q,s')(2,4)of{LT->case compare(q,s')(0,14)of{LT->case compare(q,s')(0,8)of{LT->case compare(q,s')(0,4)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just(Shift 25);GT->Nothing};EQ->Just(Shift 32);GT->Nothing};EQ->Just(Shift 3);GT->case compare(q,s')(0,3)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing}};EQ->Just(Shift 16);GT->case compare(q,s')(0,6)of{LT->case compare(q,s')(0,5)of{LT->Nothing;EQ->Just(Shift 17);GT->Nothing};EQ->Just(Shift 18);GT->case compare(q,s')(0,7)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing}}};EQ->Just(Shift 26);GT->case compare(q,s')(0,12)of{LT->case compare(q,s')(0,9)of{LT->Nothing;EQ->Just(Shift 5);GT->Nothing};EQ->Just(Shift 38);GT->case compare(q,s')(0,13)of{LT->Nothing;EQ->Just(Shift 39);GT->Nothing}}};EQ->Just(Shift 40);GT->case compare(q,s')(2,0)of{LT->case compare(q,s')(0,16)of{LT->case compare(q,s')(0,15)of{LT->Nothing;EQ->Just(Shift 41);GT->Nothing};EQ->Just(Shift 24);GT->case compare(q,s')(1,-1)of{LT->Nothing;EQ->Just(Accept);GT->Nothing}};EQ->Just(Shift 25);GT->case compare(q,s')(2,2)of{LT->case compare(q,s')(2,1)of{LT->Nothing;EQ->Just(Shift 32);GT->Nothing};EQ->Just(Shift 3);GT->case compare(q,s')(2,3)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing}}}};EQ->Just(Shift 16);GT->case compare(q,s')(2,14)of{LT->case compare(q,s')(2,8)of{LT->case compare(q,s')(2,6)of{LT->case compare(q,s')(2,5)of{LT->Nothing;EQ->Just(Shift 17);GT->Nothing};EQ->Just(Shift 18);GT->case compare(q,s')(2,7)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing}};EQ->Just(Shift 26);GT->case compare(q,s')(2,12)of{LT->case compare(q,s')(2,9)of{LT->Nothing;EQ->Just(Shift 5);GT->Nothing};EQ->Just(Shift 38);GT->case compare(q,s')(2,13)of{LT->Nothing;EQ->Just(Shift 39);GT->Nothing}}};EQ->Just(Shift 40);GT->case compare(q,s')(3,1)of{LT->case compare(q,s')(2,16)of{LT->case compare(q,s')(2,15)of{LT->Nothing;EQ->Just(Shift 41);GT->Nothing};EQ->Just(Shift 24);GT->case compare(q,s')(3,0)of{LT->Nothing;EQ->Just(Shift 25);GT->Nothing}};EQ->Just(Shift 32);GT->case compare(q,s')(3,3)of{LT->case compare(q,s')(3,2)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing};EQ->Just(Shift 15);GT->case compare(q,s')(3,4)of{LT->Nothing;EQ->Just(Shift 16);GT->Nothing}}}}};EQ->Just(Shift 17);GT->case compare(q,s')(4,6)of{LT->case compare(q,s')(3,15)of{LT->case compare(q,s')(3,9)of{LT->case compare(q,s')(3,7)of{LT->case compare(q,s')(3,6)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing};EQ->Just(Shift 19);GT->case compare(q,s')(3,8)of{LT->Nothing;EQ->Just(Shift 26);GT->Nothing}};EQ->Just(Shift 5);GT->case compare(q,s')(3,13)of{LT->case compare(q,s')(3,12)of{LT->Nothing;EQ->Just(Shift 38);GT->Nothing};EQ->Just(Shift 39);GT->case compare(q,s')(3,14)of{LT->Nothing;EQ->Just(Shift 40);GT->Nothing}}};EQ->Just(Shift 41);GT->case compare(q,s')(4,2)of{LT->case compare(q,s')(4,0)of{LT->case compare(q,s')(3,16)of{LT->Nothing;EQ->Just(Shift 24);GT->Nothing};EQ->Just(Shift 25);GT->case compare(q,s')(4,1)of{LT->Nothing;EQ->Just(Shift 32);GT->Nothing}};EQ->Just(Shift 3);GT->case compare(q,s')(4,4)of{LT->case compare(q,s')(4,3)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing};EQ->Just(Shift 16);GT->case compare(q,s')(4,5)of{LT->Nothing;EQ->Just(Shift 17);GT->Nothing}}}};EQ->Just(Shift 18);GT->case compare(q,s')(4,16)of{LT->case compare(q,s')(4,12)of{LT->case compare(q,s')(4,8)of{LT->case compare(q,s')(4,7)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing};EQ->Just(Shift 26);GT->case compare(q,s')(4,9)of{LT->Nothing;EQ->Just(Shift 5);GT->Nothing}};EQ->Just(Shift 38);GT->case compare(q,s')(4,14)of{LT->case compare(q,s')(4,13)of{LT->Nothing;EQ->Just(Shift 39);GT->Nothing};EQ->Just(Shift 40);GT->case compare(q,s')(4,15)of{LT->Nothing;EQ->Just(Shift 41);GT->Nothing}}};EQ->Just(Shift 24);GT->case compare(q,s')(5,3)of{LT->case compare(q,s')(5,1)of{LT->case compare(q,s')(5,0)of{LT->Nothing;EQ->Just(Shift 25);GT->Nothing};EQ->Just(Shift 32);GT->case compare(q,s')(5,2)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing}};EQ->Just(Shift 15);GT->case compare(q,s')(5,5)of{LT->case compare(q,s')(5,4)of{LT->Nothing;EQ->Just(Shift 16);GT->Nothing};EQ->Just(Shift 17);GT->case compare(q,s')(5,6)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing}}}}}};EQ->Just(Shift 19);GT->case compare(q,s')(6,8)of{LT->case compare(q,s')(6,0)of{LT->case compare(q,s')(5,13)of{LT->case compare(q,s')(5,9)of{LT->case compare(q,s')(5,8)of{LT->Nothing;EQ->Just(Shift 26);GT->Nothing};EQ->Just(Shift 5);GT->case compare(q,s')(5,10)of{LT->Nothing;EQ->Just(Reduce 0 18);GT->case compare(q,s')(5,12)of{LT->Nothing;EQ->Just(Shift 38);GT->Nothing}}};EQ->Just(Shift 39);GT->case compare(q,s')(5,15)of{LT->case compare(q,s')(5,14)of{LT->Nothing;EQ->Just(Shift 40);GT->Nothing};EQ->Just(Shift 41);GT->case compare(q,s')(5,16)of{LT->Nothing;EQ->Just(Shift 24);GT->Nothing}}};EQ->Just(Shift 25);GT->case compare(q,s')(6,4)of{LT->case compare(q,s')(6,2)of{LT->case compare(q,s')(6,1)of{LT->Nothing;EQ->Just(Shift 32);GT->Nothing};EQ->Just(Shift 3);GT->case compare(q,s')(6,3)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing}};EQ->Just(Shift 16);GT->case compare(q,s')(6,6)of{LT->case compare(q,s')(6,5)of{LT->Nothing;EQ->Just(Shift 17);GT->Nothing};EQ->Just(Shift 18);GT->case compare(q,s')(6,7)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing}}}};EQ->Just(Shift 26);GT->case compare(q,s')(7,0)of{LT->case compare(q,s')(6,13)of{LT->case compare(q,s')(6,10)of{LT->case compare(q,s')(6,9)of{LT->Nothing;EQ->Just(Shift 5);GT->Nothing};EQ->Just(Reduce 0 18);GT->case compare(q,s')(6,11)of{LT->Nothing;EQ->Just(Reduce 1 20);GT->case compare(q,s')(6,12)of{LT->Nothing;EQ->Just(Shift 38);GT->Nothing}}};EQ->Just(Shift 39);GT->case compare(q,s')(6,15)of{LT->case compare(q,s')(6,14)of{LT->Nothing;EQ->Just(Shift 40);GT->Nothing};EQ->Just(Shift 41);GT->case compare(q,s')(6,16)of{LT->Nothing;EQ->Just(Shift 24);GT->Nothing}}};EQ->Just(Shift 25);GT->case compare(q,s')(7,4)of{LT->case compare(q,s')(7,2)of{LT->case compare(q,s')(7,1)of{LT->Nothing;EQ->Just(Shift 32);GT->Nothing};EQ->Just(Shift 3);GT->case compare(q,s')(7,3)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing}};EQ->Just(Shift 16);GT->case compare(q,s')(7,6)of{LT->case compare(q,s')(7,5)of{LT->Nothing;EQ->Just(Shift 17);GT->Nothing};EQ->Just(Shift 18);GT->case compare(q,s')(7,7)of{LT->Nothing;EQ->Just(Shift 19);GT->Nothing}}}}}};EQ->Just(Shift 26);GT->case compare(q,s')(10,4)of{LT->case compare(q,s')(8,9)of{LT->case compare(q,s')(8,1)of{LT->case compare(q,s')(7,14)of{LT->case compare(q,s')(7,12)of{LT->case compare(q,s')(7,9)of{LT->Nothing;EQ->Just(Shift 5);GT->Nothing};EQ->Just(Shift 38);GT->case compare(q,s')(7,13)of{LT->Nothing;EQ->Just(Shift 39);GT->Nothing}};EQ->Just(Shift 40);GT->case compare(q,s')(7,16)of{LT->case compare(q,s')(7,15)of{LT->Nothing;EQ->Just(Shift 41);GT->Nothing};EQ->Just(Shift 24);GT->case compare(q,s')(8,0)of{LT->Nothing;EQ->Just(Shift 25);GT->Nothing}}};EQ->Just(Shift 32);GT->case compare(q,s')(8,5)of{LT->case compare(q,s')(8,3)of{LT->case compare(q,s')(8,2)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing};EQ->Just(Shift 15);GT->case compare(q,s')(8,4)of{LT->Nothing;EQ->Just(Shift 16);GT->Nothing}};EQ->Just(Shift 17);GT->case compare(q,s')(8,7)of{LT->case compare(q,s')(8,6)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing};EQ->Just(Shift 19);GT->case compare(q,s')(8,8)of{LT->Nothing;EQ->Just(Shift 26);GT->Nothing}}}};EQ->Just(Shift 5);GT->case compare(q,s')(9,2)of{LT->case compare(q,s')(8,15)of{LT->case compare(q,s')(8,13)of{LT->case compare(q,s')(8,12)of{LT->Nothing;EQ->Just(Shift 38);GT->Nothing};EQ->Just(Shift 39);GT->case compare(q,s')(8,14)of{LT->Nothing;EQ->Just(Shift 40);GT->Nothing}};EQ->Just(Shift 41);GT->case compare(q,s')(9,-1)of{LT->case compare(q,s')(8,16)of{LT->Nothing;EQ->Just(Shift 24);GT->Nothing};EQ->Just(Reduce 2 3);GT->case compare(q,s')(9,1)of{LT->case compare(q,s')(9,0)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->Nothing}}};EQ->Just(Reduce 2 3);GT->case compare(q,s')(9,9)of{LT->case compare(q,s')(9,6)of{LT->case compare(q,s')(9,4)of{LT->case compare(q,s')(9,3)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->case compare(q,s')(9,5)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing}};EQ->Just(Reduce 2 3);GT->case compare(q,s')(9,8)of{LT->case compare(q,s')(9,7)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->Nothing}};EQ->Just(Reduce 2 3);GT->case compare(q,s')(9,16)of{LT->case compare(q,s')(9,13)of{LT->case compare(q,s')(9,11)of{LT->case compare(q,s')(9,10)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->case compare(q,s')(9,12)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing}};EQ->Just(Reduce 2 3);GT->case compare(q,s')(9,15)of{LT->case compare(q,s')(9,14)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->Nothing}};EQ->Just(Reduce 2 3);GT->case compare(q,s')(10,1)of{LT->case compare(q,s')(10,0)of{LT->case compare(q,s')(10,-1)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing};EQ->Just(Reduce 3 2);GT->Nothing};EQ->Just(Reduce 3 2);GT->case compare(q,s')(10,3)of{LT->case compare(q,s')(10,2)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing};EQ->Just(Reduce 3 2);GT->Nothing}}}}}};EQ->Just(Reduce 3 2);GT->case compare(q,s')(13,4)of{LT->case compare(q,s')(11,13)of{LT->case compare(q,s')(11,0)of{LT->case compare(q,s')(10,11)of{LT->case compare(q,s')(10,8)of{LT->case compare(q,s')(10,6)of{LT->case compare(q,s')(10,5)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing};EQ->Just(Reduce 3 2);GT->case compare(q,s')(10,7)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing}};EQ->Just(Reduce 3 2);GT->case compare(q,s')(10,10)of{LT->case compare(q,s')(10,9)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing};EQ->Just(Reduce 3 2);GT->Nothing}};EQ->Just(Reduce 3 2);GT->case compare(q,s')(10,15)of{LT->case compare(q,s')(10,13)of{LT->case compare(q,s')(10,12)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing};EQ->Just(Reduce 3 2);GT->case compare(q,s')(10,14)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing}};EQ->Just(Reduce 3 2);GT->case compare(q,s')(11,-1)of{LT->case compare(q,s')(10,16)of{LT->Nothing;EQ->Just(Reduce 3 2);GT->Nothing};EQ->Just(Reduce 2 4);GT->Nothing}}};EQ->Just(Reduce 2 4);GT->case compare(q,s')(11,7)of{LT->case compare(q,s')(11,4)of{LT->case compare(q,s')(11,2)of{LT->case compare(q,s')(11,1)of{LT->Nothing;EQ->Just(Reduce 2 4);GT->Nothing};EQ->Just(Reduce 2 4);GT->case compare(q,s')(11,3)of{LT->Nothing;EQ->Just(Reduce 2 4);GT->Nothing}};EQ->Just(Reduce 2 4);GT->case compare(q,s')(11,6)of{LT->case compare(q,s')(11,5)of{LT->Nothing;EQ->Just(Reduce 2 4);GT->Nothing};EQ->Just(Reduce 2 4);GT->Nothing}};EQ->Just(Reduce 2 4);GT->case compare(q,s')(11,10)of{LT->case compare(q,s')(11,9)of{LT->case compare(q,s')(11,8)of{LT->Nothing;EQ->Just(Reduce 2 4);GT->Nothing};EQ->Just(Reduce 2 4);GT->Nothing};EQ->Just(Reduce 2 4);GT->case compare(q,s')(11,12)of{LT->case compare(q,s')(11,11)of{LT->Nothing;EQ->Just(Reduce 2 4);GT->Nothing};EQ->Just(Reduce 2 4);GT->Nothing}}}};EQ->Just(Reduce 2 4);GT->case compare(q,s')(12,9)of{LT->case compare(q,s')(12,2)of{LT->case compare(q,s')(12,-1)of{LT->case compare(q,s')(11,15)of{LT->case compare(q,s')(11,14)of{LT->Nothing;EQ->Just(Reduce 2 4);GT->Nothing};EQ->Just(Reduce 2 4);GT->case compare(q,s')(11,16)of{LT->Nothing;EQ->Just(Reduce 2 4);GT->Nothing}};EQ->Just(Reduce 1 0);GT->case compare(q,s')(12,1)of{LT->case compare(q,s')(12,0)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->Nothing}};EQ->Just(Reduce 1 0);GT->case compare(q,s')(12,6)of{LT->case compare(q,s')(12,4)of{LT->case compare(q,s')(12,3)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->case compare(q,s')(12,5)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing}};EQ->Just(Reduce 1 0);GT->case compare(q,s')(12,8)of{LT->case compare(q,s')(12,7)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->Nothing}}};EQ->Just(Reduce 1 0);GT->case compare(q,s')(12,16)of{LT->case compare(q,s')(12,13)of{LT->case compare(q,s')(12,11)of{LT->case compare(q,s')(12,10)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->case compare(q,s')(12,12)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing}};EQ->Just(Reduce 1 0);GT->case compare(q,s')(12,15)of{LT->case compare(q,s')(12,14)of{LT->Nothing;EQ->Just(Reduce 1 0);GT->Nothing};EQ->Just(Reduce 1 0);GT->Nothing}};EQ->Just(Reduce 1 0);GT->case compare(q,s')(13,1)of{LT->case compare(q,s')(13,0)of{LT->case compare(q,s')(13,-1)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->case compare(q,s')(13,3)of{LT->case compare(q,s')(13,2)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->Nothing}}}}};EQ->Just(Reduce 1 1);GT->case compare(q,s')(13,16)of{LT->case compare(q,s')(13,11)of{LT->case compare(q,s')(13,8)of{LT->case compare(q,s')(13,6)of{LT->case compare(q,s')(13,5)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->case compare(q,s')(13,7)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing}};EQ->Just(Reduce 1 1);GT->case compare(q,s')(13,10)of{LT->case compare(q,s')(13,9)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->Nothing}};EQ->Just(Reduce 1 1);GT->case compare(q,s')(13,14)of{LT->case compare(q,s')(13,13)of{LT->case compare(q,s')(13,12)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->Nothing};EQ->Just(Reduce 1 1);GT->case compare(q,s')(13,15)of{LT->Nothing;EQ->Just(Reduce 1 1);GT->Nothing}}};EQ->Just(Reduce 1 1);GT->case compare(q,s')(15,-1)of{LT->case compare(q,s')(14,1)of{LT->case compare(q,s')(14,0)of{LT->Nothing;EQ->Just(Shift 2);GT->Nothing};EQ->Just(Shift 9);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(15,6)of{LT->case compare(q,s')(15,3)of{LT->case compare(q,s')(15,1)of{LT->case compare(q,s')(15,0)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(15,2)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing}};EQ->Just(Reduce 1 5);GT->case compare(q,s')(15,5)of{LT->case compare(q,s')(15,4)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing}};EQ->Just(Reduce 1 5);GT->case compare(q,s')(15,9)of{LT->case compare(q,s')(15,8)of{LT->case compare(q,s')(15,7)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(15,11)of{LT->case compare(q,s')(15,10)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->Nothing}}}}}}}};EQ->Just(Reduce 1 5);GT->case compare(q,s')(28,8)of{LT->case compare(q,s')(21,11)of{LT->case compare(q,s')(18,12)of{LT->case compare(q,s')(17,3)of{LT->case compare(q,s')(16,8)of{LT->case compare(q,s')(16,1)of{LT->case compare(q,s')(15,16)of{LT->case compare(q,s')(15,14)of{LT->case compare(q,s')(15,13)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing};EQ->Just(Reduce 1 5);GT->case compare(q,s')(15,15)of{LT->Nothing;EQ->Just(Reduce 1 5);GT->Nothing}};EQ->Just(Reduce 1 5);GT->case compare(q,s')(16,0)of{LT->case compare(q,s')(16,-1)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(16,5)of{LT->case compare(q,s')(16,3)of{LT->case compare(q,s')(16,2)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->case compare(q,s')(16,4)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(16,7)of{LT->case compare(q,s')(16,6)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing}}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(16,15)of{LT->case compare(q,s')(16,12)of{LT->case compare(q,s')(16,10)of{LT->case compare(q,s')(16,9)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->case compare(q,s')(16,11)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(16,14)of{LT->case compare(q,s')(16,13)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 6);GT->Nothing}};EQ->Just(Reduce 1 6);GT->case compare(q,s')(17,0)of{LT->case compare(q,s')(17,-1)of{LT->case compare(q,s')(16,16)of{LT->Nothing;EQ->Just(Reduce 1 6);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->case compare(q,s')(17,2)of{LT->case compare(q,s')(17,1)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing}}}};EQ->Just(Reduce 1 7);GT->case compare(q,s')(18,-1)of{LT->case compare(q,s')(17,10)of{LT->case compare(q,s')(17,7)of{LT->case compare(q,s')(17,5)of{LT->case compare(q,s')(17,4)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->case compare(q,s')(17,6)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing}};EQ->Just(Reduce 1 7);GT->case compare(q,s')(17,9)of{LT->case compare(q,s')(17,8)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing}};EQ->Just(Reduce 1 7);GT->case compare(q,s')(17,14)of{LT->case compare(q,s')(17,12)of{LT->case compare(q,s')(17,11)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->case compare(q,s')(17,13)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing}};EQ->Just(Reduce 1 7);GT->case compare(q,s')(17,16)of{LT->case compare(q,s')(17,15)of{LT->Nothing;EQ->Just(Reduce 1 7);GT->Nothing};EQ->Just(Reduce 1 7);GT->Nothing}}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(18,6)of{LT->case compare(q,s')(18,3)of{LT->case compare(q,s')(18,1)of{LT->case compare(q,s')(18,0)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->case compare(q,s')(18,2)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(18,5)of{LT->case compare(q,s')(18,4)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(18,9)of{LT->case compare(q,s')(18,8)of{LT->case compare(q,s')(18,7)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->case compare(q,s')(18,11)of{LT->case compare(q,s')(18,10)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->Nothing}}}}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(20,3)of{LT->case compare(q,s')(19,8)of{LT->case compare(q,s')(19,1)of{LT->case compare(q,s')(18,16)of{LT->case compare(q,s')(18,14)of{LT->case compare(q,s')(18,13)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing};EQ->Just(Reduce 1 8);GT->case compare(q,s')(18,15)of{LT->Nothing;EQ->Just(Reduce 1 8);GT->Nothing}};EQ->Just(Reduce 1 8);GT->case compare(q,s')(19,0)of{LT->case compare(q,s')(19,-1)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->Nothing}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(19,5)of{LT->case compare(q,s')(19,3)of{LT->case compare(q,s')(19,2)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->case compare(q,s')(19,4)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(19,7)of{LT->case compare(q,s')(19,6)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->Nothing}}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(19,15)of{LT->case compare(q,s')(19,12)of{LT->case compare(q,s')(19,10)of{LT->case compare(q,s')(19,9)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->case compare(q,s')(19,11)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(19,14)of{LT->case compare(q,s')(19,13)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 10);GT->Nothing}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(20,0)of{LT->case compare(q,s')(20,-1)of{LT->case compare(q,s')(19,16)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(20,2)of{LT->case compare(q,s')(20,1)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing}}}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(20,16)of{LT->case compare(q,s')(20,10)of{LT->case compare(q,s')(20,7)of{LT->case compare(q,s')(20,5)of{LT->case compare(q,s')(20,4)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(20,6)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(20,9)of{LT->case compare(q,s')(20,8)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(20,13)of{LT->case compare(q,s')(20,12)of{LT->case compare(q,s')(20,11)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->case compare(q,s')(20,15)of{LT->case compare(q,s')(20,14)of{LT->Nothing;EQ->Just(Reduce 1 9);GT->Nothing};EQ->Just(Reduce 1 9);GT->Nothing}}};EQ->Just(Reduce 1 9);GT->case compare(q,s')(21,5)of{LT->case compare(q,s')(21,2)of{LT->case compare(q,s')(21,0)of{LT->case compare(q,s')(21,-1)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 13);GT->case compare(q,s')(21,1)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing}};EQ->Just(Reduce 1 13);GT->case compare(q,s')(21,4)of{LT->case compare(q,s')(21,3)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 13);GT->Nothing}};EQ->Just(Reduce 1 13);GT->case compare(q,s')(21,8)of{LT->case compare(q,s')(21,7)of{LT->case compare(q,s')(21,6)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 13);GT->case compare(q,s')(21,10)of{LT->case compare(q,s')(21,9)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 13);GT->Nothing}}}}}};EQ->Just(Reduce 1 13);GT->case compare(q,s')(25,9)of{LT->case compare(q,s')(23,2)of{LT->case compare(q,s')(22,7)of{LT->case compare(q,s')(22,0)of{LT->case compare(q,s')(21,15)of{LT->case compare(q,s')(21,13)of{LT->case compare(q,s')(21,12)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 13);GT->case compare(q,s')(21,14)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing}};EQ->Just(Reduce 1 13);GT->case compare(q,s')(22,-1)of{LT->case compare(q,s')(21,16)of{LT->Nothing;EQ->Just(Reduce 1 13);GT->Nothing};EQ->Just(Reduce 1 14);GT->Nothing}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(22,4)of{LT->case compare(q,s')(22,2)of{LT->case compare(q,s')(22,1)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 14);GT->case compare(q,s')(22,3)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(22,6)of{LT->case compare(q,s')(22,5)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 14);GT->Nothing}}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(22,14)of{LT->case compare(q,s')(22,11)of{LT->case compare(q,s')(22,9)of{LT->case compare(q,s')(22,8)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 14);GT->case compare(q,s')(22,10)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(22,13)of{LT->case compare(q,s')(22,12)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 14);GT->Nothing}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(23,-1)of{LT->case compare(q,s')(22,16)of{LT->case compare(q,s')(22,15)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 15);GT->case compare(q,s')(23,1)of{LT->case compare(q,s')(23,0)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->Nothing}}}};EQ->Just(Reduce 1 15);GT->case compare(q,s')(23,16)of{LT->case compare(q,s')(23,9)of{LT->case compare(q,s')(23,6)of{LT->case compare(q,s')(23,4)of{LT->case compare(q,s')(23,3)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->case compare(q,s')(23,5)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing}};EQ->Just(Reduce 1 15);GT->case compare(q,s')(23,8)of{LT->case compare(q,s')(23,7)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->Nothing}};EQ->Just(Reduce 1 15);GT->case compare(q,s')(23,13)of{LT->case compare(q,s')(23,11)of{LT->case compare(q,s')(23,10)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->case compare(q,s')(23,12)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing}};EQ->Just(Reduce 1 15);GT->case compare(q,s')(23,15)of{LT->case compare(q,s')(23,14)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->Nothing}}};EQ->Just(Reduce 1 15);GT->case compare(q,s')(25,3)of{LT->case compare(q,s')(25,0)of{LT->case compare(q,s')(24,1)of{LT->case compare(q,s')(24,0)of{LT->Nothing;EQ->Just(Reduce 1 28);GT->Nothing};EQ->Just(Reduce 1 28);GT->case compare(q,s')(25,-1)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(25,2)of{LT->case compare(q,s')(25,1)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->Nothing}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(25,6)of{LT->case compare(q,s')(25,5)of{LT->case compare(q,s')(25,4)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->case compare(q,s')(25,8)of{LT->case compare(q,s')(25,7)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->Nothing}}}}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(27,0)of{LT->case compare(q,s')(26,5)of{LT->case compare(q,s')(25,16)of{LT->case compare(q,s')(25,13)of{LT->case compare(q,s')(25,11)of{LT->case compare(q,s')(25,10)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->case compare(q,s')(25,12)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(25,15)of{LT->case compare(q,s')(25,14)of{LT->Nothing;EQ->Just(Reduce 1 12);GT->Nothing};EQ->Just(Reduce 1 12);GT->Nothing}};EQ->Just(Reduce 1 12);GT->case compare(q,s')(26,2)of{LT->case compare(q,s')(26,0)of{LT->case compare(q,s')(26,-1)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(26,1)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing}};EQ->Just(Reduce 1 11);GT->case compare(q,s')(26,4)of{LT->case compare(q,s')(26,3)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->Nothing}}};EQ->Just(Reduce 1 11);GT->case compare(q,s')(26,12)of{LT->case compare(q,s')(26,9)of{LT->case compare(q,s')(26,7)of{LT->case compare(q,s')(26,6)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(26,8)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing}};EQ->Just(Reduce 1 11);GT->case compare(q,s')(26,11)of{LT->case compare(q,s')(26,10)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->Nothing}};EQ->Just(Reduce 1 11);GT->case compare(q,s')(26,15)of{LT->case compare(q,s')(26,14)of{LT->case compare(q,s')(26,13)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 1 11);GT->case compare(q,s')(27,-1)of{LT->case compare(q,s')(26,16)of{LT->Nothing;EQ->Just(Reduce 1 11);GT->Nothing};EQ->Just(Reduce 5 17);GT->Nothing}}}};EQ->Just(Reduce 5 17);GT->case compare(q,s')(27,13)of{LT->case compare(q,s')(27,7)of{LT->case compare(q,s')(27,4)of{LT->case compare(q,s')(27,2)of{LT->case compare(q,s')(27,1)of{LT->Nothing;EQ->Just(Reduce 5 17);GT->Nothing};EQ->Just(Reduce 5 17);GT->case compare(q,s')(27,3)of{LT->Nothing;EQ->Just(Reduce 5 17);GT->Nothing}};EQ->Just(Reduce 5 17);GT->case compare(q,s')(27,6)of{LT->case compare(q,s')(27,5)of{LT->Nothing;EQ->Just(Reduce 5 17);GT->Nothing};EQ->Just(Reduce 5 17);GT->Nothing}};EQ->Just(Reduce 5 17);GT->case compare(q,s')(27,10)of{LT->case compare(q,s')(27,9)of{LT->case compare(q,s')(27,8)of{LT->Nothing;EQ->Just(Reduce 5 17);GT->Nothing};EQ->Just(Reduce 5 17);GT->Nothing};EQ->Just(Reduce 5 17);GT->case compare(q,s')(27,12)of{LT->case compare(q,s')(27,11)of{LT->Nothing;EQ->Just(Reduce 5 17);GT->Nothing};EQ->Just(Reduce 5 17);GT->Nothing}}};EQ->Just(Reduce 5 17);GT->case compare(q,s')(28,2)of{LT->case compare(q,s')(28,-1)of{LT->case compare(q,s')(27,15)of{LT->case compare(q,s')(27,14)of{LT->Nothing;EQ->Just(Reduce 5 17);GT->Nothing};EQ->Just(Reduce 5 17);GT->case compare(q,s')(27,16)of{LT->Nothing;EQ->Just(Reduce 5 17);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(28,1)of{LT->case compare(q,s')(28,0)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(28,5)of{LT->case compare(q,s')(28,4)of{LT->case compare(q,s')(28,3)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->case compare(q,s')(28,7)of{LT->case compare(q,s')(28,6)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->Nothing}}}}}}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(35,-1)of{LT->case compare(q,s')(33,11)of{LT->case compare(q,s')(33,-1)of{LT->case compare(q,s')(28,15)of{LT->case compare(q,s')(28,12)of{LT->case compare(q,s')(28,10)of{LT->case compare(q,s')(28,9)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->case compare(q,s')(28,11)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(28,14)of{LT->case compare(q,s')(28,13)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Reduce 3 16);GT->Nothing}};EQ->Just(Reduce 3 16);GT->case compare(q,s')(31,11)of{LT->case compare(q,s')(29,10)of{LT->case compare(q,s')(28,16)of{LT->Nothing;EQ->Just(Reduce 3 16);GT->Nothing};EQ->Just(Shift 27);GT->case compare(q,s')(30,10)of{LT->Nothing;EQ->Just(Shift 28);GT->Nothing}};EQ->Just(Shift 7);GT->case compare(q,s')(32,9)of{LT->Nothing;EQ->Just(Shift 8);GT->Nothing}}};EQ->Just(Reduce 4 27);GT->case compare(q,s')(33,4)of{LT->case compare(q,s')(33,1)of{LT->case compare(q,s')(33,0)of{LT->Nothing;EQ->Just(Reduce 4 27);GT->Nothing};EQ->Just(Reduce 4 27);GT->case compare(q,s')(33,3)of{LT->case compare(q,s')(33,2)of{LT->Nothing;EQ->Just(Reduce 4 27);GT->Nothing};EQ->Just(Reduce 4 27);GT->Nothing}};EQ->Just(Reduce 4 27);GT->case compare(q,s')(33,8)of{LT->case compare(q,s')(33,6)of{LT->case compare(q,s')(33,5)of{LT->Nothing;EQ->Just(Reduce 4 27);GT->Nothing};EQ->Just(Reduce 4 27);GT->case compare(q,s')(33,7)of{LT->Nothing;EQ->Just(Reduce 4 27);GT->Nothing}};EQ->Just(Reduce 4 27);GT->case compare(q,s')(33,10)of{LT->case compare(q,s')(33,9)of{LT->Nothing;EQ->Just(Reduce 4 27);GT->Nothing};EQ->Just(Reduce 4 27);GT->Nothing}}}};EQ->Just(Reduce 4 27);GT->case compare(q,s')(33,16)of{LT->case compare(q,s')(33,14)of{LT->case compare(q,s')(33,13)of{LT->case compare(q,s')(33,12)of{LT->Nothing;EQ->Just(Reduce 4 27);GT->Nothing};EQ->Just(Reduce 4 27);GT->Nothing};EQ->Just(Reduce 4 27);GT->case compare(q,s')(33,15)of{LT->Nothing;EQ->Just(Reduce 4 27);GT->Nothing}};EQ->Just(Reduce 4 27);GT->case compare(q,s')(34,10)of{LT->Nothing;EQ->Just(Shift 33);GT->Nothing}}};EQ->Just(Reduce 2 22);GT->case compare(q,s')(38,7)of{LT->case compare(q,s')(35,13)of{LT->case compare(q,s')(35,6)of{LT->case compare(q,s')(35,3)of{LT->case compare(q,s')(35,1)of{LT->case compare(q,s')(35,0)of{LT->Nothing;EQ->Just(Reduce 2 22);GT->Nothing};EQ->Just(Reduce 2 22);GT->case compare(q,s')(35,2)of{LT->Nothing;EQ->Just(Reduce 2 22);GT->Nothing}};EQ->Just(Reduce 2 22);GT->case compare(q,s')(35,5)of{LT->case compare(q,s')(35,4)of{LT->Nothing;EQ->Just(Reduce 2 22);GT->Nothing};EQ->Just(Reduce 2 22);GT->Nothing}};EQ->Just(Reduce 2 22);GT->case compare(q,s')(35,10)of{LT->case compare(q,s')(35,8)of{LT->case compare(q,s')(35,7)of{LT->Nothing;EQ->Just(Reduce 2 22);GT->Nothing};EQ->Just(Reduce 2 22);GT->case compare(q,s')(35,9)of{LT->Nothing;EQ->Just(Reduce 2 22);GT->Nothing}};EQ->Just(Reduce 2 22);GT->case compare(q,s')(35,12)of{LT->case compare(q,s')(35,11)of{LT->Nothing;EQ->Just(Reduce 2 22);GT->Nothing};EQ->Just(Reduce 2 22);GT->Nothing}}};EQ->Just(Reduce 2 22);GT->case compare(q,s')(38,1)of{LT->case compare(q,s')(36,10)of{LT->case compare(q,s')(35,15)of{LT->case compare(q,s')(35,14)of{LT->Nothing;EQ->Just(Reduce 2 22);GT->Nothing};EQ->Just(Reduce 2 22);GT->case compare(q,s')(35,16)of{LT->Nothing;EQ->Just(Reduce 2 22);GT->Nothing}};EQ->Just(Reduce 2 19);GT->case compare(q,s')(38,0)of{LT->case compare(q,s')(37,11)of{LT->Nothing;EQ->Just(Reduce 2 21);GT->Nothing};EQ->Just(Reduce 1 23);GT->Nothing}};EQ->Just(Reduce 1 23);GT->case compare(q,s')(38,4)of{LT->case compare(q,s')(38,3)of{LT->case compare(q,s')(38,2)of{LT->Nothing;EQ->Just(Reduce 1 23);GT->Nothing};EQ->Just(Reduce 1 23);GT->Nothing};EQ->Just(Reduce 1 23);GT->case compare(q,s')(38,6)of{LT->case compare(q,s')(38,5)of{LT->Nothing;EQ->Just(Reduce 1 23);GT->Nothing};EQ->Just(Reduce 1 23);GT->Nothing}}}};EQ->Just(Reduce 1 23);GT->case compare(q,s')(40,4)of{LT->case compare(q,s')(39,6)of{LT->case compare(q,s')(38,16)of{LT->case compare(q,s')(38,13)of{LT->case compare(q,s')(38,9)of{LT->case compare(q,s')(38,8)of{LT->Nothing;EQ->Just(Reduce 1 23);GT->Nothing};EQ->Just(Reduce 1 23);GT->case compare(q,s')(38,12)of{LT->Nothing;EQ->Just(Reduce 1 23);GT->Nothing}};EQ->Just(Reduce 1 23);GT->case compare(q,s')(38,15)of{LT->case compare(q,s')(38,14)of{LT->Nothing;EQ->Just(Reduce 1 23);GT->Nothing};EQ->Just(Reduce 1 23);GT->Nothing}};EQ->Just(Reduce 1 23);GT->case compare(q,s')(39,3)of{LT->case compare(q,s')(39,1)of{LT->case compare(q,s')(39,0)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing};EQ->Just(Reduce 1 24);GT->case compare(q,s')(39,2)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing}};EQ->Just(Reduce 1 24);GT->case compare(q,s')(39,5)of{LT->case compare(q,s')(39,4)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing};EQ->Just(Reduce 1 24);GT->Nothing}}};EQ->Just(Reduce 1 24);GT->case compare(q,s')(39,15)of{LT->case compare(q,s')(39,12)of{LT->case compare(q,s')(39,8)of{LT->case compare(q,s')(39,7)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing};EQ->Just(Reduce 1 24);GT->case compare(q,s')(39,9)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing}};EQ->Just(Reduce 1 24);GT->case compare(q,s')(39,14)of{LT->case compare(q,s')(39,13)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing};EQ->Just(Reduce 1 24);GT->Nothing}};EQ->Just(Reduce 1 24);GT->case compare(q,s')(40,1)of{LT->case compare(q,s')(40,0)of{LT->case compare(q,s')(39,16)of{LT->Nothing;EQ->Just(Reduce 1 24);GT->Nothing};EQ->Just(Reduce 1 25);GT->Nothing};EQ->Just(Reduce 1 25);GT->case compare(q,s')(40,3)of{LT->case compare(q,s')(40,2)of{LT->Nothing;EQ->Just(Reduce 1 25);GT->Nothing};EQ->Just(Reduce 1 25);GT->Nothing}}}};EQ->Just(Reduce 1 25);GT->case compare(q,s')(41,2)of{LT->case compare(q,s')(40,13)of{LT->case compare(q,s')(40,8)of{LT->case compare(q,s')(40,6)of{LT->case compare(q,s')(40,5)of{LT->Nothing;EQ->Just(Reduce 1 25);GT->Nothing};EQ->Just(Reduce 1 25);GT->case compare(q,s')(40,7)of{LT->Nothing;EQ->Just(Reduce 1 25);GT->Nothing}};EQ->Just(Reduce 1 25);GT->case compare(q,s')(40,12)of{LT->case compare(q,s')(40,9)of{LT->Nothing;EQ->Just(Reduce 1 25);GT->Nothing};EQ->Just(Reduce 1 25);GT->Nothing}};EQ->Just(Reduce 1 25);GT->case compare(q,s')(40,16)of{LT->case compare(q,s')(40,15)of{LT->case compare(q,s')(40,14)of{LT->Nothing;EQ->Just(Reduce 1 25);GT->Nothing};EQ->Just(Reduce 1 25);GT->Nothing};EQ->Just(Reduce 1 25);GT->case compare(q,s')(41,1)of{LT->case compare(q,s')(41,0)of{LT->Nothing;EQ->Just(Reduce 1 26);GT->Nothing};EQ->Just(Reduce 1 26);GT->Nothing}}};EQ->Just(Reduce 1 26);GT->case compare(q,s')(41,9)of{LT->case compare(q,s')(41,6)of{LT->case compare(q,s')(41,4)of{LT->case compare(q,s')(41,3)of{LT->Nothing;EQ->Just(Reduce 1 26);GT->Nothing};EQ->Just(Reduce 1 26);GT->case compare(q,s')(41,5)of{LT->Nothing;EQ->Just(Reduce 1 26);GT->Nothing}};EQ->Just(Reduce 1 26);GT->case compare(q,s')(41,8)of{LT->case compare(q,s')(41,7)of{LT->Nothing;EQ->Just(Reduce 1 26);GT->Nothing};EQ->Just(Reduce 1 26);GT->Nothing}};EQ->Just(Reduce 1 26);GT->case compare(q,s')(41,14)of{LT->case compare(q,s')(41,13)of{LT->case compare(q,s')(41,12)of{LT->Nothing;EQ->Just(Reduce 1 26);GT->Nothing};EQ->Just(Reduce 1 26);GT->Nothing};EQ->Just(Reduce 1 26);GT->case compare(q,s')(41,16)of{LT->case compare(q,s')(41,15)of{LT->Nothing;EQ->Just(Reduce 1 26);GT->Nothing};EQ->Just(Reduce 1 26);GT->Nothing}}}}}}}}}

production :: Int -> Int
production 0 = 0
production 1 = 0
production 2 = 0
production 3 = 0
production 4 = 0
production 5 = 1
production 6 = 1
production 7 = 1
production 8 = 1
production 9 = 1
production 10 = 1
production 11 = 4
production 12 = 4
production 13 = 2
production 14 = 2
production 15 = 2
production 16 = 5
production 17 = 5
production 18 = 8
production 19 = 8
production 20 = 9
production 21 = 9
production 22 = 7
production 23 = 10
production 24 = 10
production 25 = 10
production 26 = 10
production 27 = 6
production 28 = 3

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  let s' = production s in
    case compare(q,s')(5,2)of{LT->case compare(q,s')(3,1)of{LT->case compare(q,s')(2,0)of{LT->case compare(q,s')(0,4)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just 1;GT->Nothing};EQ->Just 12;GT->Nothing};EQ->Just 13;GT->case compare(q,s')(0,3)of{LT->Nothing;EQ->Just 14;GT->Nothing}};EQ->Just 20;GT->case compare(q,s')(0,7)of{LT->case compare(q,s')(0,6)of{LT->case compare(q,s')(0,5)of{LT->Nothing;EQ->Just 21;GT->Nothing};EQ->Just 22;GT->Nothing};EQ->Just 23;GT->case compare(q,s')(0,10)of{LT->Nothing;EQ->Just 4;GT->Nothing}}};EQ->Just 10;GT->case compare(q,s')(2,5)of{LT->case compare(q,s')(2,3)of{LT->case compare(q,s')(2,2)of{LT->case compare(q,s')(2,1)of{LT->Nothing;EQ->Just 12;GT->Nothing};EQ->Just 13;GT->Nothing};EQ->Just 14;GT->case compare(q,s')(2,4)of{LT->Nothing;EQ->Just 20;GT->Nothing}};EQ->Just 21;GT->case compare(q,s')(2,10)of{LT->case compare(q,s')(2,7)of{LT->case compare(q,s')(2,6)of{LT->Nothing;EQ->Just 22;GT->Nothing};EQ->Just 23;GT->Nothing};EQ->Just 4;GT->case compare(q,s')(3,0)of{LT->Nothing;EQ->Just 11;GT->Nothing}}}};EQ->Just 12;GT->case compare(q,s')(4,2)of{LT->case compare(q,s')(3,6)of{LT->case compare(q,s')(3,4)of{LT->case compare(q,s')(3,3)of{LT->case compare(q,s')(3,2)of{LT->Nothing;EQ->Just 13;GT->Nothing};EQ->Just 14;GT->Nothing};EQ->Just 20;GT->case compare(q,s')(3,5)of{LT->Nothing;EQ->Just 21;GT->Nothing}};EQ->Just 22;GT->case compare(q,s')(4,0)of{LT->case compare(q,s')(3,10)of{LT->case compare(q,s')(3,7)of{LT->Nothing;EQ->Just 23;GT->Nothing};EQ->Just 4;GT->Nothing};EQ->Just 35;GT->case compare(q,s')(4,1)of{LT->Nothing;EQ->Just 12;GT->Nothing}}};EQ->Just 13;GT->case compare(q,s')(4,7)of{LT->case compare(q,s')(4,5)of{LT->case compare(q,s')(4,4)of{LT->case compare(q,s')(4,3)of{LT->Nothing;EQ->Just 14;GT->Nothing};EQ->Just 20;GT->Nothing};EQ->Just 21;GT->case compare(q,s')(4,6)of{LT->Nothing;EQ->Just 22;GT->Nothing}};EQ->Just 23;GT->case compare(q,s')(5,0)of{LT->case compare(q,s')(4,10)of{LT->Nothing;EQ->Just 4;GT->Nothing};EQ->Just 6;GT->case compare(q,s')(5,1)of{LT->Nothing;EQ->Just 12;GT->Nothing}}}}};EQ->Just 13;GT->case compare(q,s')(6,10)of{LT->case compare(q,s')(6,1)of{LT->case compare(q,s')(5,7)of{LT->case compare(q,s')(5,5)of{LT->case compare(q,s')(5,4)of{LT->case compare(q,s')(5,3)of{LT->Nothing;EQ->Just 14;GT->Nothing};EQ->Just 20;GT->Nothing};EQ->Just 21;GT->case compare(q,s')(5,6)of{LT->Nothing;EQ->Just 22;GT->Nothing}};EQ->Just 23;GT->case compare(q,s')(5,10)of{LT->case compare(q,s')(5,9)of{LT->case compare(q,s')(5,8)of{LT->Nothing;EQ->Just 30;GT->Nothing};EQ->Just 31;GT->Nothing};EQ->Just 4;GT->case compare(q,s')(6,0)of{LT->Nothing;EQ->Just 6;GT->Nothing}}};EQ->Just 12;GT->case compare(q,s')(6,6)of{LT->case compare(q,s')(6,4)of{LT->case compare(q,s')(6,3)of{LT->case compare(q,s')(6,2)of{LT->Nothing;EQ->Just 13;GT->Nothing};EQ->Just 14;GT->Nothing};EQ->Just 20;GT->case compare(q,s')(6,5)of{LT->Nothing;EQ->Just 21;GT->Nothing}};EQ->Just 22;GT->case compare(q,s')(6,8)of{LT->case compare(q,s')(6,7)of{LT->Nothing;EQ->Just 23;GT->Nothing};EQ->Just 36;GT->case compare(q,s')(6,9)of{LT->Nothing;EQ->Just 37;GT->Nothing}}}};EQ->Just 4;GT->case compare(q,s')(8,0)of{LT->case compare(q,s')(7,4)of{LT->case compare(q,s')(7,2)of{LT->case compare(q,s')(7,1)of{LT->case compare(q,s')(7,0)of{LT->Nothing;EQ->Just 29;GT->Nothing};EQ->Just 12;GT->Nothing};EQ->Just 13;GT->case compare(q,s')(7,3)of{LT->Nothing;EQ->Just 14;GT->Nothing}};EQ->Just 20;GT->case compare(q,s')(7,7)of{LT->case compare(q,s')(7,6)of{LT->case compare(q,s')(7,5)of{LT->Nothing;EQ->Just 21;GT->Nothing};EQ->Just 22;GT->Nothing};EQ->Just 23;GT->case compare(q,s')(7,10)of{LT->Nothing;EQ->Just 4;GT->Nothing}}};EQ->Just 34;GT->case compare(q,s')(8,5)of{LT->case compare(q,s')(8,3)of{LT->case compare(q,s')(8,2)of{LT->case compare(q,s')(8,1)of{LT->Nothing;EQ->Just 12;GT->Nothing};EQ->Just 13;GT->Nothing};EQ->Just 14;GT->case compare(q,s')(8,4)of{LT->Nothing;EQ->Just 20;GT->Nothing}};EQ->Just 21;GT->case compare(q,s')(8,7)of{LT->case compare(q,s')(8,6)of{LT->Nothing;EQ->Just 22;GT->Nothing};EQ->Just 23;GT->case compare(q,s')(8,10)of{LT->Nothing;EQ->Just 4;GT->Nothing}}}}}}

parse :: Monad m => SemanticActions m -> [Token] -> m (Either (Maybe Token) (Datum, [Token]))
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
                  Token (EQUAL semanticValue) ->
                    StackValue_EQUAL semanticValue
                  Token (SHARP semanticValue) ->
                    StackValue_SHARP semanticValue
                  Token (SHARP_SEMICOLON semanticValue) ->
                    StackValue_SHARP_SEMICOLON semanticValue
                  Token (BOOLEAN semanticValue) ->
                    StackValue_BOOLEAN semanticValue
                  Token (NUMBER semanticValue) ->
                    StackValue_NUMBER semanticValue
                  Token (CHARACTER semanticValue) ->
                    StackValue_CHARACTER semanticValue
                  Token (STRING semanticValue) ->
                    StackValue_STRING semanticValue
                  Token (BYTEVECTOR semanticValue) ->
                    StackValue_BYTEVECTOR semanticValue
                  Token (IDENTIFIER semanticValue) ->
                    StackValue_IDENTIFIER semanticValue
                  Token (LPAREN semanticValue) ->
                    StackValue_LPAREN semanticValue
                  Token (RPAREN semanticValue) ->
                    StackValue_RPAREN semanticValue
                  Token (DOT semanticValue) ->
                    StackValue_DOT semanticValue
                  Token (QUOTE semanticValue) ->
                    StackValue_QUOTE semanticValue
                  Token (BACKQUOTE semanticValue) ->
                    StackValue_BACKQUOTE semanticValue
                  Token (COMMA semanticValue) ->
                    StackValue_COMMA semanticValue
                  Token (COMMAAT semanticValue) ->
                    StackValue_COMMAAT semanticValue
                  Token (UINTEGER10 semanticValue) ->
                    StackValue_UINTEGER10 semanticValue
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
                      Monad.liftM StackValue_datum $ datum_implies_simpleDatum actions (case snd (pop !! 0) of { StackValue_simpleDatum value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_datum $ datum_implies_compoundDatum actions (case snd (pop !! 0) of { StackValue_compoundDatum value -> value; _ -> undefined })
                    2 ->
                      Monad.liftM StackValue_datum $ datum_implies_label_EQUAL_datum actions (case snd (pop !! 2) of { StackValue_label value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_datum value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_datum $ datum_implies_label_SHARP actions (case snd (pop !! 1) of { StackValue_label value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_SHARP value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_datum $ datum_implies_SHARP_SEMICOLON_datum actions (case snd (pop !! 1) of { StackValue_SHARP_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_datum value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_simpleDatum $ simpleDatum_implies_BOOLEAN actions (case snd (pop !! 0) of { StackValue_BOOLEAN value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_simpleDatum $ simpleDatum_implies_NUMBER actions (case snd (pop !! 0) of { StackValue_NUMBER value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_simpleDatum $ simpleDatum_implies_CHARACTER actions (case snd (pop !! 0) of { StackValue_CHARACTER value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_simpleDatum $ simpleDatum_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_simpleDatum $ simpleDatum_implies_symbol actions (case snd (pop !! 0) of { StackValue_symbol value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_simpleDatum $ simpleDatum_implies_BYTEVECTOR actions (case snd (pop !! 0) of { StackValue_BYTEVECTOR value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_symbol $ symbol_implies_IDENTIFIER actions (case snd (pop !! 0) of { StackValue_IDENTIFIER value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_symbol $ symbol_implies_EQUAL actions (case snd (pop !! 0) of { StackValue_EQUAL value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_compoundDatum $ compoundDatum_implies_list actions (case snd (pop !! 0) of { StackValue_list value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_compoundDatum $ compoundDatum_implies_vector actions (case snd (pop !! 0) of { StackValue_vector value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_compoundDatum $ compoundDatum_implies_abbreviation actions (case snd (pop !! 0) of { StackValue_abbreviation value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_list $ list_implies_LPAREN_data0_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_data0 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_list $ list_implies_LPAREN_data1_DOT_datum_RPAREN actions (case snd (pop !! 4) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_data1 value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DOT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    18 ->
                      Monad.liftM StackValue_data0 $ data0_implies actions
                    19 ->
                      Monad.liftM StackValue_data0 $ data0_implies_datum_data0 actions (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_data0 value -> value; _ -> undefined })
                    20 ->
                      Monad.liftM StackValue_data1 $ data1_implies_datum actions (case snd (pop !! 0) of { StackValue_datum value -> value; _ -> undefined })
                    21 ->
                      Monad.liftM StackValue_data1 $ data1_implies_datum_data1 actions (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_data1 value -> value; _ -> undefined })
                    22 ->
                      Monad.liftM StackValue_abbreviation $ abbreviation_implies_abbrevPrefix_datum actions (case snd (pop !! 1) of { StackValue_abbrevPrefix value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_datum value -> value; _ -> undefined })
                    23 ->
                      Monad.liftM StackValue_abbrevPrefix $ abbrevPrefix_implies_QUOTE actions (case snd (pop !! 0) of { StackValue_QUOTE value -> value; _ -> undefined })
                    24 ->
                      Monad.liftM StackValue_abbrevPrefix $ abbrevPrefix_implies_BACKQUOTE actions (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    25 ->
                      Monad.liftM StackValue_abbrevPrefix $ abbrevPrefix_implies_COMMA actions (case snd (pop !! 0) of { StackValue_COMMA value -> value; _ -> undefined })
                    26 ->
                      Monad.liftM StackValue_abbrevPrefix $ abbrevPrefix_implies_COMMAAT actions (case snd (pop !! 0) of { StackValue_COMMAAT value -> value; _ -> undefined })
                    27 ->
                      Monad.liftM StackValue_vector $ vector_implies_SHARP_LPAREN_datum_RPAREN actions (case snd (pop !! 3) of { StackValue_SHARP value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_datum value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    28 ->
                      Monad.liftM StackValue_label $ label_implies_UINTEGER10 actions (case snd (pop !! 0) of { StackValue_UINTEGER10 value -> value; _ -> undefined })
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_datum value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { datum_implies_simpleDatum =
      return
  , datum_implies_compoundDatum =
      return
  , datum_implies_label_EQUAL_datum = \label () datum ->
      return $ Labeling label datum
  , datum_implies_label_SHARP = \label () ->
      return $ LabelRef label
  , datum_implies_SHARP_SEMICOLON_datum = \() datum ->
      return $ ExprComment datum
  , simpleDatum_implies_BOOLEAN =
      return . Bool
  , simpleDatum_implies_NUMBER =
      return . Number
  , simpleDatum_implies_CHARACTER =
      return . Character
  , simpleDatum_implies_STRING =
      return . String
  , simpleDatum_implies_symbol =
      return . Symbol
  , simpleDatum_implies_BYTEVECTOR =
      return . ByteVector
  , symbol_implies_IDENTIFIER =
      return
  , symbol_implies_EQUAL =
      const $ return "="
  , compoundDatum_implies_list =
      return
  , compoundDatum_implies_vector =
      return
  , compoundDatum_implies_abbreviation =
      return
  , list_implies_LPAREN_data0_RPAREN = \() data0 () ->
      return $ List0 data0
  , list_implies_LPAREN_data1_DOT_datum_RPAREN = \() data1 () datum () ->
      return $ List1 (data1, datum)
  , data0_implies =
      return []
  , data0_implies_datum_data0 = \datum data0 ->
      return $ datum : data0
  , data1_implies_datum = \datum ->
      return [datum]
  , data1_implies_datum_data1 = \datum data1 ->
      return $ datum : data1
  , abbreviation_implies_abbrevPrefix_datum = \prefix datum ->
      case prefix of
        APQuote -> return $ Quote datum
        APBackquote -> return $ Backquote datum
        APComma -> return $ Comma datum
        APCommaAt -> return $ CommaAt datum
  , abbrevPrefix_implies_QUOTE =
      const $ return APQuote
  , abbrevPrefix_implies_BACKQUOTE =
      const $ return APBackquote
  , abbrevPrefix_implies_COMMA =
      const $ return APComma
  , abbrevPrefix_implies_COMMAAT =
      const $ return APCommaAt
  , vector_implies_SHARP_LPAREN_datum_RPAREN = \() () datum () ->
      return $ Vector datum
  , label_implies_UINTEGER10 =
      return }


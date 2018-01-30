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
  case (q, s) of
    (0, Token (EQUAL _)) -> Just (Shift 25)
    (0, Token (SHARP _)) -> Just (Shift 32)
    (0, Token (SHARP_SEMICOLON _)) -> Just (Shift 3)
    (0, Token (BOOLEAN _)) -> Just (Shift 15)
    (0, Token (NUMBER _)) -> Just (Shift 16)
    (0, Token (CHARACTER _)) -> Just (Shift 17)
    (0, Token (STRING _)) -> Just (Shift 18)
    (0, Token (BYTEVECTOR _)) -> Just (Shift 19)
    (0, Token (IDENTIFIER _)) -> Just (Shift 26)
    (0, Token (LPAREN _)) -> Just (Shift 5)
    (0, Token (QUOTE _)) -> Just (Shift 38)
    (0, Token (BACKQUOTE _)) -> Just (Shift 39)
    (0, Token (COMMA _)) -> Just (Shift 40)
    (0, Token (COMMAAT _)) -> Just (Shift 41)
    (0, Token (UINTEGER10 _)) -> Just (Shift 24)
    (1, EOF) -> Just (Accept)
    (2, Token (EQUAL _)) -> Just (Shift 25)
    (2, Token (SHARP _)) -> Just (Shift 32)
    (2, Token (SHARP_SEMICOLON _)) -> Just (Shift 3)
    (2, Token (BOOLEAN _)) -> Just (Shift 15)
    (2, Token (NUMBER _)) -> Just (Shift 16)
    (2, Token (CHARACTER _)) -> Just (Shift 17)
    (2, Token (STRING _)) -> Just (Shift 18)
    (2, Token (BYTEVECTOR _)) -> Just (Shift 19)
    (2, Token (IDENTIFIER _)) -> Just (Shift 26)
    (2, Token (LPAREN _)) -> Just (Shift 5)
    (2, Token (QUOTE _)) -> Just (Shift 38)
    (2, Token (BACKQUOTE _)) -> Just (Shift 39)
    (2, Token (COMMA _)) -> Just (Shift 40)
    (2, Token (COMMAAT _)) -> Just (Shift 41)
    (2, Token (UINTEGER10 _)) -> Just (Shift 24)
    (3, Token (EQUAL _)) -> Just (Shift 25)
    (3, Token (SHARP _)) -> Just (Shift 32)
    (3, Token (SHARP_SEMICOLON _)) -> Just (Shift 3)
    (3, Token (BOOLEAN _)) -> Just (Shift 15)
    (3, Token (NUMBER _)) -> Just (Shift 16)
    (3, Token (CHARACTER _)) -> Just (Shift 17)
    (3, Token (STRING _)) -> Just (Shift 18)
    (3, Token (BYTEVECTOR _)) -> Just (Shift 19)
    (3, Token (IDENTIFIER _)) -> Just (Shift 26)
    (3, Token (LPAREN _)) -> Just (Shift 5)
    (3, Token (QUOTE _)) -> Just (Shift 38)
    (3, Token (BACKQUOTE _)) -> Just (Shift 39)
    (3, Token (COMMA _)) -> Just (Shift 40)
    (3, Token (COMMAAT _)) -> Just (Shift 41)
    (3, Token (UINTEGER10 _)) -> Just (Shift 24)
    (4, Token (EQUAL _)) -> Just (Shift 25)
    (4, Token (SHARP _)) -> Just (Shift 32)
    (4, Token (SHARP_SEMICOLON _)) -> Just (Shift 3)
    (4, Token (BOOLEAN _)) -> Just (Shift 15)
    (4, Token (NUMBER _)) -> Just (Shift 16)
    (4, Token (CHARACTER _)) -> Just (Shift 17)
    (4, Token (STRING _)) -> Just (Shift 18)
    (4, Token (BYTEVECTOR _)) -> Just (Shift 19)
    (4, Token (IDENTIFIER _)) -> Just (Shift 26)
    (4, Token (LPAREN _)) -> Just (Shift 5)
    (4, Token (QUOTE _)) -> Just (Shift 38)
    (4, Token (BACKQUOTE _)) -> Just (Shift 39)
    (4, Token (COMMA _)) -> Just (Shift 40)
    (4, Token (COMMAAT _)) -> Just (Shift 41)
    (4, Token (UINTEGER10 _)) -> Just (Shift 24)
    (5, Token (EQUAL _)) -> Just (Shift 25)
    (5, Token (SHARP _)) -> Just (Shift 32)
    (5, Token (SHARP_SEMICOLON _)) -> Just (Shift 3)
    (5, Token (BOOLEAN _)) -> Just (Shift 15)
    (5, Token (NUMBER _)) -> Just (Shift 16)
    (5, Token (CHARACTER _)) -> Just (Shift 17)
    (5, Token (STRING _)) -> Just (Shift 18)
    (5, Token (BYTEVECTOR _)) -> Just (Shift 19)
    (5, Token (IDENTIFIER _)) -> Just (Shift 26)
    (5, Token (LPAREN _)) -> Just (Shift 5)
    (5, Token (RPAREN _)) -> Just (Reduce 0 18)
    (5, Token (QUOTE _)) -> Just (Shift 38)
    (5, Token (BACKQUOTE _)) -> Just (Shift 39)
    (5, Token (COMMA _)) -> Just (Shift 40)
    (5, Token (COMMAAT _)) -> Just (Shift 41)
    (5, Token (UINTEGER10 _)) -> Just (Shift 24)
    (6, Token (EQUAL _)) -> Just (Shift 25)
    (6, Token (SHARP _)) -> Just (Shift 32)
    (6, Token (SHARP_SEMICOLON _)) -> Just (Shift 3)
    (6, Token (BOOLEAN _)) -> Just (Shift 15)
    (6, Token (NUMBER _)) -> Just (Shift 16)
    (6, Token (CHARACTER _)) -> Just (Shift 17)
    (6, Token (STRING _)) -> Just (Shift 18)
    (6, Token (BYTEVECTOR _)) -> Just (Shift 19)
    (6, Token (IDENTIFIER _)) -> Just (Shift 26)
    (6, Token (LPAREN _)) -> Just (Shift 5)
    (6, Token (RPAREN _)) -> Just (Reduce 0 18)
    (6, Token (DOT _)) -> Just (Reduce 1 20)
    (6, Token (QUOTE _)) -> Just (Shift 38)
    (6, Token (BACKQUOTE _)) -> Just (Shift 39)
    (6, Token (COMMA _)) -> Just (Shift 40)
    (6, Token (COMMAAT _)) -> Just (Shift 41)
    (6, Token (UINTEGER10 _)) -> Just (Shift 24)
    (7, Token (EQUAL _)) -> Just (Shift 25)
    (7, Token (SHARP _)) -> Just (Shift 32)
    (7, Token (SHARP_SEMICOLON _)) -> Just (Shift 3)
    (7, Token (BOOLEAN _)) -> Just (Shift 15)
    (7, Token (NUMBER _)) -> Just (Shift 16)
    (7, Token (CHARACTER _)) -> Just (Shift 17)
    (7, Token (STRING _)) -> Just (Shift 18)
    (7, Token (BYTEVECTOR _)) -> Just (Shift 19)
    (7, Token (IDENTIFIER _)) -> Just (Shift 26)
    (7, Token (LPAREN _)) -> Just (Shift 5)
    (7, Token (QUOTE _)) -> Just (Shift 38)
    (7, Token (BACKQUOTE _)) -> Just (Shift 39)
    (7, Token (COMMA _)) -> Just (Shift 40)
    (7, Token (COMMAAT _)) -> Just (Shift 41)
    (7, Token (UINTEGER10 _)) -> Just (Shift 24)
    (8, Token (EQUAL _)) -> Just (Shift 25)
    (8, Token (SHARP _)) -> Just (Shift 32)
    (8, Token (SHARP_SEMICOLON _)) -> Just (Shift 3)
    (8, Token (BOOLEAN _)) -> Just (Shift 15)
    (8, Token (NUMBER _)) -> Just (Shift 16)
    (8, Token (CHARACTER _)) -> Just (Shift 17)
    (8, Token (STRING _)) -> Just (Shift 18)
    (8, Token (BYTEVECTOR _)) -> Just (Shift 19)
    (8, Token (IDENTIFIER _)) -> Just (Shift 26)
    (8, Token (LPAREN _)) -> Just (Shift 5)
    (8, Token (QUOTE _)) -> Just (Shift 38)
    (8, Token (BACKQUOTE _)) -> Just (Shift 39)
    (8, Token (COMMA _)) -> Just (Shift 40)
    (8, Token (COMMAAT _)) -> Just (Shift 41)
    (8, Token (UINTEGER10 _)) -> Just (Shift 24)
    (9, EOF) -> Just (Reduce 2 3)
    (9, Token (EQUAL _)) -> Just (Reduce 2 3)
    (9, Token (SHARP _)) -> Just (Reduce 2 3)
    (9, Token (SHARP_SEMICOLON _)) -> Just (Reduce 2 3)
    (9, Token (BOOLEAN _)) -> Just (Reduce 2 3)
    (9, Token (NUMBER _)) -> Just (Reduce 2 3)
    (9, Token (CHARACTER _)) -> Just (Reduce 2 3)
    (9, Token (STRING _)) -> Just (Reduce 2 3)
    (9, Token (BYTEVECTOR _)) -> Just (Reduce 2 3)
    (9, Token (IDENTIFIER _)) -> Just (Reduce 2 3)
    (9, Token (LPAREN _)) -> Just (Reduce 2 3)
    (9, Token (RPAREN _)) -> Just (Reduce 2 3)
    (9, Token (DOT _)) -> Just (Reduce 2 3)
    (9, Token (QUOTE _)) -> Just (Reduce 2 3)
    (9, Token (BACKQUOTE _)) -> Just (Reduce 2 3)
    (9, Token (COMMA _)) -> Just (Reduce 2 3)
    (9, Token (COMMAAT _)) -> Just (Reduce 2 3)
    (9, Token (UINTEGER10 _)) -> Just (Reduce 2 3)
    (10, EOF) -> Just (Reduce 3 2)
    (10, Token (EQUAL _)) -> Just (Reduce 3 2)
    (10, Token (SHARP _)) -> Just (Reduce 3 2)
    (10, Token (SHARP_SEMICOLON _)) -> Just (Reduce 3 2)
    (10, Token (BOOLEAN _)) -> Just (Reduce 3 2)
    (10, Token (NUMBER _)) -> Just (Reduce 3 2)
    (10, Token (CHARACTER _)) -> Just (Reduce 3 2)
    (10, Token (STRING _)) -> Just (Reduce 3 2)
    (10, Token (BYTEVECTOR _)) -> Just (Reduce 3 2)
    (10, Token (IDENTIFIER _)) -> Just (Reduce 3 2)
    (10, Token (LPAREN _)) -> Just (Reduce 3 2)
    (10, Token (RPAREN _)) -> Just (Reduce 3 2)
    (10, Token (DOT _)) -> Just (Reduce 3 2)
    (10, Token (QUOTE _)) -> Just (Reduce 3 2)
    (10, Token (BACKQUOTE _)) -> Just (Reduce 3 2)
    (10, Token (COMMA _)) -> Just (Reduce 3 2)
    (10, Token (COMMAAT _)) -> Just (Reduce 3 2)
    (10, Token (UINTEGER10 _)) -> Just (Reduce 3 2)
    (11, EOF) -> Just (Reduce 2 4)
    (11, Token (EQUAL _)) -> Just (Reduce 2 4)
    (11, Token (SHARP _)) -> Just (Reduce 2 4)
    (11, Token (SHARP_SEMICOLON _)) -> Just (Reduce 2 4)
    (11, Token (BOOLEAN _)) -> Just (Reduce 2 4)
    (11, Token (NUMBER _)) -> Just (Reduce 2 4)
    (11, Token (CHARACTER _)) -> Just (Reduce 2 4)
    (11, Token (STRING _)) -> Just (Reduce 2 4)
    (11, Token (BYTEVECTOR _)) -> Just (Reduce 2 4)
    (11, Token (IDENTIFIER _)) -> Just (Reduce 2 4)
    (11, Token (LPAREN _)) -> Just (Reduce 2 4)
    (11, Token (RPAREN _)) -> Just (Reduce 2 4)
    (11, Token (DOT _)) -> Just (Reduce 2 4)
    (11, Token (QUOTE _)) -> Just (Reduce 2 4)
    (11, Token (BACKQUOTE _)) -> Just (Reduce 2 4)
    (11, Token (COMMA _)) -> Just (Reduce 2 4)
    (11, Token (COMMAAT _)) -> Just (Reduce 2 4)
    (11, Token (UINTEGER10 _)) -> Just (Reduce 2 4)
    (12, EOF) -> Just (Reduce 1 0)
    (12, Token (EQUAL _)) -> Just (Reduce 1 0)
    (12, Token (SHARP _)) -> Just (Reduce 1 0)
    (12, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 0)
    (12, Token (BOOLEAN _)) -> Just (Reduce 1 0)
    (12, Token (NUMBER _)) -> Just (Reduce 1 0)
    (12, Token (CHARACTER _)) -> Just (Reduce 1 0)
    (12, Token (STRING _)) -> Just (Reduce 1 0)
    (12, Token (BYTEVECTOR _)) -> Just (Reduce 1 0)
    (12, Token (IDENTIFIER _)) -> Just (Reduce 1 0)
    (12, Token (LPAREN _)) -> Just (Reduce 1 0)
    (12, Token (RPAREN _)) -> Just (Reduce 1 0)
    (12, Token (DOT _)) -> Just (Reduce 1 0)
    (12, Token (QUOTE _)) -> Just (Reduce 1 0)
    (12, Token (BACKQUOTE _)) -> Just (Reduce 1 0)
    (12, Token (COMMA _)) -> Just (Reduce 1 0)
    (12, Token (COMMAAT _)) -> Just (Reduce 1 0)
    (12, Token (UINTEGER10 _)) -> Just (Reduce 1 0)
    (13, EOF) -> Just (Reduce 1 1)
    (13, Token (EQUAL _)) -> Just (Reduce 1 1)
    (13, Token (SHARP _)) -> Just (Reduce 1 1)
    (13, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 1)
    (13, Token (BOOLEAN _)) -> Just (Reduce 1 1)
    (13, Token (NUMBER _)) -> Just (Reduce 1 1)
    (13, Token (CHARACTER _)) -> Just (Reduce 1 1)
    (13, Token (STRING _)) -> Just (Reduce 1 1)
    (13, Token (BYTEVECTOR _)) -> Just (Reduce 1 1)
    (13, Token (IDENTIFIER _)) -> Just (Reduce 1 1)
    (13, Token (LPAREN _)) -> Just (Reduce 1 1)
    (13, Token (RPAREN _)) -> Just (Reduce 1 1)
    (13, Token (DOT _)) -> Just (Reduce 1 1)
    (13, Token (QUOTE _)) -> Just (Reduce 1 1)
    (13, Token (BACKQUOTE _)) -> Just (Reduce 1 1)
    (13, Token (COMMA _)) -> Just (Reduce 1 1)
    (13, Token (COMMAAT _)) -> Just (Reduce 1 1)
    (13, Token (UINTEGER10 _)) -> Just (Reduce 1 1)
    (14, Token (EQUAL _)) -> Just (Shift 2)
    (14, Token (SHARP _)) -> Just (Shift 9)
    (15, EOF) -> Just (Reduce 1 5)
    (15, Token (EQUAL _)) -> Just (Reduce 1 5)
    (15, Token (SHARP _)) -> Just (Reduce 1 5)
    (15, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 5)
    (15, Token (BOOLEAN _)) -> Just (Reduce 1 5)
    (15, Token (NUMBER _)) -> Just (Reduce 1 5)
    (15, Token (CHARACTER _)) -> Just (Reduce 1 5)
    (15, Token (STRING _)) -> Just (Reduce 1 5)
    (15, Token (BYTEVECTOR _)) -> Just (Reduce 1 5)
    (15, Token (IDENTIFIER _)) -> Just (Reduce 1 5)
    (15, Token (LPAREN _)) -> Just (Reduce 1 5)
    (15, Token (RPAREN _)) -> Just (Reduce 1 5)
    (15, Token (DOT _)) -> Just (Reduce 1 5)
    (15, Token (QUOTE _)) -> Just (Reduce 1 5)
    (15, Token (BACKQUOTE _)) -> Just (Reduce 1 5)
    (15, Token (COMMA _)) -> Just (Reduce 1 5)
    (15, Token (COMMAAT _)) -> Just (Reduce 1 5)
    (15, Token (UINTEGER10 _)) -> Just (Reduce 1 5)
    (16, EOF) -> Just (Reduce 1 6)
    (16, Token (EQUAL _)) -> Just (Reduce 1 6)
    (16, Token (SHARP _)) -> Just (Reduce 1 6)
    (16, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 6)
    (16, Token (BOOLEAN _)) -> Just (Reduce 1 6)
    (16, Token (NUMBER _)) -> Just (Reduce 1 6)
    (16, Token (CHARACTER _)) -> Just (Reduce 1 6)
    (16, Token (STRING _)) -> Just (Reduce 1 6)
    (16, Token (BYTEVECTOR _)) -> Just (Reduce 1 6)
    (16, Token (IDENTIFIER _)) -> Just (Reduce 1 6)
    (16, Token (LPAREN _)) -> Just (Reduce 1 6)
    (16, Token (RPAREN _)) -> Just (Reduce 1 6)
    (16, Token (DOT _)) -> Just (Reduce 1 6)
    (16, Token (QUOTE _)) -> Just (Reduce 1 6)
    (16, Token (BACKQUOTE _)) -> Just (Reduce 1 6)
    (16, Token (COMMA _)) -> Just (Reduce 1 6)
    (16, Token (COMMAAT _)) -> Just (Reduce 1 6)
    (16, Token (UINTEGER10 _)) -> Just (Reduce 1 6)
    (17, EOF) -> Just (Reduce 1 7)
    (17, Token (EQUAL _)) -> Just (Reduce 1 7)
    (17, Token (SHARP _)) -> Just (Reduce 1 7)
    (17, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 7)
    (17, Token (BOOLEAN _)) -> Just (Reduce 1 7)
    (17, Token (NUMBER _)) -> Just (Reduce 1 7)
    (17, Token (CHARACTER _)) -> Just (Reduce 1 7)
    (17, Token (STRING _)) -> Just (Reduce 1 7)
    (17, Token (BYTEVECTOR _)) -> Just (Reduce 1 7)
    (17, Token (IDENTIFIER _)) -> Just (Reduce 1 7)
    (17, Token (LPAREN _)) -> Just (Reduce 1 7)
    (17, Token (RPAREN _)) -> Just (Reduce 1 7)
    (17, Token (DOT _)) -> Just (Reduce 1 7)
    (17, Token (QUOTE _)) -> Just (Reduce 1 7)
    (17, Token (BACKQUOTE _)) -> Just (Reduce 1 7)
    (17, Token (COMMA _)) -> Just (Reduce 1 7)
    (17, Token (COMMAAT _)) -> Just (Reduce 1 7)
    (17, Token (UINTEGER10 _)) -> Just (Reduce 1 7)
    (18, EOF) -> Just (Reduce 1 8)
    (18, Token (EQUAL _)) -> Just (Reduce 1 8)
    (18, Token (SHARP _)) -> Just (Reduce 1 8)
    (18, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 8)
    (18, Token (BOOLEAN _)) -> Just (Reduce 1 8)
    (18, Token (NUMBER _)) -> Just (Reduce 1 8)
    (18, Token (CHARACTER _)) -> Just (Reduce 1 8)
    (18, Token (STRING _)) -> Just (Reduce 1 8)
    (18, Token (BYTEVECTOR _)) -> Just (Reduce 1 8)
    (18, Token (IDENTIFIER _)) -> Just (Reduce 1 8)
    (18, Token (LPAREN _)) -> Just (Reduce 1 8)
    (18, Token (RPAREN _)) -> Just (Reduce 1 8)
    (18, Token (DOT _)) -> Just (Reduce 1 8)
    (18, Token (QUOTE _)) -> Just (Reduce 1 8)
    (18, Token (BACKQUOTE _)) -> Just (Reduce 1 8)
    (18, Token (COMMA _)) -> Just (Reduce 1 8)
    (18, Token (COMMAAT _)) -> Just (Reduce 1 8)
    (18, Token (UINTEGER10 _)) -> Just (Reduce 1 8)
    (19, EOF) -> Just (Reduce 1 10)
    (19, Token (EQUAL _)) -> Just (Reduce 1 10)
    (19, Token (SHARP _)) -> Just (Reduce 1 10)
    (19, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 10)
    (19, Token (BOOLEAN _)) -> Just (Reduce 1 10)
    (19, Token (NUMBER _)) -> Just (Reduce 1 10)
    (19, Token (CHARACTER _)) -> Just (Reduce 1 10)
    (19, Token (STRING _)) -> Just (Reduce 1 10)
    (19, Token (BYTEVECTOR _)) -> Just (Reduce 1 10)
    (19, Token (IDENTIFIER _)) -> Just (Reduce 1 10)
    (19, Token (LPAREN _)) -> Just (Reduce 1 10)
    (19, Token (RPAREN _)) -> Just (Reduce 1 10)
    (19, Token (DOT _)) -> Just (Reduce 1 10)
    (19, Token (QUOTE _)) -> Just (Reduce 1 10)
    (19, Token (BACKQUOTE _)) -> Just (Reduce 1 10)
    (19, Token (COMMA _)) -> Just (Reduce 1 10)
    (19, Token (COMMAAT _)) -> Just (Reduce 1 10)
    (19, Token (UINTEGER10 _)) -> Just (Reduce 1 10)
    (20, EOF) -> Just (Reduce 1 9)
    (20, Token (EQUAL _)) -> Just (Reduce 1 9)
    (20, Token (SHARP _)) -> Just (Reduce 1 9)
    (20, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 9)
    (20, Token (BOOLEAN _)) -> Just (Reduce 1 9)
    (20, Token (NUMBER _)) -> Just (Reduce 1 9)
    (20, Token (CHARACTER _)) -> Just (Reduce 1 9)
    (20, Token (STRING _)) -> Just (Reduce 1 9)
    (20, Token (BYTEVECTOR _)) -> Just (Reduce 1 9)
    (20, Token (IDENTIFIER _)) -> Just (Reduce 1 9)
    (20, Token (LPAREN _)) -> Just (Reduce 1 9)
    (20, Token (RPAREN _)) -> Just (Reduce 1 9)
    (20, Token (DOT _)) -> Just (Reduce 1 9)
    (20, Token (QUOTE _)) -> Just (Reduce 1 9)
    (20, Token (BACKQUOTE _)) -> Just (Reduce 1 9)
    (20, Token (COMMA _)) -> Just (Reduce 1 9)
    (20, Token (COMMAAT _)) -> Just (Reduce 1 9)
    (20, Token (UINTEGER10 _)) -> Just (Reduce 1 9)
    (21, EOF) -> Just (Reduce 1 13)
    (21, Token (EQUAL _)) -> Just (Reduce 1 13)
    (21, Token (SHARP _)) -> Just (Reduce 1 13)
    (21, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 13)
    (21, Token (BOOLEAN _)) -> Just (Reduce 1 13)
    (21, Token (NUMBER _)) -> Just (Reduce 1 13)
    (21, Token (CHARACTER _)) -> Just (Reduce 1 13)
    (21, Token (STRING _)) -> Just (Reduce 1 13)
    (21, Token (BYTEVECTOR _)) -> Just (Reduce 1 13)
    (21, Token (IDENTIFIER _)) -> Just (Reduce 1 13)
    (21, Token (LPAREN _)) -> Just (Reduce 1 13)
    (21, Token (RPAREN _)) -> Just (Reduce 1 13)
    (21, Token (DOT _)) -> Just (Reduce 1 13)
    (21, Token (QUOTE _)) -> Just (Reduce 1 13)
    (21, Token (BACKQUOTE _)) -> Just (Reduce 1 13)
    (21, Token (COMMA _)) -> Just (Reduce 1 13)
    (21, Token (COMMAAT _)) -> Just (Reduce 1 13)
    (21, Token (UINTEGER10 _)) -> Just (Reduce 1 13)
    (22, EOF) -> Just (Reduce 1 14)
    (22, Token (EQUAL _)) -> Just (Reduce 1 14)
    (22, Token (SHARP _)) -> Just (Reduce 1 14)
    (22, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 14)
    (22, Token (BOOLEAN _)) -> Just (Reduce 1 14)
    (22, Token (NUMBER _)) -> Just (Reduce 1 14)
    (22, Token (CHARACTER _)) -> Just (Reduce 1 14)
    (22, Token (STRING _)) -> Just (Reduce 1 14)
    (22, Token (BYTEVECTOR _)) -> Just (Reduce 1 14)
    (22, Token (IDENTIFIER _)) -> Just (Reduce 1 14)
    (22, Token (LPAREN _)) -> Just (Reduce 1 14)
    (22, Token (RPAREN _)) -> Just (Reduce 1 14)
    (22, Token (DOT _)) -> Just (Reduce 1 14)
    (22, Token (QUOTE _)) -> Just (Reduce 1 14)
    (22, Token (BACKQUOTE _)) -> Just (Reduce 1 14)
    (22, Token (COMMA _)) -> Just (Reduce 1 14)
    (22, Token (COMMAAT _)) -> Just (Reduce 1 14)
    (22, Token (UINTEGER10 _)) -> Just (Reduce 1 14)
    (23, EOF) -> Just (Reduce 1 15)
    (23, Token (EQUAL _)) -> Just (Reduce 1 15)
    (23, Token (SHARP _)) -> Just (Reduce 1 15)
    (23, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 15)
    (23, Token (BOOLEAN _)) -> Just (Reduce 1 15)
    (23, Token (NUMBER _)) -> Just (Reduce 1 15)
    (23, Token (CHARACTER _)) -> Just (Reduce 1 15)
    (23, Token (STRING _)) -> Just (Reduce 1 15)
    (23, Token (BYTEVECTOR _)) -> Just (Reduce 1 15)
    (23, Token (IDENTIFIER _)) -> Just (Reduce 1 15)
    (23, Token (LPAREN _)) -> Just (Reduce 1 15)
    (23, Token (RPAREN _)) -> Just (Reduce 1 15)
    (23, Token (DOT _)) -> Just (Reduce 1 15)
    (23, Token (QUOTE _)) -> Just (Reduce 1 15)
    (23, Token (BACKQUOTE _)) -> Just (Reduce 1 15)
    (23, Token (COMMA _)) -> Just (Reduce 1 15)
    (23, Token (COMMAAT _)) -> Just (Reduce 1 15)
    (23, Token (UINTEGER10 _)) -> Just (Reduce 1 15)
    (24, Token (EQUAL _)) -> Just (Reduce 1 28)
    (24, Token (SHARP _)) -> Just (Reduce 1 28)
    (25, EOF) -> Just (Reduce 1 12)
    (25, Token (EQUAL _)) -> Just (Reduce 1 12)
    (25, Token (SHARP _)) -> Just (Reduce 1 12)
    (25, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 12)
    (25, Token (BOOLEAN _)) -> Just (Reduce 1 12)
    (25, Token (NUMBER _)) -> Just (Reduce 1 12)
    (25, Token (CHARACTER _)) -> Just (Reduce 1 12)
    (25, Token (STRING _)) -> Just (Reduce 1 12)
    (25, Token (BYTEVECTOR _)) -> Just (Reduce 1 12)
    (25, Token (IDENTIFIER _)) -> Just (Reduce 1 12)
    (25, Token (LPAREN _)) -> Just (Reduce 1 12)
    (25, Token (RPAREN _)) -> Just (Reduce 1 12)
    (25, Token (DOT _)) -> Just (Reduce 1 12)
    (25, Token (QUOTE _)) -> Just (Reduce 1 12)
    (25, Token (BACKQUOTE _)) -> Just (Reduce 1 12)
    (25, Token (COMMA _)) -> Just (Reduce 1 12)
    (25, Token (COMMAAT _)) -> Just (Reduce 1 12)
    (25, Token (UINTEGER10 _)) -> Just (Reduce 1 12)
    (26, EOF) -> Just (Reduce 1 11)
    (26, Token (EQUAL _)) -> Just (Reduce 1 11)
    (26, Token (SHARP _)) -> Just (Reduce 1 11)
    (26, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 11)
    (26, Token (BOOLEAN _)) -> Just (Reduce 1 11)
    (26, Token (NUMBER _)) -> Just (Reduce 1 11)
    (26, Token (CHARACTER _)) -> Just (Reduce 1 11)
    (26, Token (STRING _)) -> Just (Reduce 1 11)
    (26, Token (BYTEVECTOR _)) -> Just (Reduce 1 11)
    (26, Token (IDENTIFIER _)) -> Just (Reduce 1 11)
    (26, Token (LPAREN _)) -> Just (Reduce 1 11)
    (26, Token (RPAREN _)) -> Just (Reduce 1 11)
    (26, Token (DOT _)) -> Just (Reduce 1 11)
    (26, Token (QUOTE _)) -> Just (Reduce 1 11)
    (26, Token (BACKQUOTE _)) -> Just (Reduce 1 11)
    (26, Token (COMMA _)) -> Just (Reduce 1 11)
    (26, Token (COMMAAT _)) -> Just (Reduce 1 11)
    (26, Token (UINTEGER10 _)) -> Just (Reduce 1 11)
    (27, EOF) -> Just (Reduce 5 17)
    (27, Token (EQUAL _)) -> Just (Reduce 5 17)
    (27, Token (SHARP _)) -> Just (Reduce 5 17)
    (27, Token (SHARP_SEMICOLON _)) -> Just (Reduce 5 17)
    (27, Token (BOOLEAN _)) -> Just (Reduce 5 17)
    (27, Token (NUMBER _)) -> Just (Reduce 5 17)
    (27, Token (CHARACTER _)) -> Just (Reduce 5 17)
    (27, Token (STRING _)) -> Just (Reduce 5 17)
    (27, Token (BYTEVECTOR _)) -> Just (Reduce 5 17)
    (27, Token (IDENTIFIER _)) -> Just (Reduce 5 17)
    (27, Token (LPAREN _)) -> Just (Reduce 5 17)
    (27, Token (RPAREN _)) -> Just (Reduce 5 17)
    (27, Token (DOT _)) -> Just (Reduce 5 17)
    (27, Token (QUOTE _)) -> Just (Reduce 5 17)
    (27, Token (BACKQUOTE _)) -> Just (Reduce 5 17)
    (27, Token (COMMA _)) -> Just (Reduce 5 17)
    (27, Token (COMMAAT _)) -> Just (Reduce 5 17)
    (27, Token (UINTEGER10 _)) -> Just (Reduce 5 17)
    (28, EOF) -> Just (Reduce 3 16)
    (28, Token (EQUAL _)) -> Just (Reduce 3 16)
    (28, Token (SHARP _)) -> Just (Reduce 3 16)
    (28, Token (SHARP_SEMICOLON _)) -> Just (Reduce 3 16)
    (28, Token (BOOLEAN _)) -> Just (Reduce 3 16)
    (28, Token (NUMBER _)) -> Just (Reduce 3 16)
    (28, Token (CHARACTER _)) -> Just (Reduce 3 16)
    (28, Token (STRING _)) -> Just (Reduce 3 16)
    (28, Token (BYTEVECTOR _)) -> Just (Reduce 3 16)
    (28, Token (IDENTIFIER _)) -> Just (Reduce 3 16)
    (28, Token (LPAREN _)) -> Just (Reduce 3 16)
    (28, Token (RPAREN _)) -> Just (Reduce 3 16)
    (28, Token (DOT _)) -> Just (Reduce 3 16)
    (28, Token (QUOTE _)) -> Just (Reduce 3 16)
    (28, Token (BACKQUOTE _)) -> Just (Reduce 3 16)
    (28, Token (COMMA _)) -> Just (Reduce 3 16)
    (28, Token (COMMAAT _)) -> Just (Reduce 3 16)
    (28, Token (UINTEGER10 _)) -> Just (Reduce 3 16)
    (29, Token (RPAREN _)) -> Just (Shift 27)
    (30, Token (RPAREN _)) -> Just (Shift 28)
    (31, Token (DOT _)) -> Just (Shift 7)
    (32, Token (LPAREN _)) -> Just (Shift 8)
    (33, EOF) -> Just (Reduce 4 27)
    (33, Token (EQUAL _)) -> Just (Reduce 4 27)
    (33, Token (SHARP _)) -> Just (Reduce 4 27)
    (33, Token (SHARP_SEMICOLON _)) -> Just (Reduce 4 27)
    (33, Token (BOOLEAN _)) -> Just (Reduce 4 27)
    (33, Token (NUMBER _)) -> Just (Reduce 4 27)
    (33, Token (CHARACTER _)) -> Just (Reduce 4 27)
    (33, Token (STRING _)) -> Just (Reduce 4 27)
    (33, Token (BYTEVECTOR _)) -> Just (Reduce 4 27)
    (33, Token (IDENTIFIER _)) -> Just (Reduce 4 27)
    (33, Token (LPAREN _)) -> Just (Reduce 4 27)
    (33, Token (RPAREN _)) -> Just (Reduce 4 27)
    (33, Token (DOT _)) -> Just (Reduce 4 27)
    (33, Token (QUOTE _)) -> Just (Reduce 4 27)
    (33, Token (BACKQUOTE _)) -> Just (Reduce 4 27)
    (33, Token (COMMA _)) -> Just (Reduce 4 27)
    (33, Token (COMMAAT _)) -> Just (Reduce 4 27)
    (33, Token (UINTEGER10 _)) -> Just (Reduce 4 27)
    (34, Token (RPAREN _)) -> Just (Shift 33)
    (35, EOF) -> Just (Reduce 2 22)
    (35, Token (EQUAL _)) -> Just (Reduce 2 22)
    (35, Token (SHARP _)) -> Just (Reduce 2 22)
    (35, Token (SHARP_SEMICOLON _)) -> Just (Reduce 2 22)
    (35, Token (BOOLEAN _)) -> Just (Reduce 2 22)
    (35, Token (NUMBER _)) -> Just (Reduce 2 22)
    (35, Token (CHARACTER _)) -> Just (Reduce 2 22)
    (35, Token (STRING _)) -> Just (Reduce 2 22)
    (35, Token (BYTEVECTOR _)) -> Just (Reduce 2 22)
    (35, Token (IDENTIFIER _)) -> Just (Reduce 2 22)
    (35, Token (LPAREN _)) -> Just (Reduce 2 22)
    (35, Token (RPAREN _)) -> Just (Reduce 2 22)
    (35, Token (DOT _)) -> Just (Reduce 2 22)
    (35, Token (QUOTE _)) -> Just (Reduce 2 22)
    (35, Token (BACKQUOTE _)) -> Just (Reduce 2 22)
    (35, Token (COMMA _)) -> Just (Reduce 2 22)
    (35, Token (COMMAAT _)) -> Just (Reduce 2 22)
    (35, Token (UINTEGER10 _)) -> Just (Reduce 2 22)
    (36, Token (RPAREN _)) -> Just (Reduce 2 19)
    (37, Token (DOT _)) -> Just (Reduce 2 21)
    (38, Token (EQUAL _)) -> Just (Reduce 1 23)
    (38, Token (SHARP _)) -> Just (Reduce 1 23)
    (38, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 23)
    (38, Token (BOOLEAN _)) -> Just (Reduce 1 23)
    (38, Token (NUMBER _)) -> Just (Reduce 1 23)
    (38, Token (CHARACTER _)) -> Just (Reduce 1 23)
    (38, Token (STRING _)) -> Just (Reduce 1 23)
    (38, Token (BYTEVECTOR _)) -> Just (Reduce 1 23)
    (38, Token (IDENTIFIER _)) -> Just (Reduce 1 23)
    (38, Token (LPAREN _)) -> Just (Reduce 1 23)
    (38, Token (QUOTE _)) -> Just (Reduce 1 23)
    (38, Token (BACKQUOTE _)) -> Just (Reduce 1 23)
    (38, Token (COMMA _)) -> Just (Reduce 1 23)
    (38, Token (COMMAAT _)) -> Just (Reduce 1 23)
    (38, Token (UINTEGER10 _)) -> Just (Reduce 1 23)
    (39, Token (EQUAL _)) -> Just (Reduce 1 24)
    (39, Token (SHARP _)) -> Just (Reduce 1 24)
    (39, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 24)
    (39, Token (BOOLEAN _)) -> Just (Reduce 1 24)
    (39, Token (NUMBER _)) -> Just (Reduce 1 24)
    (39, Token (CHARACTER _)) -> Just (Reduce 1 24)
    (39, Token (STRING _)) -> Just (Reduce 1 24)
    (39, Token (BYTEVECTOR _)) -> Just (Reduce 1 24)
    (39, Token (IDENTIFIER _)) -> Just (Reduce 1 24)
    (39, Token (LPAREN _)) -> Just (Reduce 1 24)
    (39, Token (QUOTE _)) -> Just (Reduce 1 24)
    (39, Token (BACKQUOTE _)) -> Just (Reduce 1 24)
    (39, Token (COMMA _)) -> Just (Reduce 1 24)
    (39, Token (COMMAAT _)) -> Just (Reduce 1 24)
    (39, Token (UINTEGER10 _)) -> Just (Reduce 1 24)
    (40, Token (EQUAL _)) -> Just (Reduce 1 25)
    (40, Token (SHARP _)) -> Just (Reduce 1 25)
    (40, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 25)
    (40, Token (BOOLEAN _)) -> Just (Reduce 1 25)
    (40, Token (NUMBER _)) -> Just (Reduce 1 25)
    (40, Token (CHARACTER _)) -> Just (Reduce 1 25)
    (40, Token (STRING _)) -> Just (Reduce 1 25)
    (40, Token (BYTEVECTOR _)) -> Just (Reduce 1 25)
    (40, Token (IDENTIFIER _)) -> Just (Reduce 1 25)
    (40, Token (LPAREN _)) -> Just (Reduce 1 25)
    (40, Token (QUOTE _)) -> Just (Reduce 1 25)
    (40, Token (BACKQUOTE _)) -> Just (Reduce 1 25)
    (40, Token (COMMA _)) -> Just (Reduce 1 25)
    (40, Token (COMMAAT _)) -> Just (Reduce 1 25)
    (40, Token (UINTEGER10 _)) -> Just (Reduce 1 25)
    (41, Token (EQUAL _)) -> Just (Reduce 1 26)
    (41, Token (SHARP _)) -> Just (Reduce 1 26)
    (41, Token (SHARP_SEMICOLON _)) -> Just (Reduce 1 26)
    (41, Token (BOOLEAN _)) -> Just (Reduce 1 26)
    (41, Token (NUMBER _)) -> Just (Reduce 1 26)
    (41, Token (CHARACTER _)) -> Just (Reduce 1 26)
    (41, Token (STRING _)) -> Just (Reduce 1 26)
    (41, Token (BYTEVECTOR _)) -> Just (Reduce 1 26)
    (41, Token (IDENTIFIER _)) -> Just (Reduce 1 26)
    (41, Token (LPAREN _)) -> Just (Reduce 1 26)
    (41, Token (QUOTE _)) -> Just (Reduce 1 26)
    (41, Token (BACKQUOTE _)) -> Just (Reduce 1 26)
    (41, Token (COMMA _)) -> Just (Reduce 1 26)
    (41, Token (COMMAAT _)) -> Just (Reduce 1 26)
    (41, Token (UINTEGER10 _)) -> Just (Reduce 1 26)
    (_, _) -> Nothing

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
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 1) -> Just 12
    (0, 2) -> Just 13
    (0, 3) -> Just 14
    (0, 4) -> Just 20
    (0, 5) -> Just 21
    (0, 6) -> Just 22
    (0, 7) -> Just 23
    (0, 10) -> Just 4
    (2, 0) -> Just 10
    (2, 1) -> Just 12
    (2, 2) -> Just 13
    (2, 3) -> Just 14
    (2, 4) -> Just 20
    (2, 5) -> Just 21
    (2, 6) -> Just 22
    (2, 7) -> Just 23
    (2, 10) -> Just 4
    (3, 0) -> Just 11
    (3, 1) -> Just 12
    (3, 2) -> Just 13
    (3, 3) -> Just 14
    (3, 4) -> Just 20
    (3, 5) -> Just 21
    (3, 6) -> Just 22
    (3, 7) -> Just 23
    (3, 10) -> Just 4
    (4, 0) -> Just 35
    (4, 1) -> Just 12
    (4, 2) -> Just 13
    (4, 3) -> Just 14
    (4, 4) -> Just 20
    (4, 5) -> Just 21
    (4, 6) -> Just 22
    (4, 7) -> Just 23
    (4, 10) -> Just 4
    (5, 0) -> Just 6
    (5, 1) -> Just 12
    (5, 2) -> Just 13
    (5, 3) -> Just 14
    (5, 4) -> Just 20
    (5, 5) -> Just 21
    (5, 6) -> Just 22
    (5, 7) -> Just 23
    (5, 8) -> Just 30
    (5, 9) -> Just 31
    (5, 10) -> Just 4
    (6, 0) -> Just 6
    (6, 1) -> Just 12
    (6, 2) -> Just 13
    (6, 3) -> Just 14
    (6, 4) -> Just 20
    (6, 5) -> Just 21
    (6, 6) -> Just 22
    (6, 7) -> Just 23
    (6, 8) -> Just 36
    (6, 9) -> Just 37
    (6, 10) -> Just 4
    (7, 0) -> Just 29
    (7, 1) -> Just 12
    (7, 2) -> Just 13
    (7, 3) -> Just 14
    (7, 4) -> Just 20
    (7, 5) -> Just 21
    (7, 6) -> Just 22
    (7, 7) -> Just 23
    (7, 10) -> Just 4
    (8, 0) -> Just 34
    (8, 1) -> Just 12
    (8, 2) -> Just 13
    (8, 3) -> Just 14
    (8, 4) -> Just 20
    (8, 5) -> Just 21
    (8, 6) -> Just 22
    (8, 7) -> Just 23
    (8, 10) -> Just 4
    (_, _) -> Nothing

parse :: Monad m => SemanticActions m -> [Token] -> m (Maybe (Datum, [Token]))
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
                return Nothing
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
          case stack of { [(_, StackValue_datum value)] -> return $ Just (value, tokens); _ -> return Nothing }



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


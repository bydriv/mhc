module  Language.HsLex.Parsing  where
import qualified Control.Monad as Monad


import qualified Data.Char      as Char
import qualified Language.HsLex as HsLex

type ANY = ()
type HAT = ()
type HYPHEN = ()
type LBRACKET = ()
type RBRACKET = ()
type CODE = Char
type COMMA = ()
type DARROW = ()
type CASE = ()
type SEMANTIC_ACTION = String
type LEXING_STATE = String
type PLBRACE = ()
type PMODULE = ()
type PP = ()
type PRBRACE = ()
type PWHERE = ()
type PIPE = ()
type STAR = ()
type PLUS = ()
type QUES = ()
type LPAREN = ()
type RPAREN = ()
type STRING = String
type CHAR = Char
type GENERAL_CATEGORY = Char.GeneralCategory

type Start = (Definitions, Cases, Codes)

type Definitions = [Definition]

data Definition =
    DefnModule Codes
  | DefnCodes Codes
  deriving (Eq, Ord, Read, Show)

type Cases = [Case]
type Case = (LexingState, Regexp, SemanticAction)

type Atexp = Regexp
type Suffixexp = Regexp
type Catexp = Regexp
type Orexp = Regexp
type Exp = Regexp
type Regexp = HsLex.Regexp
type Charsets = [Charset]
data Charset = Range Char Char | Chars Chars
type Chars = [CHAR]

type LexingState = LEXING_STATE
type SemanticAction = SEMANTIC_ACTION

type Codes = [Code]
type Code = CODE

data Token =
    ANY ANY
  | CASE CASE
  | CHAR CHAR
  | CODE CODE
  | COMMA COMMA
  | DARROW DARROW
  | GENERAL_CATEGORY GENERAL_CATEGORY
  | HAT HAT
  | HYPHEN HYPHEN
  | LBRACKET LBRACKET
  | LEXING_STATE LEXING_STATE
  | LPAREN LPAREN
  | PIPE PIPE
  | PLBRACE PLBRACE
  | PLUS PLUS
  | PMODULE PMODULE
  | PP PP
  | PRBRACE PRBRACE
  | PWHERE PWHERE
  | QUES QUES
  | RBRACKET RBRACKET
  | RPAREN RPAREN
  | SEMANTIC_ACTION SEMANTIC_ACTION
  | STAR STAR
  | STRING STRING
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
type GotoState = Int
type GotoSymbol = Int

data StackValue =
    StackValue_EOF
  | StackValue_PP PP
  | StackValue_PMODULE PMODULE
  | StackValue_PWHERE PWHERE
  | StackValue_PLBRACE PLBRACE
  | StackValue_PRBRACE PRBRACE
  | StackValue_CASE CASE
  | StackValue_LPAREN LPAREN
  | StackValue_COMMA COMMA
  | StackValue_RPAREN RPAREN
  | StackValue_DARROW DARROW
  | StackValue_ANY ANY
  | StackValue_CHAR CHAR
  | StackValue_STRING STRING
  | StackValue_GENERAL_CATEGORY GENERAL_CATEGORY
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_HAT HAT
  | StackValue_PIPE PIPE
  | StackValue_HYPHEN HYPHEN
  | StackValue_STAR STAR
  | StackValue_PLUS PLUS
  | StackValue_QUES QUES
  | StackValue_LEXING_STATE LEXING_STATE
  | StackValue_SEMANTIC_ACTION SEMANTIC_ACTION
  | StackValue_CODE CODE
  | StackValue_start Start
  | StackValue_definitions Definitions
  | StackValue_cases Cases
  | StackValue_codes Codes
  | StackValue_definition Definition
  | StackValue_case Case
  | StackValue_lexingState LexingState
  | StackValue_exp Exp
  | StackValue_semanticAction SemanticAction
  | StackValue_atexp Atexp
  | StackValue_charsets Charsets
  | StackValue_charset Charset
  | StackValue_chars Chars
  | StackValue_suffixexp Suffixexp
  | StackValue_catexp Catexp
  | StackValue_orexp Orexp
  | StackValue_code Code

data SemanticActions m = SemanticActions
  { start_implies_definitions_PP_cases_PP_codes :: Definitions -> PP -> Cases -> PP -> Codes -> m Start
  , definitions_implies :: m Definitions
  , definitions_implies_definition_definitions :: Definition -> Definitions -> m Definitions
  , definition_implies_PMODULE_codes_PWHERE :: PMODULE -> Codes -> PWHERE -> m Definition
  , definition_implies_PLBRACE_codes_PRBRACE :: PLBRACE -> Codes -> PRBRACE -> m Definition
  , cases_implies :: m Cases
  , cases_implies_case_cases :: Case -> Cases -> m Cases
  , case_implies_CASE_LPAREN_lexingState_COMMA_exp_RPAREN_DARROW_semanticAction :: CASE -> LPAREN -> LexingState -> COMMA -> Exp -> RPAREN -> DARROW -> SemanticAction -> m Case
  , atexp_implies_ANY :: ANY -> m Atexp
  , atexp_implies_CHAR :: CHAR -> m Atexp
  , atexp_implies_STRING :: STRING -> m Atexp
  , atexp_implies_GENERAL_CATEGORY :: GENERAL_CATEGORY -> m Atexp
  , atexp_implies_LPAREN_exp_RPAREN :: LPAREN -> Exp -> RPAREN -> m Atexp
  , atexp_implies_LBRACKET_charsets_RBRACKET :: LBRACKET -> Charsets -> RBRACKET -> m Atexp
  , atexp_implies_LBRACKET_HAT_charsets_RBRACKET :: LBRACKET -> HAT -> Charsets -> RBRACKET -> m Atexp
  , charsets_implies_charset :: Charset -> m Charsets
  , charsets_implies_charset_PIPE_charsets :: Charset -> PIPE -> Charsets -> m Charsets
  , charset_implies_chars :: Chars -> m Charset
  , charset_implies_CHAR_HYPHEN_CHAR :: CHAR -> HYPHEN -> CHAR -> m Charset
  , chars_implies_CHAR :: CHAR -> m Chars
  , chars_implies_CHAR_chars :: CHAR -> Chars -> m Chars
  , suffixexp_implies_atexp :: Atexp -> m Suffixexp
  , suffixexp_implies_suffixexp_STAR :: Suffixexp -> STAR -> m Suffixexp
  , suffixexp_implies_suffixexp_PLUS :: Suffixexp -> PLUS -> m Suffixexp
  , suffixexp_implies_suffixexp_QUES :: Suffixexp -> QUES -> m Suffixexp
  , catexp_implies_suffixexp :: Suffixexp -> m Catexp
  , catexp_implies_suffixexp_catexp :: Suffixexp -> Catexp -> m Catexp
  , orexp_implies_catexp :: Catexp -> m Orexp
  , orexp_implies_catexp_PIPE_orexp :: Catexp -> PIPE -> Orexp -> m Orexp
  , exp_implies_orexp :: Orexp -> m Exp
  , lexingState_implies_LEXING_STATE :: LEXING_STATE -> m LexingState
  , semanticAction_implies_SEMANTIC_ACTION :: SEMANTIC_ACTION -> m SemanticAction
  , codes_implies :: m Codes
  , codes_implies_code_codes :: Code -> Codes -> m Codes
  , code_implies_CODE :: CODE -> m Code }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token (PP _)) -> Just (Reduce 0 1)
    (0, Token (PMODULE _)) -> Just (Shift 12)
    (0, Token (PLBRACE _)) -> Just (Shift 13)
    (1, EOF) -> Just (Accept)
    (2, Token (PP _)) -> Just (Reduce 0 5)
    (2, Token (CASE _)) -> Just (Shift 19)
    (3, EOF) -> Just (Reduce 0 32)
    (3, Token (CODE _)) -> Just (Shift 61)
    (4, Token (PP _)) -> Just (Shift 2)
    (5, Token (PP _)) -> Just (Shift 3)
    (6, EOF) -> Just (Reduce 5 0)
    (7, Token (PP _)) -> Just (Reduce 0 1)
    (7, Token (PMODULE _)) -> Just (Shift 12)
    (7, Token (PLBRACE _)) -> Just (Shift 13)
    (8, Token (PP _)) -> Just (Reduce 2 2)
    (9, Token (PP _)) -> Just (Reduce 0 5)
    (9, Token (CASE _)) -> Just (Shift 19)
    (10, Token (PP _)) -> Just (Reduce 2 6)
    (11, EOF) -> Just (Reduce 0 32)
    (11, Token (PWHERE _)) -> Just (Reduce 0 32)
    (11, Token (PRBRACE _)) -> Just (Reduce 0 32)
    (11, Token (CODE _)) -> Just (Shift 61)
    (12, Token (PWHERE _)) -> Just (Reduce 0 32)
    (12, Token (CODE _)) -> Just (Shift 61)
    (13, Token (PRBRACE _)) -> Just (Reduce 0 32)
    (13, Token (CODE _)) -> Just (Shift 61)
    (14, EOF) -> Just (Reduce 2 33)
    (14, Token (PWHERE _)) -> Just (Reduce 2 33)
    (14, Token (PRBRACE _)) -> Just (Reduce 2 33)
    (15, Token (PP _)) -> Just (Reduce 3 3)
    (15, Token (PMODULE _)) -> Just (Reduce 3 3)
    (15, Token (PLBRACE _)) -> Just (Reduce 3 3)
    (15, Token (CODE _)) -> Just (Reduce 3 3)
    (16, Token (PP _)) -> Just (Reduce 3 4)
    (16, Token (PMODULE _)) -> Just (Reduce 3 4)
    (16, Token (PLBRACE _)) -> Just (Reduce 3 4)
    (16, Token (CODE _)) -> Just (Reduce 3 4)
    (17, Token (PWHERE _)) -> Just (Shift 15)
    (18, Token (PRBRACE _)) -> Just (Shift 16)
    (19, Token (LPAREN _)) -> Just (Shift 20)
    (20, Token (LEXING_STATE _)) -> Just (Shift 27)
    (21, Token (LPAREN _)) -> Just (Shift 28)
    (21, Token (ANY _)) -> Just (Shift 34)
    (21, Token (CHAR _)) -> Just (Shift 35)
    (21, Token (STRING _)) -> Just (Shift 36)
    (21, Token (GENERAL_CATEGORY _)) -> Just (Shift 37)
    (21, Token (LBRACKET _)) -> Just (Shift 38)
    (22, Token (DARROW _)) -> Just (Shift 23)
    (23, Token (SEMANTIC_ACTION _)) -> Just (Shift 30)
    (24, Token (COMMA _)) -> Just (Shift 21)
    (25, Token (RPAREN _)) -> Just (Shift 22)
    (26, Token (PP _)) -> Just (Reduce 8 7)
    (26, Token (CASE _)) -> Just (Reduce 8 7)
    (26, Token (LPAREN _)) -> Just (Reduce 8 7)
    (27, Token (COMMA _)) -> Just (Reduce 1 30)
    (28, Token (LPAREN _)) -> Just (Shift 28)
    (28, Token (ANY _)) -> Just (Shift 34)
    (28, Token (CHAR _)) -> Just (Shift 35)
    (28, Token (STRING _)) -> Just (Shift 36)
    (28, Token (GENERAL_CATEGORY _)) -> Just (Shift 37)
    (28, Token (LBRACKET _)) -> Just (Shift 38)
    (29, Token (RPAREN _)) -> Just (Reduce 1 29)
    (30, Token (PP _)) -> Just (Reduce 1 31)
    (30, Token (CASE _)) -> Just (Reduce 1 31)
    (30, Token (LPAREN _)) -> Just (Reduce 1 31)
    (31, Token (LPAREN _)) -> Just (Shift 28)
    (31, Token (RPAREN _)) -> Just (Reduce 1 25)
    (31, Token (ANY _)) -> Just (Shift 34)
    (31, Token (CHAR _)) -> Just (Shift 35)
    (31, Token (STRING _)) -> Just (Shift 36)
    (31, Token (GENERAL_CATEGORY _)) -> Just (Shift 37)
    (31, Token (LBRACKET _)) -> Just (Shift 38)
    (31, Token (PIPE _)) -> Just (Reduce 1 25)
    (31, Token (STAR _)) -> Just (Shift 54)
    (31, Token (PLUS _)) -> Just (Shift 55)
    (31, Token (QUES _)) -> Just (Shift 56)
    (32, Token (LPAREN _)) -> Just (Shift 28)
    (32, Token (ANY _)) -> Just (Shift 34)
    (32, Token (CHAR _)) -> Just (Shift 35)
    (32, Token (STRING _)) -> Just (Shift 36)
    (32, Token (GENERAL_CATEGORY _)) -> Just (Shift 37)
    (32, Token (LBRACKET _)) -> Just (Shift 38)
    (33, Token (LPAREN _)) -> Just (Reduce 3 12)
    (33, Token (RPAREN _)) -> Just (Reduce 3 12)
    (33, Token (ANY _)) -> Just (Reduce 3 12)
    (33, Token (CHAR _)) -> Just (Reduce 3 12)
    (33, Token (STRING _)) -> Just (Reduce 3 12)
    (33, Token (GENERAL_CATEGORY _)) -> Just (Reduce 3 12)
    (33, Token (LBRACKET _)) -> Just (Reduce 3 12)
    (33, Token (HAT _)) -> Just (Reduce 3 12)
    (33, Token (PIPE _)) -> Just (Reduce 3 12)
    (33, Token (HYPHEN _)) -> Just (Reduce 3 12)
    (33, Token (STAR _)) -> Just (Reduce 3 12)
    (33, Token (PLUS _)) -> Just (Reduce 3 12)
    (33, Token (QUES _)) -> Just (Reduce 3 12)
    (34, Token (LPAREN _)) -> Just (Reduce 1 8)
    (34, Token (RPAREN _)) -> Just (Reduce 1 8)
    (34, Token (ANY _)) -> Just (Reduce 1 8)
    (34, Token (CHAR _)) -> Just (Reduce 1 8)
    (34, Token (STRING _)) -> Just (Reduce 1 8)
    (34, Token (GENERAL_CATEGORY _)) -> Just (Reduce 1 8)
    (34, Token (LBRACKET _)) -> Just (Reduce 1 8)
    (34, Token (HAT _)) -> Just (Reduce 1 8)
    (34, Token (PIPE _)) -> Just (Reduce 1 8)
    (34, Token (HYPHEN _)) -> Just (Reduce 1 8)
    (34, Token (STAR _)) -> Just (Reduce 1 8)
    (34, Token (PLUS _)) -> Just (Reduce 1 8)
    (34, Token (QUES _)) -> Just (Reduce 1 8)
    (35, Token (LPAREN _)) -> Just (Reduce 1 9)
    (35, Token (RPAREN _)) -> Just (Reduce 1 9)
    (35, Token (ANY _)) -> Just (Reduce 1 9)
    (35, Token (CHAR _)) -> Just (Reduce 1 9)
    (35, Token (STRING _)) -> Just (Reduce 1 9)
    (35, Token (GENERAL_CATEGORY _)) -> Just (Reduce 1 9)
    (35, Token (LBRACKET _)) -> Just (Reduce 1 9)
    (35, Token (HAT _)) -> Just (Reduce 1 9)
    (35, Token (PIPE _)) -> Just (Reduce 1 9)
    (35, Token (HYPHEN _)) -> Just (Reduce 1 9)
    (35, Token (STAR _)) -> Just (Reduce 1 9)
    (35, Token (PLUS _)) -> Just (Reduce 1 9)
    (35, Token (QUES _)) -> Just (Reduce 1 9)
    (36, Token (LPAREN _)) -> Just (Reduce 1 10)
    (36, Token (RPAREN _)) -> Just (Reduce 1 10)
    (36, Token (ANY _)) -> Just (Reduce 1 10)
    (36, Token (CHAR _)) -> Just (Reduce 1 10)
    (36, Token (STRING _)) -> Just (Reduce 1 10)
    (36, Token (GENERAL_CATEGORY _)) -> Just (Reduce 1 10)
    (36, Token (LBRACKET _)) -> Just (Reduce 1 10)
    (36, Token (HAT _)) -> Just (Reduce 1 10)
    (36, Token (PIPE _)) -> Just (Reduce 1 10)
    (36, Token (HYPHEN _)) -> Just (Reduce 1 10)
    (36, Token (STAR _)) -> Just (Reduce 1 10)
    (36, Token (PLUS _)) -> Just (Reduce 1 10)
    (36, Token (QUES _)) -> Just (Reduce 1 10)
    (37, Token (LPAREN _)) -> Just (Reduce 1 11)
    (37, Token (RPAREN _)) -> Just (Reduce 1 11)
    (37, Token (ANY _)) -> Just (Reduce 1 11)
    (37, Token (CHAR _)) -> Just (Reduce 1 11)
    (37, Token (STRING _)) -> Just (Reduce 1 11)
    (37, Token (GENERAL_CATEGORY _)) -> Just (Reduce 1 11)
    (37, Token (LBRACKET _)) -> Just (Reduce 1 11)
    (37, Token (HAT _)) -> Just (Reduce 1 11)
    (37, Token (PIPE _)) -> Just (Reduce 1 11)
    (37, Token (HYPHEN _)) -> Just (Reduce 1 11)
    (37, Token (STAR _)) -> Just (Reduce 1 11)
    (37, Token (PLUS _)) -> Just (Reduce 1 11)
    (37, Token (QUES _)) -> Just (Reduce 1 11)
    (38, Token (CHAR _)) -> Just (Shift 48)
    (38, Token (HAT _)) -> Just (Shift 41)
    (39, Token (LPAREN _)) -> Just (Reduce 3 13)
    (39, Token (RPAREN _)) -> Just (Reduce 3 13)
    (39, Token (ANY _)) -> Just (Reduce 3 13)
    (39, Token (CHAR _)) -> Just (Reduce 3 13)
    (39, Token (STRING _)) -> Just (Reduce 3 13)
    (39, Token (GENERAL_CATEGORY _)) -> Just (Reduce 3 13)
    (39, Token (LBRACKET _)) -> Just (Reduce 3 13)
    (39, Token (HAT _)) -> Just (Reduce 3 13)
    (39, Token (PIPE _)) -> Just (Reduce 3 13)
    (39, Token (HYPHEN _)) -> Just (Reduce 3 13)
    (39, Token (STAR _)) -> Just (Reduce 3 13)
    (39, Token (PLUS _)) -> Just (Reduce 3 13)
    (39, Token (QUES _)) -> Just (Reduce 3 13)
    (40, Token (LPAREN _)) -> Just (Reduce 4 14)
    (40, Token (RPAREN _)) -> Just (Reduce 4 14)
    (40, Token (ANY _)) -> Just (Reduce 4 14)
    (40, Token (CHAR _)) -> Just (Reduce 4 14)
    (40, Token (STRING _)) -> Just (Reduce 4 14)
    (40, Token (GENERAL_CATEGORY _)) -> Just (Reduce 4 14)
    (40, Token (LBRACKET _)) -> Just (Reduce 4 14)
    (40, Token (HAT _)) -> Just (Reduce 4 14)
    (40, Token (PIPE _)) -> Just (Reduce 4 14)
    (40, Token (HYPHEN _)) -> Just (Reduce 4 14)
    (40, Token (STAR _)) -> Just (Reduce 4 14)
    (40, Token (PLUS _)) -> Just (Reduce 4 14)
    (40, Token (QUES _)) -> Just (Reduce 4 14)
    (41, Token (CHAR _)) -> Just (Shift 48)
    (42, Token (RPAREN _)) -> Just (Shift 33)
    (43, Token (RBRACKET _)) -> Just (Shift 39)
    (44, Token (RBRACKET _)) -> Just (Shift 40)
    (45, Token (CHAR _)) -> Just (Shift 48)
    (46, Token (RBRACKET _)) -> Just (Reduce 3 16)
    (47, Token (RBRACKET _)) -> Just (Reduce 1 15)
    (47, Token (PIPE _)) -> Just (Shift 45)
    (48, Token (CHAR _)) -> Just (Shift 52)
    (48, Token (RBRACKET _)) -> Just (Reduce 1 19)
    (48, Token (PIPE _)) -> Just (Reduce 1 19)
    (48, Token (HYPHEN _)) -> Just (Shift 50)
    (49, Token (RBRACKET _)) -> Just (Reduce 3 18)
    (49, Token (PIPE _)) -> Just (Reduce 3 18)
    (50, Token (CHAR _)) -> Just (Shift 49)
    (51, Token (RBRACKET _)) -> Just (Reduce 1 17)
    (51, Token (PIPE _)) -> Just (Reduce 1 17)
    (52, Token (CHAR _)) -> Just (Shift 52)
    (52, Token (RBRACKET _)) -> Just (Reduce 1 19)
    (52, Token (PIPE _)) -> Just (Reduce 1 19)
    (53, Token (RBRACKET _)) -> Just (Reduce 2 20)
    (53, Token (PIPE _)) -> Just (Reduce 2 20)
    (54, Token (LPAREN _)) -> Just (Reduce 2 22)
    (54, Token (RPAREN _)) -> Just (Reduce 2 22)
    (54, Token (ANY _)) -> Just (Reduce 2 22)
    (54, Token (CHAR _)) -> Just (Reduce 2 22)
    (54, Token (STRING _)) -> Just (Reduce 2 22)
    (54, Token (GENERAL_CATEGORY _)) -> Just (Reduce 2 22)
    (54, Token (LBRACKET _)) -> Just (Reduce 2 22)
    (54, Token (HAT _)) -> Just (Reduce 2 22)
    (54, Token (PIPE _)) -> Just (Reduce 2 22)
    (54, Token (HYPHEN _)) -> Just (Reduce 2 22)
    (54, Token (STAR _)) -> Just (Reduce 2 22)
    (54, Token (PLUS _)) -> Just (Reduce 2 22)
    (54, Token (QUES _)) -> Just (Reduce 2 22)
    (55, Token (LPAREN _)) -> Just (Reduce 2 23)
    (55, Token (RPAREN _)) -> Just (Reduce 2 23)
    (55, Token (ANY _)) -> Just (Reduce 2 23)
    (55, Token (CHAR _)) -> Just (Reduce 2 23)
    (55, Token (STRING _)) -> Just (Reduce 2 23)
    (55, Token (GENERAL_CATEGORY _)) -> Just (Reduce 2 23)
    (55, Token (LBRACKET _)) -> Just (Reduce 2 23)
    (55, Token (HAT _)) -> Just (Reduce 2 23)
    (55, Token (PIPE _)) -> Just (Reduce 2 23)
    (55, Token (HYPHEN _)) -> Just (Reduce 2 23)
    (55, Token (STAR _)) -> Just (Reduce 2 23)
    (55, Token (PLUS _)) -> Just (Reduce 2 23)
    (55, Token (QUES _)) -> Just (Reduce 2 23)
    (56, Token (LPAREN _)) -> Just (Reduce 2 24)
    (56, Token (RPAREN _)) -> Just (Reduce 2 24)
    (56, Token (ANY _)) -> Just (Reduce 2 24)
    (56, Token (CHAR _)) -> Just (Reduce 2 24)
    (56, Token (STRING _)) -> Just (Reduce 2 24)
    (56, Token (GENERAL_CATEGORY _)) -> Just (Reduce 2 24)
    (56, Token (LBRACKET _)) -> Just (Reduce 2 24)
    (56, Token (HAT _)) -> Just (Reduce 2 24)
    (56, Token (PIPE _)) -> Just (Reduce 2 24)
    (56, Token (HYPHEN _)) -> Just (Reduce 2 24)
    (56, Token (STAR _)) -> Just (Reduce 2 24)
    (56, Token (PLUS _)) -> Just (Reduce 2 24)
    (56, Token (QUES _)) -> Just (Reduce 2 24)
    (57, Token (LPAREN _)) -> Just (Reduce 1 21)
    (57, Token (RPAREN _)) -> Just (Reduce 1 21)
    (57, Token (ANY _)) -> Just (Reduce 1 21)
    (57, Token (CHAR _)) -> Just (Reduce 1 21)
    (57, Token (STRING _)) -> Just (Reduce 1 21)
    (57, Token (GENERAL_CATEGORY _)) -> Just (Reduce 1 21)
    (57, Token (LBRACKET _)) -> Just (Reduce 1 21)
    (57, Token (HAT _)) -> Just (Reduce 1 21)
    (57, Token (PIPE _)) -> Just (Reduce 1 21)
    (57, Token (HYPHEN _)) -> Just (Reduce 1 21)
    (57, Token (STAR _)) -> Just (Reduce 1 21)
    (57, Token (PLUS _)) -> Just (Reduce 1 21)
    (57, Token (QUES _)) -> Just (Reduce 1 21)
    (58, Token (RPAREN _)) -> Just (Reduce 2 26)
    (58, Token (PIPE _)) -> Just (Reduce 2 26)
    (59, Token (RPAREN _)) -> Just (Reduce 1 27)
    (59, Token (PIPE _)) -> Just (Shift 32)
    (60, Token (RPAREN _)) -> Just (Reduce 3 28)
    (61, EOF) -> Just (Reduce 1 34)
    (61, Token (PWHERE _)) -> Just (Reduce 1 34)
    (61, Token (PRBRACE _)) -> Just (Reduce 1 34)
    (61, Token (CODE _)) -> Just (Reduce 1 34)
    (_, _) -> Nothing

production :: Int -> Int
production 0 = 0
production 1 = 1
production 2 = 1
production 3 = 4
production 4 = 4
production 5 = 2
production 6 = 2
production 7 = 5
production 8 = 9
production 9 = 9
production 10 = 9
production 11 = 9
production 12 = 9
production 13 = 9
production 14 = 9
production 15 = 10
production 16 = 10
production 17 = 11
production 18 = 11
production 19 = 12
production 20 = 12
production 21 = 13
production 22 = 13
production 23 = 13
production 24 = 13
production 25 = 14
production 26 = 14
production 27 = 15
production 28 = 15
production 29 = 7
production 30 = 6
production 31 = 8
production 32 = 3
production 33 = 3
production 34 = 16

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 1) -> Just 4
    (0, 4) -> Just 7
    (2, 2) -> Just 5
    (2, 5) -> Just 9
    (3, 3) -> Just 6
    (3, 16) -> Just 11
    (7, 1) -> Just 8
    (7, 4) -> Just 7
    (9, 2) -> Just 10
    (9, 5) -> Just 9
    (11, 3) -> Just 14
    (11, 16) -> Just 11
    (12, 3) -> Just 17
    (12, 16) -> Just 11
    (13, 3) -> Just 18
    (13, 16) -> Just 11
    (20, 6) -> Just 24
    (21, 7) -> Just 25
    (21, 9) -> Just 57
    (21, 13) -> Just 31
    (21, 14) -> Just 59
    (21, 15) -> Just 29
    (23, 8) -> Just 26
    (28, 7) -> Just 42
    (28, 9) -> Just 57
    (28, 13) -> Just 31
    (28, 14) -> Just 59
    (28, 15) -> Just 29
    (31, 9) -> Just 57
    (31, 13) -> Just 31
    (31, 14) -> Just 58
    (32, 9) -> Just 57
    (32, 13) -> Just 31
    (32, 14) -> Just 59
    (32, 15) -> Just 60
    (38, 10) -> Just 43
    (38, 11) -> Just 47
    (38, 12) -> Just 51
    (41, 10) -> Just 44
    (41, 11) -> Just 47
    (41, 12) -> Just 51
    (45, 10) -> Just 46
    (45, 11) -> Just 47
    (45, 12) -> Just 51
    (48, 12) -> Just 53
    (52, 12) -> Just 53
    (_, _) -> Nothing

parse :: Monad m => SemanticActions m -> [Token] -> m (Maybe (Start, [Token]))
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
                  Token (PP semanticValue) ->
                    StackValue_PP semanticValue
                  Token (PMODULE semanticValue) ->
                    StackValue_PMODULE semanticValue
                  Token (PWHERE semanticValue) ->
                    StackValue_PWHERE semanticValue
                  Token (PLBRACE semanticValue) ->
                    StackValue_PLBRACE semanticValue
                  Token (PRBRACE semanticValue) ->
                    StackValue_PRBRACE semanticValue
                  Token (CASE semanticValue) ->
                    StackValue_CASE semanticValue
                  Token (LPAREN semanticValue) ->
                    StackValue_LPAREN semanticValue
                  Token (COMMA semanticValue) ->
                    StackValue_COMMA semanticValue
                  Token (RPAREN semanticValue) ->
                    StackValue_RPAREN semanticValue
                  Token (DARROW semanticValue) ->
                    StackValue_DARROW semanticValue
                  Token (ANY semanticValue) ->
                    StackValue_ANY semanticValue
                  Token (CHAR semanticValue) ->
                    StackValue_CHAR semanticValue
                  Token (STRING semanticValue) ->
                    StackValue_STRING semanticValue
                  Token (GENERAL_CATEGORY semanticValue) ->
                    StackValue_GENERAL_CATEGORY semanticValue
                  Token (LBRACKET semanticValue) ->
                    StackValue_LBRACKET semanticValue
                  Token (RBRACKET semanticValue) ->
                    StackValue_RBRACKET semanticValue
                  Token (HAT semanticValue) ->
                    StackValue_HAT semanticValue
                  Token (PIPE semanticValue) ->
                    StackValue_PIPE semanticValue
                  Token (HYPHEN semanticValue) ->
                    StackValue_HYPHEN semanticValue
                  Token (STAR semanticValue) ->
                    StackValue_STAR semanticValue
                  Token (PLUS semanticValue) ->
                    StackValue_PLUS semanticValue
                  Token (QUES semanticValue) ->
                    StackValue_QUES semanticValue
                  Token (LEXING_STATE semanticValue) ->
                    StackValue_LEXING_STATE semanticValue
                  Token (SEMANTIC_ACTION semanticValue) ->
                    StackValue_SEMANTIC_ACTION semanticValue
                  Token (CODE semanticValue) ->
                    StackValue_CODE semanticValue
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
                      Monad.liftM StackValue_start $ start_implies_definitions_PP_cases_PP_codes actions (case snd (pop !! 4) of { StackValue_definitions value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_PP value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_cases value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PP value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_codes value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_definitions $ definitions_implies actions
                    2 ->
                      Monad.liftM StackValue_definitions $ definitions_implies_definition_definitions actions (case snd (pop !! 1) of { StackValue_definition value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_definitions value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_definition $ definition_implies_PMODULE_codes_PWHERE actions (case snd (pop !! 2) of { StackValue_PMODULE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_codes value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_PWHERE value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_definition $ definition_implies_PLBRACE_codes_PRBRACE actions (case snd (pop !! 2) of { StackValue_PLBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_codes value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_PRBRACE value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_cases $ cases_implies actions
                    6 ->
                      Monad.liftM StackValue_cases $ cases_implies_case_cases actions (case snd (pop !! 1) of { StackValue_case value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cases value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_case $ case_implies_CASE_LPAREN_lexingState_COMMA_exp_RPAREN_DARROW_semanticAction actions (case snd (pop !! 7) of { StackValue_CASE value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_lexingState value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_RPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_semanticAction value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_ANY actions (case snd (pop !! 0) of { StackValue_ANY value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_CHAR actions (case snd (pop !! 0) of { StackValue_CHAR value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_GENERAL_CATEGORY actions (case snd (pop !! 0) of { StackValue_GENERAL_CATEGORY value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_LBRACKET_charsets_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_charsets value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_LBRACKET_HAT_charsets_RBRACKET actions (case snd (pop !! 3) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_HAT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_charsets value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_charsets $ charsets_implies_charset actions (case snd (pop !! 0) of { StackValue_charset value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_charsets $ charsets_implies_charset_PIPE_charsets actions (case snd (pop !! 2) of { StackValue_charset value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_charsets value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_charset $ charset_implies_chars actions (case snd (pop !! 0) of { StackValue_chars value -> value; _ -> undefined })
                    18 ->
                      Monad.liftM StackValue_charset $ charset_implies_CHAR_HYPHEN_CHAR actions (case snd (pop !! 2) of { StackValue_CHAR value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_HYPHEN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_CHAR value -> value; _ -> undefined })
                    19 ->
                      Monad.liftM StackValue_chars $ chars_implies_CHAR actions (case snd (pop !! 0) of { StackValue_CHAR value -> value; _ -> undefined })
                    20 ->
                      Monad.liftM StackValue_chars $ chars_implies_CHAR_chars actions (case snd (pop !! 1) of { StackValue_CHAR value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_chars value -> value; _ -> undefined })
                    21 ->
                      Monad.liftM StackValue_suffixexp $ suffixexp_implies_atexp actions (case snd (pop !! 0) of { StackValue_atexp value -> value; _ -> undefined })
                    22 ->
                      Monad.liftM StackValue_suffixexp $ suffixexp_implies_suffixexp_STAR actions (case snd (pop !! 1) of { StackValue_suffixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_STAR value -> value; _ -> undefined })
                    23 ->
                      Monad.liftM StackValue_suffixexp $ suffixexp_implies_suffixexp_PLUS actions (case snd (pop !! 1) of { StackValue_suffixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_PLUS value -> value; _ -> undefined })
                    24 ->
                      Monad.liftM StackValue_suffixexp $ suffixexp_implies_suffixexp_QUES actions (case snd (pop !! 1) of { StackValue_suffixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_QUES value -> value; _ -> undefined })
                    25 ->
                      Monad.liftM StackValue_catexp $ catexp_implies_suffixexp actions (case snd (pop !! 0) of { StackValue_suffixexp value -> value; _ -> undefined })
                    26 ->
                      Monad.liftM StackValue_catexp $ catexp_implies_suffixexp_catexp actions (case snd (pop !! 1) of { StackValue_suffixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_catexp value -> value; _ -> undefined })
                    27 ->
                      Monad.liftM StackValue_orexp $ orexp_implies_catexp actions (case snd (pop !! 0) of { StackValue_catexp value -> value; _ -> undefined })
                    28 ->
                      Monad.liftM StackValue_orexp $ orexp_implies_catexp_PIPE_orexp actions (case snd (pop !! 2) of { StackValue_catexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_orexp value -> value; _ -> undefined })
                    29 ->
                      Monad.liftM StackValue_exp $ exp_implies_orexp actions (case snd (pop !! 0) of { StackValue_orexp value -> value; _ -> undefined })
                    30 ->
                      Monad.liftM StackValue_lexingState $ lexingState_implies_LEXING_STATE actions (case snd (pop !! 0) of { StackValue_LEXING_STATE value -> value; _ -> undefined })
                    31 ->
                      Monad.liftM StackValue_semanticAction $ semanticAction_implies_SEMANTIC_ACTION actions (case snd (pop !! 0) of { StackValue_SEMANTIC_ACTION value -> value; _ -> undefined })
                    32 ->
                      Monad.liftM StackValue_codes $ codes_implies actions
                    33 ->
                      Monad.liftM StackValue_codes $ codes_implies_code_codes actions (case snd (pop !! 1) of { StackValue_code value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_codes value -> value; _ -> undefined })
                    34 ->
                      Monad.liftM StackValue_code $ code_implies_CODE actions (case snd (pop !! 0) of { StackValue_CODE value -> value; _ -> undefined })
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_start value)] -> return $ Just (value, tokens); _ -> return Nothing }



charsetsToRegexp :: Charsets -> Regexp
charsetsToRegexp = foldr (HsLex.|||) HsLex.empty . map charsetToRegexp
  where
    charsetToRegexp (Chars chars) = HsLex.oneOf chars
    charsetToRegexp (Range c1 c2) = HsLex.to c1 c2

charsetsToNotRegexp :: Charsets -> Regexp
charsetsToNotRegexp = HsLex.noneOf . allChars
  where
    allChars = concat . map allChars'
    allChars' (Chars chars) = chars
    allChars' (Range c1 c2) = [c1 .. c2]

semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { start_implies_definitions_PP_cases_PP_codes = \defns () cases () codes ->
      return (defns, cases, codes)
  , definitions_implies =
      return []
  , definitions_implies_definition_definitions = \defn defns ->
      return $ defn : defns
  , definition_implies_PMODULE_codes_PWHERE = \() codes () ->
      return $ DefnModule codes
  , definition_implies_PLBRACE_codes_PRBRACE = \() codes () ->
      return $ DefnCodes codes
  , cases_implies =
      return []
  , cases_implies_case_cases = \case' cases ->
      return $ case' : cases
  , case_implies_CASE_LPAREN_lexingState_COMMA_exp_RPAREN_DARROW_semanticAction = \() () lexingState () regexp () () sa ->
      return (lexingState, regexp, sa)
  , atexp_implies_ANY =
      const $ return $ HsLex.any
  , atexp_implies_CHAR =
      return . HsLex.char
  , atexp_implies_STRING =
      return . HsLex.string
  , atexp_implies_GENERAL_CATEGORY =
      return . HsLex.generalCategory
  , atexp_implies_LPAREN_exp_RPAREN = \() regexp () ->
      return regexp
  , atexp_implies_LBRACKET_charsets_RBRACKET = \() charsets () ->
      return $ charsetsToRegexp charsets
  , atexp_implies_LBRACKET_HAT_charsets_RBRACKET = \() () charsets () ->
      return $ charsetsToNotRegexp charsets
  , charsets_implies_charset = \charset ->
      return [charset]
  , charsets_implies_charset_PIPE_charsets = \charset () charsets ->
      return $ charset : charsets
  , charset_implies_chars = \chars ->
      return $ Chars chars
  , charset_implies_CHAR_HYPHEN_CHAR = \c1 () c2 ->
      return $ Range c1 c2
  , chars_implies_CHAR = \char ->
      return [char]
  , chars_implies_CHAR_chars = \char chars ->
      return $ char : chars
  , suffixexp_implies_atexp =
      return
  , suffixexp_implies_suffixexp_STAR = \regexp () ->
      return $ HsLex.many regexp
  , suffixexp_implies_suffixexp_PLUS = \regexp () ->
      return $ HsLex.some regexp
  , suffixexp_implies_suffixexp_QUES = \regexp () ->
      return $ HsLex.optional regexp
  , catexp_implies_suffixexp =
      return
  , catexp_implies_suffixexp_catexp = \regexp1 regexp2 ->
      return (regexp1 HsLex.>>> regexp2)
  , orexp_implies_catexp =
      return
  , orexp_implies_catexp_PIPE_orexp = \regexp1 () regexp2 ->
      return (regexp1 HsLex.||| regexp2)
  , exp_implies_orexp =
      return
  , lexingState_implies_LEXING_STATE =
      return
  , semanticAction_implies_SEMANTIC_ACTION =
      return
  , codes_implies =
      return []
  , codes_implies_code_codes = \code codes ->
      return $ code : codes
  , code_implies_CODE =
      return }


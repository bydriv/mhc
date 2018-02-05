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
  deriving (Eq, Ord, Read, Show)
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
  let s' =
        case s of
          EOF -> -1
          Token (ANY _) -> 10
          Token (CASE _) -> 5
          Token (CHAR _) -> 11
          Token (CODE _) -> 24
          Token (COMMA _) -> 7
          Token (DARROW _) -> 9
          Token (GENERAL_CATEGORY _) -> 13
          Token (HAT _) -> 16
          Token (HYPHEN _) -> 18
          Token (LBRACKET _) -> 14
          Token (LEXING_STATE _) -> 22
          Token (LPAREN _) -> 6
          Token (PIPE _) -> 17
          Token (PLBRACE _) -> 3
          Token (PLUS _) -> 20
          Token (PMODULE _) -> 1
          Token (PP _) -> 0
          Token (PRBRACE _) -> 4
          Token (PWHERE _) -> 2
          Token (QUES _) -> 21
          Token (RBRACKET _) -> 15
          Token (RPAREN _) -> 8
          Token (SEMANTIC_ACTION _) -> 23
          Token (STAR _) -> 19
          Token (STRING _) -> 12
  in
    case compare (q, s') (52, 15) of {
      LT ->
    case compare (q, s') (39, 14) of {
      LT ->
    case compare (q, s') (36, 8) of {
      LT ->
    case compare (q, s') (32, 13) of {
      LT ->
    case compare (q, s') (13, 24) of {
      LT ->
    case compare (q, s') (9, 0) of {
      LT ->
    case compare (q, s') (3, 24) of {
      LT ->
    case compare (q, s') (1, -1) of {
      LT ->
    case compare (q, s') (0, 1) of {
      LT ->
    case compare (q, s') (0, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 0 1);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 12);
      GT ->
    case compare (q, s') (0, 3) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 13);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Accept);
      GT ->
    case compare (q, s') (2, 5) of {
      LT ->
    case compare (q, s') (2, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 0 5);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 19);
      GT ->
    case compare (q, s') (3, -1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 0 32);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Shift 61);
      GT ->
    case compare (q, s') (7, 3) of {
      LT ->
    case compare (q, s') (7, 0) of {
      LT ->
    case compare (q, s') (5, 0) of {
      LT ->
    case compare (q, s') (4, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 2);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 3);
      GT ->
    case compare (q, s') (6, -1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 5 0);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 0 1);
      GT ->
    case compare (q, s') (7, 1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 12);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 13);
      GT ->
    case compare (q, s') (8, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 2);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 0 5);
      GT ->
    case compare (q, s') (11, 4) of {
      LT ->
    case compare (q, s') (11, -1) of {
      LT ->
    case compare (q, s') (9, 5) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 19);
      GT ->
    case compare (q, s') (10, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 6);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 0 32);
      GT ->
    case compare (q, s') (11, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 0 32);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 0 32);
      GT ->
    case compare (q, s') (12, 24) of {
      LT ->
    case compare (q, s') (11, 24) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 61);
      GT ->
    case compare (q, s') (12, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 0 32);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 61);
      GT ->
    case compare (q, s') (13, 4) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 0 32);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Shift 61);
      GT ->
    case compare (q, s') (20, 22) of {
      LT ->
    case compare (q, s') (16, 3) of {
      LT ->
    case compare (q, s') (15, 3) of {
      LT ->
    case compare (q, s') (15, 0) of {
      LT ->
    case compare (q, s') (14, 2) of {
      LT ->
    case compare (q, s') (14, -1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 33);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 33);
      GT ->
    case compare (q, s') (14, 4) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 33);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 3 3);
      GT ->
    case compare (q, s') (15, 1) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 3);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 3 3);
      GT ->
    case compare (q, s') (16, 0) of {
      LT ->
    case compare (q, s') (15, 24) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 3);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 4);
      GT ->
    case compare (q, s') (16, 1) of {
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
    case compare (q, s') (17, 2) of {
      LT ->
    case compare (q, s') (16, 24) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 4);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 15);
      GT ->
    case compare (q, s') (18, 4) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 16);
      GT ->
    case compare (q, s') (19, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 20);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Shift 27);
      GT ->
    case compare (q, s') (31, 8) of {
      LT ->
    case compare (q, s') (28, 13) of {
      LT ->
    case compare (q, s') (27, 7) of {
      LT ->
    case compare (q, s') (23, 23) of {
      LT ->
    case compare (q, s') (21, 12) of {
      LT ->
    case compare (q, s') (21, 10) of {
      LT ->
    case compare (q, s') (21, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 28);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 34);
      GT ->
    case compare (q, s') (21, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 35);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 36);
      GT ->
    case compare (q, s') (21, 14) of {
      LT ->
    case compare (q, s') (21, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 37);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 38);
      GT ->
    case compare (q, s') (22, 9) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 23);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Shift 30);
      GT ->
    case compare (q, s') (26, 5) of {
      LT ->
    case compare (q, s') (25, 8) of {
      LT ->
    case compare (q, s') (24, 7) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 21);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 22);
      GT ->
    case compare (q, s') (26, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 8 7);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 8 7);
      GT ->
    case compare (q, s') (26, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 8 7);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 1 30);
      GT ->
    case compare (q, s') (28, 10) of {
      LT ->
    case compare (q, s') (28, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 28);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 34);
      GT ->
    case compare (q, s') (28, 12) of {
      LT ->
    case compare (q, s') (28, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 35);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 36);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Shift 37);
      GT ->
    case compare (q, s') (30, 6) of {
      LT ->
    case compare (q, s') (30, 0) of {
      LT ->
    case compare (q, s') (28, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 38);
      GT ->
    case compare (q, s') (29, 8) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 29);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 31);
      GT ->
    case compare (q, s') (30, 5) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 31);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 31);
      GT ->
    case compare (q, s') (31, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 28);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 1 25);
      GT ->
    case compare (q, s') (31, 14) of {
      LT ->
    case compare (q, s') (31, 11) of {
      LT ->
    case compare (q, s') (31, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 34);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 35);
      GT ->
    case compare (q, s') (31, 13) of {
      LT ->
    case compare (q, s') (31, 12) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 36);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 37);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 38);
      GT ->
    case compare (q, s') (31, 19) of {
      LT ->
    case compare (q, s') (31, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 25);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 54);
      GT ->
    case compare (q, s') (32, 10) of {
      LT ->
    case compare (q, s') (31, 21) of {
      LT ->
    case compare (q, s') (31, 20) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 55);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 56);
      GT ->
    case compare (q, s') (32, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 28);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 34);
      GT ->
    case compare (q, s') (32, 12) of {
      LT ->
    case compare (q, s') (32, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 35);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 36);
      GT ->
        Nothing
    }
    }
    }
    }
    }
    }
    };
      EQ ->
        Just (Shift 37);
      GT ->
    case compare (q, s') (34, 20) of {
      LT ->
    case compare (q, s') (34, 11) of {
      LT ->
    case compare (q, s') (33, 17) of {
      LT ->
    case compare (q, s') (33, 12) of {
      LT ->
    case compare (q, s') (33, 10) of {
      LT ->
    case compare (q, s') (33, 6) of {
      LT ->
    case compare (q, s') (32, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 38);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 12);
      GT ->
    case compare (q, s') (33, 8) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 12);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 3 12);
      GT ->
    case compare (q, s') (33, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 12);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 3 12);
      GT ->
    case compare (q, s') (33, 14) of {
      LT ->
    case compare (q, s') (33, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 12);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 12);
      GT ->
    case compare (q, s') (33, 16) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 12);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 3 12);
      GT ->
    case compare (q, s') (33, 21) of {
      LT ->
    case compare (q, s') (33, 19) of {
      LT ->
    case compare (q, s') (33, 18) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 12);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 12);
      GT ->
    case compare (q, s') (33, 20) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 12);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 3 12);
      GT ->
    case compare (q, s') (34, 8) of {
      LT ->
    case compare (q, s') (34, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 8);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 8);
      GT ->
    case compare (q, s') (34, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 8);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Reduce 1 8);
      GT ->
    case compare (q, s') (34, 16) of {
      LT ->
    case compare (q, s') (34, 13) of {
      LT ->
    case compare (q, s') (34, 12) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 8);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 8);
      GT ->
    case compare (q, s') (34, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 8);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 8);
      GT ->
    case compare (q, s') (34, 18) of {
      LT ->
    case compare (q, s') (34, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 8);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 8);
      GT ->
    case compare (q, s') (34, 19) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 8);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Reduce 1 8);
      GT ->
    case compare (q, s') (35, 14) of {
      LT ->
    case compare (q, s') (35, 10) of {
      LT ->
    case compare (q, s') (35, 6) of {
      LT ->
    case compare (q, s') (34, 21) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 8);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 9);
      GT ->
    case compare (q, s') (35, 8) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 9);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 9);
      GT ->
    case compare (q, s') (35, 12) of {
      LT ->
    case compare (q, s') (35, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 9);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 9);
      GT ->
    case compare (q, s') (35, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 9);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 1 9);
      GT ->
    case compare (q, s') (35, 19) of {
      LT ->
    case compare (q, s') (35, 17) of {
      LT ->
    case compare (q, s') (35, 16) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 9);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 9);
      GT ->
    case compare (q, s') (35, 18) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 9);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 9);
      GT ->
    case compare (q, s') (35, 21) of {
      LT ->
    case compare (q, s') (35, 20) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 9);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 9);
      GT ->
    case compare (q, s') (36, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 10);
      GT ->
        Nothing
    }
    }
    }
    }
    }
    };
      EQ ->
        Just (Reduce 1 10);
      GT ->
    case compare (q, s') (37, 12) of {
      LT ->
    case compare (q, s') (36, 18) of {
      LT ->
    case compare (q, s') (36, 13) of {
      LT ->
    case compare (q, s') (36, 11) of {
      LT ->
    case compare (q, s') (36, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 10);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 10);
      GT ->
    case compare (q, s') (36, 12) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 10);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 10);
      GT ->
    case compare (q, s') (36, 16) of {
      LT ->
    case compare (q, s') (36, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 10);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 10);
      GT ->
    case compare (q, s') (36, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 10);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 1 10);
      GT ->
    case compare (q, s') (37, 6) of {
      LT ->
    case compare (q, s') (36, 20) of {
      LT ->
    case compare (q, s') (36, 19) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 10);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 10);
      GT ->
    case compare (q, s') (36, 21) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 10);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 11);
      GT ->
    case compare (q, s') (37, 10) of {
      LT ->
    case compare (q, s') (37, 8) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 11);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 11);
      GT ->
    case compare (q, s') (37, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 11);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Reduce 1 11);
      GT ->
    case compare (q, s') (37, 21) of {
      LT ->
    case compare (q, s') (37, 17) of {
      LT ->
    case compare (q, s') (37, 14) of {
      LT ->
    case compare (q, s') (37, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 11);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 11);
      GT ->
    case compare (q, s') (37, 16) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 11);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 11);
      GT ->
    case compare (q, s') (37, 19) of {
      LT ->
    case compare (q, s') (37, 18) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 11);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 11);
      GT ->
    case compare (q, s') (37, 20) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 11);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 1 11);
      GT ->
    case compare (q, s') (39, 10) of {
      LT ->
    case compare (q, s') (38, 16) of {
      LT ->
    case compare (q, s') (38, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 48);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 41);
      GT ->
    case compare (q, s') (39, 8) of {
      LT ->
    case compare (q, s') (39, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 13);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 13);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 3 13);
      GT ->
    case compare (q, s') (39, 12) of {
      LT ->
    case compare (q, s') (39, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 13);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 13);
      GT ->
    case compare (q, s') (39, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 13);
      GT ->
        Nothing
    }
    }
    }
    }
    }
    };
      EQ ->
        Just (Reduce 3 13);
      GT ->
    case compare (q, s') (40, 18) of {
      LT ->
    case compare (q, s') (40, 8) of {
      LT ->
    case compare (q, s') (39, 19) of {
      LT ->
    case compare (q, s') (39, 17) of {
      LT ->
    case compare (q, s') (39, 16) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 13);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 13);
      GT ->
    case compare (q, s') (39, 18) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 13);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 3 13);
      GT ->
    case compare (q, s') (39, 21) of {
      LT ->
    case compare (q, s') (39, 20) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 13);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 13);
      GT ->
    case compare (q, s') (40, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 4 14);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 4 14);
      GT ->
    case compare (q, s') (40, 13) of {
      LT ->
    case compare (q, s') (40, 11) of {
      LT ->
    case compare (q, s') (40, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 4 14);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 4 14);
      GT ->
    case compare (q, s') (40, 12) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 4 14);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 4 14);
      GT ->
    case compare (q, s') (40, 16) of {
      LT ->
    case compare (q, s') (40, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 4 14);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 4 14);
      GT ->
    case compare (q, s') (40, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 4 14);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Reduce 4 14);
      GT ->
    case compare (q, s') (48, 15) of {
      LT ->
    case compare (q, s') (41, 11) of {
      LT ->
    case compare (q, s') (40, 20) of {
      LT ->
    case compare (q, s') (40, 19) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 4 14);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 4 14);
      GT ->
    case compare (q, s') (40, 21) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 4 14);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 48);
      GT ->
    case compare (q, s') (45, 11) of {
      LT ->
    case compare (q, s') (43, 15) of {
      LT ->
    case compare (q, s') (42, 8) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 33);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 39);
      GT ->
    case compare (q, s') (44, 15) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 40);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 48);
      GT ->
    case compare (q, s') (47, 17) of {
      LT ->
    case compare (q, s') (47, 15) of {
      LT ->
    case compare (q, s') (46, 15) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 16);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 15);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 45);
      GT ->
    case compare (q, s') (48, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 52);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Reduce 1 19);
      GT ->
    case compare (q, s') (50, 11) of {
      LT ->
    case compare (q, s') (48, 18) of {
      LT ->
    case compare (q, s') (48, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 19);
      GT ->
        Nothing
    };
      EQ ->
        Just (Shift 50);
      GT ->
    case compare (q, s') (49, 17) of {
      LT ->
    case compare (q, s') (49, 15) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 18);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 3 18);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Shift 49);
      GT ->
    case compare (q, s') (51, 17) of {
      LT ->
    case compare (q, s') (51, 15) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 17);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 17);
      GT ->
    case compare (q, s') (52, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 52);
      GT ->
        Nothing
    }
    }
    }
    }
    }
    };
      EQ ->
        Just (Reduce 1 19);
      GT ->
    case compare (q, s') (56, 10) of {
      LT ->
    case compare (q, s') (54, 21) of {
      LT ->
    case compare (q, s') (54, 12) of {
      LT ->
    case compare (q, s') (54, 6) of {
      LT ->
    case compare (q, s') (53, 15) of {
      LT ->
    case compare (q, s') (52, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 19);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 20);
      GT ->
    case compare (q, s') (53, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 20);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 2 22);
      GT ->
    case compare (q, s') (54, 10) of {
      LT ->
    case compare (q, s') (54, 8) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 22);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 22);
      GT ->
    case compare (q, s') (54, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 22);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 2 22);
      GT ->
    case compare (q, s') (54, 17) of {
      LT ->
    case compare (q, s') (54, 14) of {
      LT ->
    case compare (q, s') (54, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 22);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 22);
      GT ->
    case compare (q, s') (54, 16) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 22);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 2 22);
      GT ->
    case compare (q, s') (54, 19) of {
      LT ->
    case compare (q, s') (54, 18) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 22);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 22);
      GT ->
    case compare (q, s') (54, 20) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 22);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Reduce 2 22);
      GT ->
    case compare (q, s') (55, 16) of {
      LT ->
    case compare (q, s') (55, 11) of {
      LT ->
    case compare (q, s') (55, 8) of {
      LT ->
    case compare (q, s') (55, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 23);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 23);
      GT ->
    case compare (q, s') (55, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 23);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 2 23);
      GT ->
    case compare (q, s') (55, 13) of {
      LT ->
    case compare (q, s') (55, 12) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 23);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 23);
      GT ->
    case compare (q, s') (55, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 23);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 2 23);
      GT ->
    case compare (q, s') (55, 20) of {
      LT ->
    case compare (q, s') (55, 18) of {
      LT ->
    case compare (q, s') (55, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 23);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 23);
      GT ->
    case compare (q, s') (55, 19) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 23);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 2 23);
      GT ->
    case compare (q, s') (56, 6) of {
      LT ->
    case compare (q, s') (55, 21) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 23);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 24);
      GT ->
    case compare (q, s') (56, 8) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 24);
      GT ->
        Nothing
    }
    }
    }
    }
    };
      EQ ->
        Just (Reduce 2 24);
      GT ->
    case compare (q, s') (57, 13) of {
      LT ->
    case compare (q, s') (56, 19) of {
      LT ->
    case compare (q, s') (56, 14) of {
      LT ->
    case compare (q, s') (56, 12) of {
      LT ->
    case compare (q, s') (56, 11) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 24);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 24);
      GT ->
    case compare (q, s') (56, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 24);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 2 24);
      GT ->
    case compare (q, s') (56, 17) of {
      LT ->
    case compare (q, s') (56, 16) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 24);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 24);
      GT ->
    case compare (q, s') (56, 18) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 24);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 2 24);
      GT ->
    case compare (q, s') (57, 8) of {
      LT ->
    case compare (q, s') (56, 21) of {
      LT ->
    case compare (q, s') (56, 20) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 24);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 2 24);
      GT ->
    case compare (q, s') (57, 6) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 21);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 21);
      GT ->
    case compare (q, s') (57, 11) of {
      LT ->
    case compare (q, s') (57, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 21);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 21);
      GT ->
    case compare (q, s') (57, 12) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 21);
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just (Reduce 1 21);
      GT ->
    case compare (q, s') (58, 8) of {
      LT ->
    case compare (q, s') (57, 18) of {
      LT ->
    case compare (q, s') (57, 16) of {
      LT ->
    case compare (q, s') (57, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 21);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 21);
      GT ->
    case compare (q, s') (57, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 21);
      GT ->
        Nothing
    }
    };
      EQ ->
        Just (Reduce 1 21);
      GT ->
    case compare (q, s') (57, 20) of {
      LT ->
    case compare (q, s') (57, 19) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 21);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 21);
      GT ->
    case compare (q, s') (57, 21) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 21);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 2 26);
      GT ->
    case compare (q, s') (61, -1) of {
      LT ->
    case compare (q, s') (59, 8) of {
      LT ->
    case compare (q, s') (58, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 2 26);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 27);
      GT ->
    case compare (q, s') (59, 17) of {
      LT ->
        Nothing;
      EQ ->
        Just (Shift 32);
      GT ->
    case compare (q, s') (60, 8) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 3 28);
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just (Reduce 1 34);
      GT ->
    case compare (q, s') (61, 4) of {
      LT ->
    case compare (q, s') (61, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 34);
      GT ->
        Nothing
    };
      EQ ->
        Just (Reduce 1 34);
      GT ->
    case compare (q, s') (61, 24) of {
      LT ->
        Nothing;
      EQ ->
        Just (Reduce 1 34);
      GT ->
        Nothing
    }
    }
    }
    }
    }
    }
    }

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
  let s' = production s in
    case compare (q, s') (23, 8) of {
      LT ->
    case compare (q, s') (11, 3) of {
      LT ->
    case compare (q, s') (3, 3) of {
      LT ->
    case compare (q, s') (0, 4) of {
      LT ->
    case compare (q, s') (0, 1) of {
      LT ->
    case compare (q, s') (0, 0) of {
      LT ->
        Nothing;
      EQ ->
        Just 1;
      GT ->
        Nothing
    };
      EQ ->
        Just 4;
      GT ->
        Nothing
    };
      EQ ->
        Just 7;
      GT ->
    case compare (q, s') (2, 5) of {
      LT ->
    case compare (q, s') (2, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just 5;
      GT ->
        Nothing
    };
      EQ ->
        Just 9;
      GT ->
        Nothing
    }
    };
      EQ ->
        Just 6;
      GT ->
    case compare (q, s') (7, 4) of {
      LT ->
    case compare (q, s') (7, 1) of {
      LT ->
    case compare (q, s') (3, 16) of {
      LT ->
        Nothing;
      EQ ->
        Just 11;
      GT ->
        Nothing
    };
      EQ ->
        Just 8;
      GT ->
        Nothing
    };
      EQ ->
        Just 7;
      GT ->
    case compare (q, s') (9, 5) of {
      LT ->
    case compare (q, s') (9, 2) of {
      LT ->
        Nothing;
      EQ ->
        Just 10;
      GT ->
        Nothing
    };
      EQ ->
        Just 9;
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just 14;
      GT ->
    case compare (q, s') (20, 6) of {
      LT ->
    case compare (q, s') (12, 16) of {
      LT ->
    case compare (q, s') (12, 3) of {
      LT ->
    case compare (q, s') (11, 16) of {
      LT ->
        Nothing;
      EQ ->
        Just 11;
      GT ->
        Nothing
    };
      EQ ->
        Just 17;
      GT ->
        Nothing
    };
      EQ ->
        Just 11;
      GT ->
    case compare (q, s') (13, 16) of {
      LT ->
    case compare (q, s') (13, 3) of {
      LT ->
        Nothing;
      EQ ->
        Just 18;
      GT ->
        Nothing
    };
      EQ ->
        Just 11;
      GT ->
        Nothing
    }
    };
      EQ ->
        Just 24;
      GT ->
    case compare (q, s') (21, 13) of {
      LT ->
    case compare (q, s') (21, 9) of {
      LT ->
    case compare (q, s') (21, 7) of {
      LT ->
        Nothing;
      EQ ->
        Just 25;
      GT ->
        Nothing
    };
      EQ ->
        Just 57;
      GT ->
        Nothing
    };
      EQ ->
        Just 31;
      GT ->
    case compare (q, s') (21, 15) of {
      LT ->
    case compare (q, s') (21, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just 59;
      GT ->
        Nothing
    };
      EQ ->
        Just 29;
      GT ->
        Nothing
    }
    }
    }
    };
      EQ ->
        Just 26;
      GT ->
    case compare (q, s') (32, 15) of {
      LT ->
    case compare (q, s') (31, 9) of {
      LT ->
    case compare (q, s') (28, 13) of {
      LT ->
    case compare (q, s') (28, 9) of {
      LT ->
    case compare (q, s') (28, 7) of {
      LT ->
        Nothing;
      EQ ->
        Just 42;
      GT ->
        Nothing
    };
      EQ ->
        Just 57;
      GT ->
        Nothing
    };
      EQ ->
        Just 31;
      GT ->
    case compare (q, s') (28, 15) of {
      LT ->
    case compare (q, s') (28, 14) of {
      LT ->
        Nothing;
      EQ ->
        Just 59;
      GT ->
        Nothing
    };
      EQ ->
        Just 29;
      GT ->
        Nothing
    }
    };
      EQ ->
        Just 57;
      GT ->
    case compare (q, s') (32, 9) of {
      LT ->
    case compare (q, s') (31, 14) of {
      LT ->
    case compare (q, s') (31, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just 31;
      GT ->
        Nothing
    };
      EQ ->
        Just 58;
      GT ->
        Nothing
    };
      EQ ->
        Just 57;
      GT ->
    case compare (q, s') (32, 14) of {
      LT ->
    case compare (q, s') (32, 13) of {
      LT ->
        Nothing;
      EQ ->
        Just 31;
      GT ->
        Nothing
    };
      EQ ->
        Just 59;
      GT ->
        Nothing
    }
    }
    };
      EQ ->
        Just 60;
      GT ->
    case compare (q, s') (41, 12) of {
      LT ->
    case compare (q, s') (38, 12) of {
      LT ->
    case compare (q, s') (38, 11) of {
      LT ->
    case compare (q, s') (38, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just 43;
      GT ->
        Nothing
    };
      EQ ->
        Just 47;
      GT ->
        Nothing
    };
      EQ ->
        Just 51;
      GT ->
    case compare (q, s') (41, 11) of {
      LT ->
    case compare (q, s') (41, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just 44;
      GT ->
        Nothing
    };
      EQ ->
        Just 47;
      GT ->
        Nothing
    }
    };
      EQ ->
        Just 51;
      GT ->
    case compare (q, s') (45, 12) of {
      LT ->
    case compare (q, s') (45, 11) of {
      LT ->
    case compare (q, s') (45, 10) of {
      LT ->
        Nothing;
      EQ ->
        Just 46;
      GT ->
        Nothing
    };
      EQ ->
        Just 47;
      GT ->
        Nothing
    };
      EQ ->
        Just 51;
      GT ->
    case compare (q, s') (52, 12) of {
      LT ->
    case compare (q, s') (48, 12) of {
      LT ->
        Nothing;
      EQ ->
        Just 53;
      GT ->
        Nothing
    };
      EQ ->
        Just 53;
      GT ->
        Nothing
    }
    }
    }
    }
    }

parse :: Monad m => SemanticActions m -> [Token] -> m (Either (Maybe Token) (Start, [Token]))
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
                case tokens of
                  [] -> return $ Left $ Nothing
                  (token : _) -> return $ Left $ Just token
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
          case stack of { [(_, StackValue_start value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



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


module  Language.HsLex.Parsing  where
import qualified Control.Monad as Monad


import qualified Language.HsLex as HsLex

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
    CASE CASE
  | CHAR CHAR
  | CODE CODE
  | COMMA COMMA
  | DARROW DARROW
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
  | StackValue_STRING STRING
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_HAT HAT
  | StackValue_PIPE PIPE
  | StackValue_CHAR CHAR
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
  , atexp_implies_STRING :: STRING -> m Atexp
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
    (3, EOF) -> Just (Reduce 0 29)
    (3, Token (CODE _)) -> Just (Shift 58)
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
    (11, EOF) -> Just (Reduce 0 29)
    (11, Token (PWHERE _)) -> Just (Reduce 0 29)
    (11, Token (PRBRACE _)) -> Just (Reduce 0 29)
    (11, Token (CODE _)) -> Just (Shift 58)
    (12, Token (PWHERE _)) -> Just (Reduce 0 29)
    (12, Token (CODE _)) -> Just (Shift 58)
    (13, Token (PRBRACE _)) -> Just (Reduce 0 29)
    (13, Token (CODE _)) -> Just (Shift 58)
    (14, EOF) -> Just (Reduce 2 30)
    (14, Token (PWHERE _)) -> Just (Reduce 2 30)
    (14, Token (PRBRACE _)) -> Just (Reduce 2 30)
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
    (21, Token (STRING _)) -> Just (Shift 34)
    (21, Token (LBRACKET _)) -> Just (Shift 35)
    (22, Token (DARROW _)) -> Just (Shift 23)
    (23, Token (SEMANTIC_ACTION _)) -> Just (Shift 30)
    (24, Token (COMMA _)) -> Just (Shift 21)
    (25, Token (RPAREN _)) -> Just (Shift 22)
    (26, Token (PP _)) -> Just (Reduce 8 7)
    (26, Token (CASE _)) -> Just (Reduce 8 7)
    (26, Token (LPAREN _)) -> Just (Reduce 8 7)
    (27, Token (COMMA _)) -> Just (Reduce 1 27)
    (28, Token (LPAREN _)) -> Just (Shift 28)
    (28, Token (STRING _)) -> Just (Shift 34)
    (28, Token (LBRACKET _)) -> Just (Shift 35)
    (29, Token (RPAREN _)) -> Just (Reduce 1 26)
    (30, Token (PP _)) -> Just (Reduce 1 28)
    (30, Token (CASE _)) -> Just (Reduce 1 28)
    (30, Token (LPAREN _)) -> Just (Reduce 1 28)
    (31, Token (LPAREN _)) -> Just (Shift 28)
    (31, Token (RPAREN _)) -> Just (Reduce 1 22)
    (31, Token (STRING _)) -> Just (Shift 34)
    (31, Token (LBRACKET _)) -> Just (Shift 35)
    (31, Token (PIPE _)) -> Just (Reduce 1 22)
    (31, Token (STAR _)) -> Just (Shift 51)
    (31, Token (PLUS _)) -> Just (Shift 52)
    (31, Token (QUES _)) -> Just (Shift 53)
    (32, Token (LPAREN _)) -> Just (Shift 28)
    (32, Token (STRING _)) -> Just (Shift 34)
    (32, Token (LBRACKET _)) -> Just (Shift 35)
    (33, Token (LPAREN _)) -> Just (Reduce 3 9)
    (33, Token (RPAREN _)) -> Just (Reduce 3 9)
    (33, Token (STRING _)) -> Just (Reduce 3 9)
    (33, Token (LBRACKET _)) -> Just (Reduce 3 9)
    (33, Token (HAT _)) -> Just (Reduce 3 9)
    (33, Token (PIPE _)) -> Just (Reduce 3 9)
    (33, Token (CHAR _)) -> Just (Reduce 3 9)
    (33, Token (HYPHEN _)) -> Just (Reduce 3 9)
    (33, Token (STAR _)) -> Just (Reduce 3 9)
    (33, Token (PLUS _)) -> Just (Reduce 3 9)
    (33, Token (QUES _)) -> Just (Reduce 3 9)
    (34, Token (LPAREN _)) -> Just (Reduce 1 8)
    (34, Token (RPAREN _)) -> Just (Reduce 1 8)
    (34, Token (STRING _)) -> Just (Reduce 1 8)
    (34, Token (LBRACKET _)) -> Just (Reduce 1 8)
    (34, Token (HAT _)) -> Just (Reduce 1 8)
    (34, Token (PIPE _)) -> Just (Reduce 1 8)
    (34, Token (CHAR _)) -> Just (Reduce 1 8)
    (34, Token (HYPHEN _)) -> Just (Reduce 1 8)
    (34, Token (STAR _)) -> Just (Reduce 1 8)
    (34, Token (PLUS _)) -> Just (Reduce 1 8)
    (34, Token (QUES _)) -> Just (Reduce 1 8)
    (35, Token (HAT _)) -> Just (Shift 38)
    (35, Token (CHAR _)) -> Just (Shift 45)
    (36, Token (LPAREN _)) -> Just (Reduce 3 10)
    (36, Token (RPAREN _)) -> Just (Reduce 3 10)
    (36, Token (STRING _)) -> Just (Reduce 3 10)
    (36, Token (LBRACKET _)) -> Just (Reduce 3 10)
    (36, Token (HAT _)) -> Just (Reduce 3 10)
    (36, Token (PIPE _)) -> Just (Reduce 3 10)
    (36, Token (CHAR _)) -> Just (Reduce 3 10)
    (36, Token (HYPHEN _)) -> Just (Reduce 3 10)
    (36, Token (STAR _)) -> Just (Reduce 3 10)
    (36, Token (PLUS _)) -> Just (Reduce 3 10)
    (36, Token (QUES _)) -> Just (Reduce 3 10)
    (37, Token (LPAREN _)) -> Just (Reduce 4 11)
    (37, Token (RPAREN _)) -> Just (Reduce 4 11)
    (37, Token (STRING _)) -> Just (Reduce 4 11)
    (37, Token (LBRACKET _)) -> Just (Reduce 4 11)
    (37, Token (HAT _)) -> Just (Reduce 4 11)
    (37, Token (PIPE _)) -> Just (Reduce 4 11)
    (37, Token (CHAR _)) -> Just (Reduce 4 11)
    (37, Token (HYPHEN _)) -> Just (Reduce 4 11)
    (37, Token (STAR _)) -> Just (Reduce 4 11)
    (37, Token (PLUS _)) -> Just (Reduce 4 11)
    (37, Token (QUES _)) -> Just (Reduce 4 11)
    (38, Token (CHAR _)) -> Just (Shift 45)
    (39, Token (RPAREN _)) -> Just (Shift 33)
    (40, Token (RBRACKET _)) -> Just (Shift 36)
    (41, Token (RBRACKET _)) -> Just (Shift 37)
    (42, Token (CHAR _)) -> Just (Shift 45)
    (43, Token (RBRACKET _)) -> Just (Reduce 3 13)
    (44, Token (RBRACKET _)) -> Just (Reduce 1 12)
    (44, Token (PIPE _)) -> Just (Shift 42)
    (45, Token (RBRACKET _)) -> Just (Reduce 1 16)
    (45, Token (PIPE _)) -> Just (Reduce 1 16)
    (45, Token (CHAR _)) -> Just (Shift 49)
    (45, Token (HYPHEN _)) -> Just (Shift 47)
    (46, Token (RBRACKET _)) -> Just (Reduce 3 15)
    (46, Token (PIPE _)) -> Just (Reduce 3 15)
    (47, Token (CHAR _)) -> Just (Shift 46)
    (48, Token (RBRACKET _)) -> Just (Reduce 1 14)
    (48, Token (PIPE _)) -> Just (Reduce 1 14)
    (49, Token (RBRACKET _)) -> Just (Reduce 1 16)
    (49, Token (PIPE _)) -> Just (Reduce 1 16)
    (49, Token (CHAR _)) -> Just (Shift 49)
    (50, Token (RBRACKET _)) -> Just (Reduce 2 17)
    (50, Token (PIPE _)) -> Just (Reduce 2 17)
    (51, Token (LPAREN _)) -> Just (Reduce 2 19)
    (51, Token (RPAREN _)) -> Just (Reduce 2 19)
    (51, Token (STRING _)) -> Just (Reduce 2 19)
    (51, Token (LBRACKET _)) -> Just (Reduce 2 19)
    (51, Token (HAT _)) -> Just (Reduce 2 19)
    (51, Token (PIPE _)) -> Just (Reduce 2 19)
    (51, Token (CHAR _)) -> Just (Reduce 2 19)
    (51, Token (HYPHEN _)) -> Just (Reduce 2 19)
    (51, Token (STAR _)) -> Just (Reduce 2 19)
    (51, Token (PLUS _)) -> Just (Reduce 2 19)
    (51, Token (QUES _)) -> Just (Reduce 2 19)
    (52, Token (LPAREN _)) -> Just (Reduce 2 20)
    (52, Token (RPAREN _)) -> Just (Reduce 2 20)
    (52, Token (STRING _)) -> Just (Reduce 2 20)
    (52, Token (LBRACKET _)) -> Just (Reduce 2 20)
    (52, Token (HAT _)) -> Just (Reduce 2 20)
    (52, Token (PIPE _)) -> Just (Reduce 2 20)
    (52, Token (CHAR _)) -> Just (Reduce 2 20)
    (52, Token (HYPHEN _)) -> Just (Reduce 2 20)
    (52, Token (STAR _)) -> Just (Reduce 2 20)
    (52, Token (PLUS _)) -> Just (Reduce 2 20)
    (52, Token (QUES _)) -> Just (Reduce 2 20)
    (53, Token (LPAREN _)) -> Just (Reduce 2 21)
    (53, Token (RPAREN _)) -> Just (Reduce 2 21)
    (53, Token (STRING _)) -> Just (Reduce 2 21)
    (53, Token (LBRACKET _)) -> Just (Reduce 2 21)
    (53, Token (HAT _)) -> Just (Reduce 2 21)
    (53, Token (PIPE _)) -> Just (Reduce 2 21)
    (53, Token (CHAR _)) -> Just (Reduce 2 21)
    (53, Token (HYPHEN _)) -> Just (Reduce 2 21)
    (53, Token (STAR _)) -> Just (Reduce 2 21)
    (53, Token (PLUS _)) -> Just (Reduce 2 21)
    (53, Token (QUES _)) -> Just (Reduce 2 21)
    (54, Token (LPAREN _)) -> Just (Reduce 1 18)
    (54, Token (RPAREN _)) -> Just (Reduce 1 18)
    (54, Token (STRING _)) -> Just (Reduce 1 18)
    (54, Token (LBRACKET _)) -> Just (Reduce 1 18)
    (54, Token (HAT _)) -> Just (Reduce 1 18)
    (54, Token (PIPE _)) -> Just (Reduce 1 18)
    (54, Token (CHAR _)) -> Just (Reduce 1 18)
    (54, Token (HYPHEN _)) -> Just (Reduce 1 18)
    (54, Token (STAR _)) -> Just (Reduce 1 18)
    (54, Token (PLUS _)) -> Just (Reduce 1 18)
    (54, Token (QUES _)) -> Just (Reduce 1 18)
    (55, Token (RPAREN _)) -> Just (Reduce 2 23)
    (55, Token (PIPE _)) -> Just (Reduce 2 23)
    (56, Token (RPAREN _)) -> Just (Reduce 1 24)
    (56, Token (PIPE _)) -> Just (Shift 32)
    (57, Token (RPAREN _)) -> Just (Reduce 3 25)
    (58, EOF) -> Just (Reduce 1 31)
    (58, Token (PWHERE _)) -> Just (Reduce 1 31)
    (58, Token (PRBRACE _)) -> Just (Reduce 1 31)
    (58, Token (CODE _)) -> Just (Reduce 1 31)
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
production 12 = 10
production 13 = 10
production 14 = 11
production 15 = 11
production 16 = 12
production 17 = 12
production 18 = 13
production 19 = 13
production 20 = 13
production 21 = 13
production 22 = 14
production 23 = 14
production 24 = 15
production 25 = 15
production 26 = 7
production 27 = 6
production 28 = 8
production 29 = 3
production 30 = 3
production 31 = 16

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
    (21, 9) -> Just 54
    (21, 13) -> Just 31
    (21, 14) -> Just 56
    (21, 15) -> Just 29
    (23, 8) -> Just 26
    (28, 7) -> Just 39
    (28, 9) -> Just 54
    (28, 13) -> Just 31
    (28, 14) -> Just 56
    (28, 15) -> Just 29
    (31, 9) -> Just 54
    (31, 13) -> Just 31
    (31, 14) -> Just 55
    (32, 9) -> Just 54
    (32, 13) -> Just 31
    (32, 14) -> Just 56
    (32, 15) -> Just 57
    (35, 10) -> Just 40
    (35, 11) -> Just 44
    (35, 12) -> Just 48
    (38, 10) -> Just 41
    (38, 11) -> Just 44
    (38, 12) -> Just 48
    (42, 10) -> Just 43
    (42, 11) -> Just 44
    (42, 12) -> Just 48
    (45, 12) -> Just 50
    (49, 12) -> Just 50
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
                  Token (STRING semanticValue) ->
                    StackValue_STRING semanticValue
                  Token (LBRACKET semanticValue) ->
                    StackValue_LBRACKET semanticValue
                  Token (RBRACKET semanticValue) ->
                    StackValue_RBRACKET semanticValue
                  Token (HAT semanticValue) ->
                    StackValue_HAT semanticValue
                  Token (PIPE semanticValue) ->
                    StackValue_PIPE semanticValue
                  Token (CHAR semanticValue) ->
                    StackValue_CHAR semanticValue
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
                      Monad.liftM StackValue_atexp $ atexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_LBRACKET_charsets_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_charsets value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_atexp $ atexp_implies_LBRACKET_HAT_charsets_RBRACKET actions (case snd (pop !! 3) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_HAT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_charsets value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_charsets $ charsets_implies_charset actions (case snd (pop !! 0) of { StackValue_charset value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_charsets $ charsets_implies_charset_PIPE_charsets actions (case snd (pop !! 2) of { StackValue_charset value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_charsets value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_charset $ charset_implies_chars actions (case snd (pop !! 0) of { StackValue_chars value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_charset $ charset_implies_CHAR_HYPHEN_CHAR actions (case snd (pop !! 2) of { StackValue_CHAR value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_HYPHEN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_CHAR value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_chars $ chars_implies_CHAR actions (case snd (pop !! 0) of { StackValue_CHAR value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_chars $ chars_implies_CHAR_chars actions (case snd (pop !! 1) of { StackValue_CHAR value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_chars value -> value; _ -> undefined })
                    18 ->
                      Monad.liftM StackValue_suffixexp $ suffixexp_implies_atexp actions (case snd (pop !! 0) of { StackValue_atexp value -> value; _ -> undefined })
                    19 ->
                      Monad.liftM StackValue_suffixexp $ suffixexp_implies_suffixexp_STAR actions (case snd (pop !! 1) of { StackValue_suffixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_STAR value -> value; _ -> undefined })
                    20 ->
                      Monad.liftM StackValue_suffixexp $ suffixexp_implies_suffixexp_PLUS actions (case snd (pop !! 1) of { StackValue_suffixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_PLUS value -> value; _ -> undefined })
                    21 ->
                      Monad.liftM StackValue_suffixexp $ suffixexp_implies_suffixexp_QUES actions (case snd (pop !! 1) of { StackValue_suffixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_QUES value -> value; _ -> undefined })
                    22 ->
                      Monad.liftM StackValue_catexp $ catexp_implies_suffixexp actions (case snd (pop !! 0) of { StackValue_suffixexp value -> value; _ -> undefined })
                    23 ->
                      Monad.liftM StackValue_catexp $ catexp_implies_suffixexp_catexp actions (case snd (pop !! 1) of { StackValue_suffixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_catexp value -> value; _ -> undefined })
                    24 ->
                      Monad.liftM StackValue_orexp $ orexp_implies_catexp actions (case snd (pop !! 0) of { StackValue_catexp value -> value; _ -> undefined })
                    25 ->
                      Monad.liftM StackValue_orexp $ orexp_implies_catexp_PIPE_orexp actions (case snd (pop !! 2) of { StackValue_catexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_orexp value -> value; _ -> undefined })
                    26 ->
                      Monad.liftM StackValue_exp $ exp_implies_orexp actions (case snd (pop !! 0) of { StackValue_orexp value -> value; _ -> undefined })
                    27 ->
                      Monad.liftM StackValue_lexingState $ lexingState_implies_LEXING_STATE actions (case snd (pop !! 0) of { StackValue_LEXING_STATE value -> value; _ -> undefined })
                    28 ->
                      Monad.liftM StackValue_semanticAction $ semanticAction_implies_SEMANTIC_ACTION actions (case snd (pop !! 0) of { StackValue_SEMANTIC_ACTION value -> value; _ -> undefined })
                    29 ->
                      Monad.liftM StackValue_codes $ codes_implies actions
                    30 ->
                      Monad.liftM StackValue_codes $ codes_implies_code_codes actions (case snd (pop !! 1) of { StackValue_code value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_codes value -> value; _ -> undefined })
                    31 ->
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
  , atexp_implies_STRING =
      return . HsLex.string
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


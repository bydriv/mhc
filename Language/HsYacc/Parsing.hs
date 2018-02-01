module  Language.HsYacc.Parsing  where
import qualified Control.Monad as Monad


type CODE = Char
type COLONEQ = ()
type DEF = ()
type NONTERMINAL = String
type PIPE = ()
type PLBRACE = ()
type PMODULE = ()
type PP = ()
type PRBRACE = ()
type PSTART = ()
type PTRIVIAL = ()
type PWHERE = ()
type RULE = ()
type TERMINAL = String

type Start = (Definitions, Rules, Codes)

type Definitions = [Definition]

data Definition =
    DefnStart Nonterminal
  | DefnTrivial
  | DefnModule Codes
  | DefnCodes Codes
  deriving (Eq, Ord, Read, Show)

type Rules = [Rule']
type Rule' = (NONTERMINAL, RuleBodies)
type RuleBodies = [RuleBody]
type RuleBody = [Symbol]
type Codes = [Code]
type Code = CODE
data Symbol = Terminal Terminal | Nonterminal Nonterminal
  deriving (Eq, Ord, Read, Show)
type Terminal = TERMINAL
type Nonterminal = NONTERMINAL

data Token =
    CODE CODE
  | COLONEQ COLONEQ
  | DEF DEF
  | NONTERMINAL NONTERMINAL
  | PIPE PIPE
  | PLBRACE PLBRACE
  | PMODULE PMODULE
  | PP PP
  | PRBRACE PRBRACE
  | PSTART PSTART
  | PTRIVIAL PTRIVIAL
  | PWHERE PWHERE
  | RULE RULE
  | TERMINAL TERMINAL
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
type GotoState = Int
type GotoSymbol = Int

data StackValue =
    StackValue_EOF
  | StackValue_PP PP
  | StackValue_PSTART PSTART
  | StackValue_PTRIVIAL PTRIVIAL
  | StackValue_PMODULE PMODULE
  | StackValue_PWHERE PWHERE
  | StackValue_PLBRACE PLBRACE
  | StackValue_PRBRACE PRBRACE
  | StackValue_DEF DEF
  | StackValue_RULE RULE
  | StackValue_COLONEQ COLONEQ
  | StackValue_PIPE PIPE
  | StackValue_TERMINAL TERMINAL
  | StackValue_NONTERMINAL NONTERMINAL
  | StackValue_CODE CODE
  | StackValue_start Start
  | StackValue_definitions Definitions
  | StackValue_rules Rules
  | StackValue_codes Codes
  | StackValue_definition Definition
  | StackValue_nonterminal Nonterminal
  | StackValue_rule' Rule'
  | StackValue_ruleBodies RuleBodies
  | StackValue_ruleBody RuleBody
  | StackValue_symbol Symbol
  | StackValue_terminal Terminal
  | StackValue_code Code

data SemanticActions m = SemanticActions
  { start_implies_definitions_PP_rules_PP_codes :: Definitions -> PP -> Rules -> PP -> Codes -> m Start
  , definitions_implies :: m Definitions
  , definitions_implies_definition_definitions :: Definition -> Definitions -> m Definitions
  , definition_implies_PSTART_nonterminal :: PSTART -> Nonterminal -> m Definition
  , definition_implies_PTRIVIAL :: PTRIVIAL -> m Definition
  , definition_implies_PMODULE_codes_PWHERE :: PMODULE -> Codes -> PWHERE -> m Definition
  , definition_implies_PLBRACE_codes_PRBRACE :: PLBRACE -> Codes -> PRBRACE -> m Definition
  , rules_implies :: m Rules
  , rules_implies_rule'_rules :: Rule' -> Rules -> m Rules
  , rule'_implies_DEF_RULE_nonterminal_COLONEQ_ruleBodies :: DEF -> RULE -> Nonterminal -> COLONEQ -> RuleBodies -> m Rule'
  , ruleBodies_implies_ruleBody :: RuleBody -> m RuleBodies
  , ruleBodies_implies_ruleBody_PIPE_ruleBodies :: RuleBody -> PIPE -> RuleBodies -> m RuleBodies
  , ruleBody_implies :: m RuleBody
  , ruleBody_implies_symbol_ruleBody :: Symbol -> RuleBody -> m RuleBody
  , terminal_implies_TERMINAL :: TERMINAL -> m Terminal
  , nonterminal_implies_NONTERMINAL :: NONTERMINAL -> m Nonterminal
  , symbol_implies_terminal :: Terminal -> m Symbol
  , symbol_implies_nonterminal :: Nonterminal -> m Symbol
  , codes_implies :: m Codes
  , codes_implies_code_codes :: Code -> Codes -> m Codes
  , code_implies_CODE :: CODE -> m Code }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token (PP _)) -> Just (Reduce 0 1)
    (0, Token (PSTART _)) -> Just (Shift 15)
    (0, Token (PTRIVIAL _)) -> Just (Shift 16)
    (0, Token (PMODULE _)) -> Just (Shift 12)
    (0, Token (PLBRACE _)) -> Just (Shift 13)
    (1, EOF) -> Just (Accept)
    (2, Token (PP _)) -> Just (Reduce 0 7)
    (2, Token (DEF _)) -> Just (Shift 27)
    (3, EOF) -> Just (Reduce 0 18)
    (3, Token (CODE _)) -> Just (Shift 36)
    (4, Token (PP _)) -> Just (Shift 2)
    (5, Token (PP _)) -> Just (Shift 3)
    (6, EOF) -> Just (Reduce 5 0)
    (7, Token (PP _)) -> Just (Reduce 0 1)
    (7, Token (PSTART _)) -> Just (Shift 15)
    (7, Token (PTRIVIAL _)) -> Just (Shift 16)
    (7, Token (PMODULE _)) -> Just (Shift 12)
    (7, Token (PLBRACE _)) -> Just (Shift 13)
    (8, Token (PP _)) -> Just (Reduce 2 2)
    (9, Token (PP _)) -> Just (Reduce 0 7)
    (9, Token (DEF _)) -> Just (Shift 27)
    (10, Token (PP _)) -> Just (Reduce 2 8)
    (11, EOF) -> Just (Reduce 0 18)
    (11, Token (PWHERE _)) -> Just (Reduce 0 18)
    (11, Token (PRBRACE _)) -> Just (Reduce 0 18)
    (11, Token (CODE _)) -> Just (Shift 36)
    (12, Token (PWHERE _)) -> Just (Reduce 0 18)
    (12, Token (CODE _)) -> Just (Shift 36)
    (13, Token (PRBRACE _)) -> Just (Reduce 0 18)
    (13, Token (CODE _)) -> Just (Shift 36)
    (14, EOF) -> Just (Reduce 2 19)
    (14, Token (PWHERE _)) -> Just (Reduce 2 19)
    (14, Token (PRBRACE _)) -> Just (Reduce 2 19)
    (15, Token (NONTERMINAL _)) -> Just (Shift 26)
    (16, Token (PP _)) -> Just (Reduce 1 4)
    (16, Token (PSTART _)) -> Just (Reduce 1 4)
    (16, Token (PTRIVIAL _)) -> Just (Reduce 1 4)
    (16, Token (PMODULE _)) -> Just (Reduce 1 4)
    (16, Token (PLBRACE _)) -> Just (Reduce 1 4)
    (16, Token (NONTERMINAL _)) -> Just (Reduce 1 4)
    (16, Token (CODE _)) -> Just (Reduce 1 4)
    (17, Token (PP _)) -> Just (Reduce 3 5)
    (17, Token (PSTART _)) -> Just (Reduce 3 5)
    (17, Token (PTRIVIAL _)) -> Just (Reduce 3 5)
    (17, Token (PMODULE _)) -> Just (Reduce 3 5)
    (17, Token (PLBRACE _)) -> Just (Reduce 3 5)
    (17, Token (NONTERMINAL _)) -> Just (Reduce 3 5)
    (17, Token (CODE _)) -> Just (Reduce 3 5)
    (18, Token (PP _)) -> Just (Reduce 3 6)
    (18, Token (PSTART _)) -> Just (Reduce 3 6)
    (18, Token (PTRIVIAL _)) -> Just (Reduce 3 6)
    (18, Token (PMODULE _)) -> Just (Reduce 3 6)
    (18, Token (PLBRACE _)) -> Just (Reduce 3 6)
    (18, Token (NONTERMINAL _)) -> Just (Reduce 3 6)
    (18, Token (CODE _)) -> Just (Reduce 3 6)
    (19, Token (PWHERE _)) -> Just (Shift 17)
    (20, Token (PRBRACE _)) -> Just (Shift 18)
    (21, Token (PP _)) -> Just (Reduce 2 3)
    (21, Token (PSTART _)) -> Just (Reduce 2 3)
    (21, Token (PTRIVIAL _)) -> Just (Reduce 2 3)
    (21, Token (PMODULE _)) -> Just (Reduce 2 3)
    (21, Token (PLBRACE _)) -> Just (Reduce 2 3)
    (21, Token (NONTERMINAL _)) -> Just (Reduce 2 3)
    (21, Token (CODE _)) -> Just (Reduce 2 3)
    (22, Token (PP _)) -> Just (Reduce 0 12)
    (22, Token (DEF _)) -> Just (Reduce 0 12)
    (22, Token (RULE _)) -> Just (Reduce 0 12)
    (22, Token (PIPE _)) -> Just (Reduce 0 12)
    (22, Token (TERMINAL _)) -> Just (Shift 35)
    (22, Token (NONTERMINAL _)) -> Just (Shift 26)
    (23, Token (PP _)) -> Just (Reduce 0 12)
    (23, Token (DEF _)) -> Just (Reduce 0 12)
    (23, Token (RULE _)) -> Just (Reduce 0 12)
    (23, Token (PIPE _)) -> Just (Reduce 0 12)
    (23, Token (TERMINAL _)) -> Just (Shift 35)
    (23, Token (NONTERMINAL _)) -> Just (Shift 26)
    (24, Token (PP _)) -> Just (Reduce 0 12)
    (24, Token (DEF _)) -> Just (Reduce 0 12)
    (24, Token (RULE _)) -> Just (Reduce 0 12)
    (24, Token (PIPE _)) -> Just (Reduce 0 12)
    (24, Token (TERMINAL _)) -> Just (Shift 35)
    (24, Token (NONTERMINAL _)) -> Just (Shift 26)
    (25, Token (NONTERMINAL _)) -> Just (Shift 26)
    (26, Token (PP _)) -> Just (Reduce 1 15)
    (26, Token (PSTART _)) -> Just (Reduce 1 15)
    (26, Token (PTRIVIAL _)) -> Just (Reduce 1 15)
    (26, Token (PMODULE _)) -> Just (Reduce 1 15)
    (26, Token (PLBRACE _)) -> Just (Reduce 1 15)
    (26, Token (DEF _)) -> Just (Reduce 1 15)
    (26, Token (RULE _)) -> Just (Reduce 1 15)
    (26, Token (COLONEQ _)) -> Just (Reduce 1 15)
    (26, Token (PIPE _)) -> Just (Reduce 1 15)
    (26, Token (TERMINAL _)) -> Just (Reduce 1 15)
    (26, Token (NONTERMINAL _)) -> Just (Reduce 1 15)
    (26, Token (CODE _)) -> Just (Reduce 1 15)
    (27, Token (RULE _)) -> Just (Shift 25)
    (28, Token (COLONEQ _)) -> Just (Shift 22)
    (29, Token (PP _)) -> Just (Reduce 5 9)
    (29, Token (DEF _)) -> Just (Reduce 5 9)
    (29, Token (RULE _)) -> Just (Reduce 5 9)
    (30, Token (PP _)) -> Just (Reduce 3 11)
    (30, Token (DEF _)) -> Just (Reduce 3 11)
    (30, Token (RULE _)) -> Just (Reduce 3 11)
    (31, Token (PP _)) -> Just (Reduce 1 10)
    (31, Token (DEF _)) -> Just (Reduce 1 10)
    (31, Token (RULE _)) -> Just (Reduce 1 10)
    (31, Token (PIPE _)) -> Just (Shift 23)
    (32, Token (PP _)) -> Just (Reduce 2 13)
    (32, Token (DEF _)) -> Just (Reduce 2 13)
    (32, Token (RULE _)) -> Just (Reduce 2 13)
    (32, Token (PIPE _)) -> Just (Reduce 2 13)
    (33, Token (PP _)) -> Just (Reduce 1 17)
    (33, Token (DEF _)) -> Just (Reduce 1 17)
    (33, Token (RULE _)) -> Just (Reduce 1 17)
    (33, Token (PIPE _)) -> Just (Reduce 1 17)
    (33, Token (TERMINAL _)) -> Just (Reduce 1 17)
    (33, Token (NONTERMINAL _)) -> Just (Reduce 1 17)
    (34, Token (PP _)) -> Just (Reduce 1 16)
    (34, Token (DEF _)) -> Just (Reduce 1 16)
    (34, Token (RULE _)) -> Just (Reduce 1 16)
    (34, Token (PIPE _)) -> Just (Reduce 1 16)
    (34, Token (TERMINAL _)) -> Just (Reduce 1 16)
    (34, Token (NONTERMINAL _)) -> Just (Reduce 1 16)
    (35, Token (PP _)) -> Just (Reduce 1 14)
    (35, Token (DEF _)) -> Just (Reduce 1 14)
    (35, Token (RULE _)) -> Just (Reduce 1 14)
    (35, Token (PIPE _)) -> Just (Reduce 1 14)
    (35, Token (TERMINAL _)) -> Just (Reduce 1 14)
    (35, Token (NONTERMINAL _)) -> Just (Reduce 1 14)
    (36, EOF) -> Just (Reduce 1 20)
    (36, Token (PWHERE _)) -> Just (Reduce 1 20)
    (36, Token (PRBRACE _)) -> Just (Reduce 1 20)
    (36, Token (CODE _)) -> Just (Reduce 1 20)
    (_, _) -> Nothing

production :: Int -> Int
production 0 = 0
production 1 = 1
production 2 = 1
production 3 = 4
production 4 = 4
production 5 = 4
production 6 = 4
production 7 = 2
production 8 = 2
production 9 = 6
production 10 = 7
production 11 = 7
production 12 = 8
production 13 = 8
production 14 = 10
production 15 = 5
production 16 = 9
production 17 = 9
production 18 = 3
production 19 = 3
production 20 = 11

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 1) -> Just 4
    (0, 4) -> Just 7
    (2, 2) -> Just 5
    (2, 6) -> Just 9
    (3, 3) -> Just 6
    (3, 11) -> Just 11
    (7, 1) -> Just 8
    (7, 4) -> Just 7
    (9, 2) -> Just 10
    (9, 6) -> Just 9
    (11, 3) -> Just 14
    (11, 11) -> Just 11
    (12, 3) -> Just 19
    (12, 11) -> Just 11
    (13, 3) -> Just 20
    (13, 11) -> Just 11
    (15, 5) -> Just 21
    (22, 5) -> Just 33
    (22, 7) -> Just 29
    (22, 8) -> Just 31
    (22, 9) -> Just 24
    (22, 10) -> Just 34
    (23, 5) -> Just 33
    (23, 7) -> Just 30
    (23, 8) -> Just 31
    (23, 9) -> Just 24
    (23, 10) -> Just 34
    (24, 5) -> Just 33
    (24, 8) -> Just 32
    (24, 9) -> Just 24
    (24, 10) -> Just 34
    (25, 5) -> Just 28
    (_, _) -> Nothing

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
                  Token (PSTART semanticValue) ->
                    StackValue_PSTART semanticValue
                  Token (PTRIVIAL semanticValue) ->
                    StackValue_PTRIVIAL semanticValue
                  Token (PMODULE semanticValue) ->
                    StackValue_PMODULE semanticValue
                  Token (PWHERE semanticValue) ->
                    StackValue_PWHERE semanticValue
                  Token (PLBRACE semanticValue) ->
                    StackValue_PLBRACE semanticValue
                  Token (PRBRACE semanticValue) ->
                    StackValue_PRBRACE semanticValue
                  Token (DEF semanticValue) ->
                    StackValue_DEF semanticValue
                  Token (RULE semanticValue) ->
                    StackValue_RULE semanticValue
                  Token (COLONEQ semanticValue) ->
                    StackValue_COLONEQ semanticValue
                  Token (PIPE semanticValue) ->
                    StackValue_PIPE semanticValue
                  Token (TERMINAL semanticValue) ->
                    StackValue_TERMINAL semanticValue
                  Token (NONTERMINAL semanticValue) ->
                    StackValue_NONTERMINAL semanticValue
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
                      Monad.liftM StackValue_start $ start_implies_definitions_PP_rules_PP_codes actions (case snd (pop !! 4) of { StackValue_definitions value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_PP value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_rules value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PP value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_codes value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_definitions $ definitions_implies actions
                    2 ->
                      Monad.liftM StackValue_definitions $ definitions_implies_definition_definitions actions (case snd (pop !! 1) of { StackValue_definition value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_definitions value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_definition $ definition_implies_PSTART_nonterminal actions (case snd (pop !! 1) of { StackValue_PSTART value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_nonterminal value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_definition $ definition_implies_PTRIVIAL actions (case snd (pop !! 0) of { StackValue_PTRIVIAL value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_definition $ definition_implies_PMODULE_codes_PWHERE actions (case snd (pop !! 2) of { StackValue_PMODULE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_codes value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_PWHERE value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_definition $ definition_implies_PLBRACE_codes_PRBRACE actions (case snd (pop !! 2) of { StackValue_PLBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_codes value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_PRBRACE value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_rules $ rules_implies actions
                    8 ->
                      Monad.liftM StackValue_rules $ rules_implies_rule'_rules actions (case snd (pop !! 1) of { StackValue_rule' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rules value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_rule' $ rule'_implies_DEF_RULE_nonterminal_COLONEQ_ruleBodies actions (case snd (pop !! 4) of { StackValue_DEF value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RULE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_nonterminal value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLONEQ value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBodies value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_ruleBodies $ ruleBodies_implies_ruleBody actions (case snd (pop !! 0) of { StackValue_ruleBody value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_ruleBodies $ ruleBodies_implies_ruleBody_PIPE_ruleBodies actions (case snd (pop !! 2) of { StackValue_ruleBody value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBodies value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_ruleBody $ ruleBody_implies actions
                    13 ->
                      Monad.liftM StackValue_ruleBody $ ruleBody_implies_symbol_ruleBody actions (case snd (pop !! 1) of { StackValue_symbol value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBody value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_terminal $ terminal_implies_TERMINAL actions (case snd (pop !! 0) of { StackValue_TERMINAL value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_nonterminal $ nonterminal_implies_NONTERMINAL actions (case snd (pop !! 0) of { StackValue_NONTERMINAL value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_symbol $ symbol_implies_terminal actions (case snd (pop !! 0) of { StackValue_terminal value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_symbol $ symbol_implies_nonterminal actions (case snd (pop !! 0) of { StackValue_nonterminal value -> value; _ -> undefined })
                    18 ->
                      Monad.liftM StackValue_codes $ codes_implies actions
                    19 ->
                      Monad.liftM StackValue_codes $ codes_implies_code_codes actions (case snd (pop !! 1) of { StackValue_code value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_codes value -> value; _ -> undefined })
                    20 ->
                      Monad.liftM StackValue_code $ code_implies_CODE actions (case snd (pop !! 0) of { StackValue_CODE value -> value; _ -> undefined })
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_start value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}



semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { start_implies_definitions_PP_rules_PP_codes = \defns () rules () codes ->
      return (defns, rules, codes)
  , definitions_implies =
      return []
  , definitions_implies_definition_definitions = \defn defns ->
      return $ defn : defns
  , definition_implies_PSTART_nonterminal = \() start ->
      return $ DefnStart start
  , definition_implies_PTRIVIAL = const $
      return $ DefnTrivial
  , definition_implies_PMODULE_codes_PWHERE = \() codes () ->
      return $ DefnModule codes
  , definition_implies_PLBRACE_codes_PRBRACE = \() codes () ->
      return $ DefnCodes codes
  , rules_implies =
      return []
  , rules_implies_rule'_rules = \rule rules ->
      return $ rule : rules
  , rule'_implies_DEF_RULE_nonterminal_COLONEQ_ruleBodies = \() () hd () body ->
      return (hd, body)
  , ruleBodies_implies_ruleBody = \body ->
      return [body]
  , ruleBodies_implies_ruleBody_PIPE_ruleBodies = \body () bodies ->
      return $ body : bodies
  , ruleBody_implies =
      return []
  , ruleBody_implies_symbol_ruleBody = \symbol body ->
      return $ symbol : body
  , symbol_implies_terminal = \terminal ->
      return $ Terminal terminal
  , symbol_implies_nonterminal = \nonterminal ->
      return $ Nonterminal nonterminal
  , terminal_implies_TERMINAL =
      return
  , nonterminal_implies_NONTERMINAL =
      return
  , codes_implies =
      return []
  , codes_implies_code_codes = \code codes ->
      return (code : codes)
  , code_implies_CODE = return }


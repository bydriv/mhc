module  Language.HsYacc.Parsing  where
import qualified Control.Monad as Monad


type CODE    = Char
type COLONEQ = ()
type DEF = ()
type NONTERMINAL = String
type PIPE = ()
type PLBRACE = ()
type PMODULE = ()
type PP = ()
type PRBRACE = ()
type PSTART = ()
type PWHERE = ()
type RULE = ()
type TERMINAL = String

type Start = (Definitions, Rules, Codes)

type Definitions = [Definition]

data Definition =
    DefnStart Nonterminal
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
    (0, Token (PSTART _)) -> Just (Shift 19)
    (0, Token (PMODULE _)) -> Just (Shift 13)
    (0, Token (PLBRACE _)) -> Just (Shift 15)
    (1, EOF) -> Just (Accept)
    (2, Token (PP _)) -> Just (Reduce 0 6)
    (2, Token (DEF _)) -> Just (Shift 32)
    (3, EOF) -> Just (Reduce 0 17)
    (3, Token (CODE _)) -> Just (Shift 41)
    (4, Token (PP _)) -> Just (Shift 2)
    (5, Token (PP _)) -> Just (Shift 3)
    (6, EOF) -> Just (Reduce 5 0)
    (7, Token (PP _)) -> Just (Reduce 0 1)
    (7, Token (PSTART _)) -> Just (Shift 19)
    (7, Token (PMODULE _)) -> Just (Shift 13)
    (7, Token (PLBRACE _)) -> Just (Shift 15)
    (8, Token (PP _)) -> Just (Reduce 2 2)
    (9, Token (PP _)) -> Just (Reduce 0 6)
    (9, Token (DEF _)) -> Just (Shift 32)
    (10, Token (PP _)) -> Just (Reduce 2 7)
    (11, EOF) -> Just (Reduce 0 17)
    (11, Token (CODE _)) -> Just (Shift 41)
    (12, Token (PWHERE _)) -> Just (Reduce 0 17)
    (12, Token (CODE _)) -> Just (Shift 42)
    (13, Token (PWHERE _)) -> Just (Reduce 0 17)
    (13, Token (CODE _)) -> Just (Shift 42)
    (14, Token (PRBRACE _)) -> Just (Reduce 0 17)
    (14, Token (CODE _)) -> Just (Shift 43)
    (15, Token (PRBRACE _)) -> Just (Reduce 0 17)
    (15, Token (CODE _)) -> Just (Shift 43)
    (16, EOF) -> Just (Reduce 2 18)
    (17, Token (PWHERE _)) -> Just (Reduce 2 18)
    (18, Token (PRBRACE _)) -> Just (Reduce 2 18)
    (19, Token (NONTERMINAL _)) -> Just (Shift 29)
    (20, Token (PP _)) -> Just (Reduce 3 4)
    (20, Token (PSTART _)) -> Just (Reduce 3 4)
    (20, Token (PMODULE _)) -> Just (Reduce 3 4)
    (20, Token (PLBRACE _)) -> Just (Reduce 3 4)
    (20, Token (NONTERMINAL _)) -> Just (Reduce 3 4)
    (20, Token (CODE _)) -> Just (Reduce 3 4)
    (21, Token (PP _)) -> Just (Reduce 3 5)
    (21, Token (PSTART _)) -> Just (Reduce 3 5)
    (21, Token (PMODULE _)) -> Just (Reduce 3 5)
    (21, Token (PLBRACE _)) -> Just (Reduce 3 5)
    (21, Token (NONTERMINAL _)) -> Just (Reduce 3 5)
    (21, Token (CODE _)) -> Just (Reduce 3 5)
    (22, Token (PWHERE _)) -> Just (Shift 20)
    (23, Token (PRBRACE _)) -> Just (Shift 21)
    (24, Token (PP _)) -> Just (Reduce 2 3)
    (24, Token (PSTART _)) -> Just (Reduce 2 3)
    (24, Token (PMODULE _)) -> Just (Reduce 2 3)
    (24, Token (PLBRACE _)) -> Just (Reduce 2 3)
    (24, Token (NONTERMINAL _)) -> Just (Reduce 2 3)
    (24, Token (CODE _)) -> Just (Reduce 2 3)
    (25, Token (PP _)) -> Just (Reduce 0 11)
    (25, Token (DEF _)) -> Just (Reduce 0 11)
    (25, Token (RULE _)) -> Just (Reduce 0 11)
    (25, Token (PIPE _)) -> Just (Reduce 0 11)
    (25, Token (TERMINAL _)) -> Just (Shift 40)
    (25, Token (NONTERMINAL _)) -> Just (Shift 30)
    (26, Token (PP _)) -> Just (Reduce 0 11)
    (26, Token (DEF _)) -> Just (Reduce 0 11)
    (26, Token (RULE _)) -> Just (Reduce 0 11)
    (26, Token (PIPE _)) -> Just (Reduce 0 11)
    (26, Token (TERMINAL _)) -> Just (Shift 40)
    (26, Token (NONTERMINAL _)) -> Just (Shift 30)
    (27, Token (PP _)) -> Just (Reduce 0 11)
    (27, Token (DEF _)) -> Just (Reduce 0 11)
    (27, Token (RULE _)) -> Just (Reduce 0 11)
    (27, Token (PIPE _)) -> Just (Reduce 0 11)
    (27, Token (TERMINAL _)) -> Just (Shift 40)
    (27, Token (NONTERMINAL _)) -> Just (Shift 30)
    (28, Token (NONTERMINAL _)) -> Just (Shift 31)
    (29, Token (PP _)) -> Just (Reduce 1 14)
    (29, Token (PSTART _)) -> Just (Reduce 1 14)
    (29, Token (PMODULE _)) -> Just (Reduce 1 14)
    (29, Token (PLBRACE _)) -> Just (Reduce 1 14)
    (29, Token (NONTERMINAL _)) -> Just (Reduce 1 14)
    (29, Token (CODE _)) -> Just (Reduce 1 14)
    (30, Token (PP _)) -> Just (Reduce 1 14)
    (30, Token (DEF _)) -> Just (Reduce 1 14)
    (30, Token (RULE _)) -> Just (Reduce 1 14)
    (30, Token (PIPE _)) -> Just (Reduce 1 14)
    (30, Token (TERMINAL _)) -> Just (Reduce 1 14)
    (30, Token (NONTERMINAL _)) -> Just (Reduce 1 14)
    (31, Token (COLONEQ _)) -> Just (Reduce 1 14)
    (32, Token (RULE _)) -> Just (Shift 28)
    (33, Token (COLONEQ _)) -> Just (Shift 25)
    (34, Token (PP _)) -> Just (Reduce 5 8)
    (34, Token (DEF _)) -> Just (Reduce 5 8)
    (34, Token (RULE _)) -> Just (Reduce 5 8)
    (35, Token (PP _)) -> Just (Reduce 3 10)
    (35, Token (DEF _)) -> Just (Reduce 3 10)
    (35, Token (RULE _)) -> Just (Reduce 3 10)
    (36, Token (PP _)) -> Just (Reduce 1 9)
    (36, Token (DEF _)) -> Just (Reduce 1 9)
    (36, Token (RULE _)) -> Just (Reduce 1 9)
    (36, Token (PIPE _)) -> Just (Shift 26)
    (37, Token (PP _)) -> Just (Reduce 2 12)
    (37, Token (DEF _)) -> Just (Reduce 2 12)
    (37, Token (RULE _)) -> Just (Reduce 2 12)
    (37, Token (PIPE _)) -> Just (Reduce 2 12)
    (38, Token (PP _)) -> Just (Reduce 1 16)
    (38, Token (DEF _)) -> Just (Reduce 1 16)
    (38, Token (RULE _)) -> Just (Reduce 1 16)
    (38, Token (PIPE _)) -> Just (Reduce 1 16)
    (38, Token (TERMINAL _)) -> Just (Reduce 1 16)
    (38, Token (NONTERMINAL _)) -> Just (Reduce 1 16)
    (39, Token (PP _)) -> Just (Reduce 1 15)
    (39, Token (DEF _)) -> Just (Reduce 1 15)
    (39, Token (RULE _)) -> Just (Reduce 1 15)
    (39, Token (PIPE _)) -> Just (Reduce 1 15)
    (39, Token (TERMINAL _)) -> Just (Reduce 1 15)
    (39, Token (NONTERMINAL _)) -> Just (Reduce 1 15)
    (40, Token (PP _)) -> Just (Reduce 1 13)
    (40, Token (DEF _)) -> Just (Reduce 1 13)
    (40, Token (RULE _)) -> Just (Reduce 1 13)
    (40, Token (PIPE _)) -> Just (Reduce 1 13)
    (40, Token (TERMINAL _)) -> Just (Reduce 1 13)
    (40, Token (NONTERMINAL _)) -> Just (Reduce 1 13)
    (41, EOF) -> Just (Reduce 1 19)
    (41, Token (CODE _)) -> Just (Reduce 1 19)
    (42, Token (PWHERE _)) -> Just (Reduce 1 19)
    (42, Token (CODE _)) -> Just (Reduce 1 19)
    (43, Token (PRBRACE _)) -> Just (Reduce 1 19)
    (43, Token (CODE _)) -> Just (Reduce 1 19)
    (_, _) -> Nothing

production :: Int -> Int
production 0 = 0
production 1 = 1
production 2 = 1
production 3 = 4
production 4 = 4
production 5 = 4
production 6 = 2
production 7 = 2
production 8 = 6
production 9 = 7
production 10 = 7
production 11 = 8
production 12 = 8
production 13 = 10
production 14 = 5
production 15 = 9
production 16 = 9
production 17 = 3
production 18 = 3
production 19 = 11

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
    (11, 3) -> Just 16
    (11, 11) -> Just 11
    (12, 3) -> Just 17
    (12, 11) -> Just 12
    (13, 3) -> Just 22
    (13, 11) -> Just 12
    (14, 3) -> Just 18
    (14, 11) -> Just 14
    (15, 3) -> Just 23
    (15, 11) -> Just 14
    (19, 5) -> Just 24
    (25, 5) -> Just 38
    (25, 7) -> Just 34
    (25, 8) -> Just 36
    (25, 9) -> Just 27
    (25, 10) -> Just 39
    (26, 5) -> Just 38
    (26, 7) -> Just 35
    (26, 8) -> Just 36
    (26, 9) -> Just 27
    (26, 10) -> Just 39
    (27, 5) -> Just 38
    (27, 8) -> Just 37
    (27, 9) -> Just 27
    (27, 10) -> Just 39
    (28, 5) -> Just 33
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
                  Token (PSTART semanticValue) ->
                    StackValue_PSTART semanticValue
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
                return Nothing
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
                      Monad.liftM StackValue_definition $ definition_implies_PMODULE_codes_PWHERE actions (case snd (pop !! 2) of { StackValue_PMODULE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_codes value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_PWHERE value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_definition $ definition_implies_PLBRACE_codes_PRBRACE actions (case snd (pop !! 2) of { StackValue_PLBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_codes value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_PRBRACE value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_rules $ rules_implies actions
                    7 ->
                      Monad.liftM StackValue_rules $ rules_implies_rule'_rules actions (case snd (pop !! 1) of { StackValue_rule' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rules value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_rule' $ rule'_implies_DEF_RULE_nonterminal_COLONEQ_ruleBodies actions (case snd (pop !! 4) of { StackValue_DEF value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RULE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_nonterminal value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLONEQ value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBodies value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_ruleBodies $ ruleBodies_implies_ruleBody actions (case snd (pop !! 0) of { StackValue_ruleBody value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_ruleBodies $ ruleBodies_implies_ruleBody_PIPE_ruleBodies actions (case snd (pop !! 2) of { StackValue_ruleBody value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBodies value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_ruleBody $ ruleBody_implies actions
                    12 ->
                      Monad.liftM StackValue_ruleBody $ ruleBody_implies_symbol_ruleBody actions (case snd (pop !! 1) of { StackValue_symbol value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBody value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_terminal $ terminal_implies_TERMINAL actions (case snd (pop !! 0) of { StackValue_TERMINAL value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_nonterminal $ nonterminal_implies_NONTERMINAL actions (case snd (pop !! 0) of { StackValue_NONTERMINAL value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_symbol $ symbol_implies_terminal actions (case snd (pop !! 0) of { StackValue_terminal value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_symbol $ symbol_implies_nonterminal actions (case snd (pop !! 0) of { StackValue_nonterminal value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_codes $ codes_implies actions
                    18 ->
                      Monad.liftM StackValue_codes $ codes_implies_code_codes actions (case snd (pop !! 1) of { StackValue_code value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_codes value -> value; _ -> undefined })
                    19 ->
                      Monad.liftM StackValue_code $ code_implies_CODE actions (case snd (pop !! 0) of { StackValue_CODE value -> value; _ -> undefined })
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_start value)] -> return $ Just (value, tokens); _ -> return Nothing }



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


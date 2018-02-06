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
  deriving (Eq, Ord, Read, Show)
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
  let s' =
        case s of
          EOF -> -1
          Token (CODE _) -> 13
          Token (COLONEQ _) -> 9
          Token (DEF _) -> 7
          Token (NONTERMINAL _) -> 12
          Token (PIPE _) -> 10
          Token (PLBRACE _) -> 5
          Token (PMODULE _) -> 3
          Token (PP _) -> 0
          Token (PRBRACE _) -> 6
          Token (PSTART _) -> 1
          Token (PTRIVIAL _) -> 2
          Token (PWHERE _) -> 4
          Token (RULE _) -> 8
          Token (TERMINAL _) -> 11
  in case compare(q,s')(22,7)of{LT->case compare(q,s')(12,4)of{LT->case compare(q,s')(7,3)of{LT->case compare(q,s')(4,0)of{LT->case compare(q,s')(0,5)of{LT->case compare(q,s')(0,2)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just(Reduce 0 1);GT->case compare(q,s')(0,1)of{LT->Nothing;EQ->Just(Shift 15);GT->Nothing}};EQ->Just(Shift 16);GT->case compare(q,s')(0,3)of{LT->Nothing;EQ->Just(Shift 12);GT->Nothing}};EQ->Just(Shift 13);GT->case compare(q,s')(2,0)of{LT->case compare(q,s')(1,-1)of{LT->Nothing;EQ->Just(Accept);GT->Nothing};EQ->Just(Reduce 0 7);GT->case compare(q,s')(3,-1)of{LT->case compare(q,s')(2,7)of{LT->Nothing;EQ->Just(Shift 27);GT->Nothing};EQ->Just(Reduce 0 18);GT->case compare(q,s')(3,13)of{LT->Nothing;EQ->Just(Shift 36);GT->Nothing}}}};EQ->Just(Shift 2);GT->case compare(q,s')(6,-1)of{LT->case compare(q,s')(5,0)of{LT->Nothing;EQ->Just(Shift 3);GT->Nothing};EQ->Just(Reduce 5 0);GT->case compare(q,s')(7,1)of{LT->case compare(q,s')(7,0)of{LT->Nothing;EQ->Just(Reduce 0 1);GT->Nothing};EQ->Just(Shift 15);GT->case compare(q,s')(7,2)of{LT->Nothing;EQ->Just(Shift 16);GT->Nothing}}}};EQ->Just(Shift 12);GT->case compare(q,s')(9,0)of{LT->case compare(q,s')(8,0)of{LT->case compare(q,s')(7,5)of{LT->Nothing;EQ->Just(Shift 13);GT->Nothing};EQ->Just(Reduce 2 2);GT->Nothing};EQ->Just(Reduce 0 7);GT->case compare(q,s')(11,4)of{LT->case compare(q,s')(11,-1)of{LT->case compare(q,s')(10,0)of{LT->case compare(q,s')(9,7)of{LT->Nothing;EQ->Just(Shift 27);GT->Nothing};EQ->Just(Reduce 2 8);GT->Nothing};EQ->Just(Reduce 0 18);GT->Nothing};EQ->Just(Reduce 0 18);GT->case compare(q,s')(11,6)of{LT->Nothing;EQ->Just(Reduce 0 18);GT->case compare(q,s')(11,13)of{LT->Nothing;EQ->Just(Shift 36);GT->Nothing}}}}};EQ->Just(Reduce 0 18);GT->case compare(q,s')(17,2)of{LT->case compare(q,s')(13,6)of{LT->case compare(q,s')(12,13)of{LT->Nothing;EQ->Just(Shift 36);GT->Nothing};EQ->Just(Reduce 0 18);GT->case compare(q,s')(16,3)of{LT->case compare(q,s')(16,0)of{LT->case compare(q,s')(14,4)of{LT->case compare(q,s')(14,-1)of{LT->case compare(q,s')(13,13)of{LT->Nothing;EQ->Just(Shift 36);GT->Nothing};EQ->Just(Reduce 2 19);GT->Nothing};EQ->Just(Reduce 2 19);GT->case compare(q,s')(14,6)of{LT->Nothing;EQ->Just(Reduce 2 19);GT->case compare(q,s')(15,12)of{LT->Nothing;EQ->Just(Shift 26);GT->Nothing}}};EQ->Just(Reduce 1 4);GT->case compare(q,s')(16,2)of{LT->case compare(q,s')(16,1)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing}};EQ->Just(Reduce 1 4);GT->case compare(q,s')(16,13)of{LT->case compare(q,s')(16,12)of{LT->case compare(q,s')(16,5)of{LT->Nothing;EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->Nothing};EQ->Just(Reduce 1 4);GT->case compare(q,s')(17,1)of{LT->case compare(q,s')(17,0)of{LT->Nothing;EQ->Just(Reduce 3 5);GT->Nothing};EQ->Just(Reduce 3 5);GT->Nothing}}}};EQ->Just(Reduce 3 5);GT->case compare(q,s')(19,4)of{LT->case compare(q,s')(18,2)of{LT->case compare(q,s')(17,13)of{LT->case compare(q,s')(17,5)of{LT->case compare(q,s')(17,3)of{LT->Nothing;EQ->Just(Reduce 3 5);GT->Nothing};EQ->Just(Reduce 3 5);GT->case compare(q,s')(17,12)of{LT->Nothing;EQ->Just(Reduce 3 5);GT->Nothing}};EQ->Just(Reduce 3 5);GT->case compare(q,s')(18,1)of{LT->case compare(q,s')(18,0)of{LT->Nothing;EQ->Just(Reduce 3 6);GT->Nothing};EQ->Just(Reduce 3 6);GT->Nothing}};EQ->Just(Reduce 3 6);GT->case compare(q,s')(18,12)of{LT->case compare(q,s')(18,5)of{LT->case compare(q,s')(18,3)of{LT->Nothing;EQ->Just(Reduce 3 6);GT->Nothing};EQ->Just(Reduce 3 6);GT->Nothing};EQ->Just(Reduce 3 6);GT->case compare(q,s')(18,13)of{LT->Nothing;EQ->Just(Reduce 3 6);GT->Nothing}}};EQ->Just(Shift 17);GT->case compare(q,s')(21,1)of{LT->case compare(q,s')(21,0)of{LT->case compare(q,s')(20,6)of{LT->Nothing;EQ->Just(Shift 18);GT->Nothing};EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->case compare(q,s')(21,12)of{LT->case compare(q,s')(21,3)of{LT->case compare(q,s')(21,2)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 2 3);GT->case compare(q,s')(21,5)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing}};EQ->Just(Reduce 2 3);GT->case compare(q,s')(22,0)of{LT->case compare(q,s')(21,13)of{LT->Nothing;EQ->Just(Reduce 2 3);GT->Nothing};EQ->Just(Reduce 0 12);GT->Nothing}}}}}};EQ->Just(Reduce 0 12);GT->case compare(q,s')(30,0)of{LT->case compare(q,s')(23,10)of{LT->case compare(q,s')(22,12)of{LT->case compare(q,s')(22,10)of{LT->case compare(q,s')(22,8)of{LT->Nothing;EQ->Just(Reduce 0 12);GT->Nothing};EQ->Just(Reduce 0 12);GT->case compare(q,s')(22,11)of{LT->Nothing;EQ->Just(Shift 35);GT->Nothing}};EQ->Just(Shift 26);GT->case compare(q,s')(23,0)of{LT->Nothing;EQ->Just(Reduce 0 12);GT->case compare(q,s')(23,8)of{LT->case compare(q,s')(23,7)of{LT->Nothing;EQ->Just(Reduce 0 12);GT->Nothing};EQ->Just(Reduce 0 12);GT->Nothing}}};EQ->Just(Reduce 0 12);GT->case compare(q,s')(26,2)of{LT->case compare(q,s')(25,12)of{LT->case compare(q,s')(24,7)of{LT->case compare(q,s')(23,12)of{LT->case compare(q,s')(23,11)of{LT->Nothing;EQ->Just(Shift 35);GT->Nothing};EQ->Just(Shift 26);GT->case compare(q,s')(24,0)of{LT->Nothing;EQ->Just(Reduce 0 12);GT->Nothing}};EQ->Just(Reduce 0 12);GT->case compare(q,s')(24,10)of{LT->case compare(q,s')(24,8)of{LT->Nothing;EQ->Just(Reduce 0 12);GT->Nothing};EQ->Just(Reduce 0 12);GT->case compare(q,s')(24,12)of{LT->case compare(q,s')(24,11)of{LT->Nothing;EQ->Just(Shift 35);GT->Nothing};EQ->Just(Shift 26);GT->Nothing}}};EQ->Just(Shift 26);GT->case compare(q,s')(26,1)of{LT->case compare(q,s')(26,0)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->Nothing}};EQ->Just(Reduce 1 15);GT->case compare(q,s')(26,10)of{LT->case compare(q,s')(26,7)of{LT->case compare(q,s')(26,5)of{LT->case compare(q,s')(26,3)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->case compare(q,s')(26,9)of{LT->case compare(q,s')(26,8)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->Nothing}};EQ->Just(Reduce 1 15);GT->case compare(q,s')(27,8)of{LT->case compare(q,s')(26,12)of{LT->case compare(q,s')(26,11)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing};EQ->Just(Reduce 1 15);GT->case compare(q,s')(26,13)of{LT->Nothing;EQ->Just(Reduce 1 15);GT->Nothing}};EQ->Just(Shift 25);GT->case compare(q,s')(29,0)of{LT->case compare(q,s')(28,9)of{LT->Nothing;EQ->Just(Shift 22);GT->Nothing};EQ->Just(Reduce 5 9);GT->case compare(q,s')(29,8)of{LT->case compare(q,s')(29,7)of{LT->Nothing;EQ->Just(Reduce 5 9);GT->Nothing};EQ->Just(Reduce 5 9);GT->Nothing}}}}}};EQ->Just(Reduce 3 11);GT->case compare(q,s')(32,0)of{LT->case compare(q,s')(31,8)of{LT->case compare(q,s')(31,0)of{LT->case compare(q,s')(30,8)of{LT->case compare(q,s')(30,7)of{LT->Nothing;EQ->Just(Reduce 3 11);GT->Nothing};EQ->Just(Reduce 3 11);GT->Nothing};EQ->Just(Reduce 1 10);GT->case compare(q,s')(31,7)of{LT->Nothing;EQ->Just(Reduce 1 10);GT->Nothing}};EQ->Just(Reduce 1 10);GT->case compare(q,s')(31,10)of{LT->Nothing;EQ->Just(Shift 23);GT->Nothing}};EQ->Just(Reduce 2 13);GT->case compare(q,s')(34,10)of{LT->case compare(q,s')(33,10)of{LT->case compare(q,s')(33,0)of{LT->case compare(q,s')(32,8)of{LT->case compare(q,s')(32,7)of{LT->Nothing;EQ->Just(Reduce 2 13);GT->Nothing};EQ->Just(Reduce 2 13);GT->case compare(q,s')(32,10)of{LT->Nothing;EQ->Just(Reduce 2 13);GT->Nothing}};EQ->Just(Reduce 1 17);GT->case compare(q,s')(33,8)of{LT->case compare(q,s')(33,7)of{LT->Nothing;EQ->Just(Reduce 1 17);GT->Nothing};EQ->Just(Reduce 1 17);GT->Nothing}};EQ->Just(Reduce 1 17);GT->case compare(q,s')(34,0)of{LT->case compare(q,s')(33,12)of{LT->case compare(q,s')(33,11)of{LT->Nothing;EQ->Just(Reduce 1 17);GT->Nothing};EQ->Just(Reduce 1 17);GT->Nothing};EQ->Just(Reduce 1 16);GT->case compare(q,s')(34,8)of{LT->case compare(q,s')(34,7)of{LT->Nothing;EQ->Just(Reduce 1 16);GT->Nothing};EQ->Just(Reduce 1 16);GT->Nothing}}};EQ->Just(Reduce 1 16);GT->case compare(q,s')(35,11)of{LT->case compare(q,s')(35,7)of{LT->case compare(q,s')(34,12)of{LT->case compare(q,s')(34,11)of{LT->Nothing;EQ->Just(Reduce 1 16);GT->Nothing};EQ->Just(Reduce 1 16);GT->case compare(q,s')(35,0)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(35,10)of{LT->case compare(q,s')(35,8)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 14);GT->Nothing}};EQ->Just(Reduce 1 14);GT->case compare(q,s')(36,4)of{LT->case compare(q,s')(36,-1)of{LT->case compare(q,s')(35,12)of{LT->Nothing;EQ->Just(Reduce 1 14);GT->Nothing};EQ->Just(Reduce 1 20);GT->Nothing};EQ->Just(Reduce 1 20);GT->case compare(q,s')(36,13)of{LT->case compare(q,s')(36,6)of{LT->Nothing;EQ->Just(Reduce 1 20);GT->Nothing};EQ->Just(Reduce 1 20);GT->Nothing}}}}}}}

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
  let s' = production s in
    case compare(q,s')(13,11)of{LT->case compare(q,s')(7,4)of{LT->case compare(q,s')(2,6)of{LT->case compare(q,s')(0,4)of{LT->case compare(q,s')(0,1)of{LT->case compare(q,s')(0,0)of{LT->Nothing;EQ->Just 1;GT->Nothing};EQ->Just 4;GT->Nothing};EQ->Just 7;GT->case compare(q,s')(2,2)of{LT->Nothing;EQ->Just 5;GT->Nothing}};EQ->Just 9;GT->case compare(q,s')(3,11)of{LT->case compare(q,s')(3,3)of{LT->Nothing;EQ->Just 6;GT->Nothing};EQ->Just 11;GT->case compare(q,s')(7,1)of{LT->Nothing;EQ->Just 8;GT->Nothing}}};EQ->Just 7;GT->case compare(q,s')(11,11)of{LT->case compare(q,s')(9,6)of{LT->case compare(q,s')(9,2)of{LT->Nothing;EQ->Just 10;GT->Nothing};EQ->Just 9;GT->case compare(q,s')(11,3)of{LT->Nothing;EQ->Just 14;GT->Nothing}};EQ->Just 11;GT->case compare(q,s')(12,11)of{LT->case compare(q,s')(12,3)of{LT->Nothing;EQ->Just 19;GT->Nothing};EQ->Just 11;GT->case compare(q,s')(13,3)of{LT->Nothing;EQ->Just 20;GT->Nothing}}}};EQ->Just 11;GT->case compare(q,s')(23,8)of{LT->case compare(q,s')(22,9)of{LT->case compare(q,s')(22,7)of{LT->case compare(q,s')(22,5)of{LT->case compare(q,s')(15,5)of{LT->Nothing;EQ->Just 21;GT->Nothing};EQ->Just 33;GT->Nothing};EQ->Just 29;GT->case compare(q,s')(22,8)of{LT->Nothing;EQ->Just 31;GT->Nothing}};EQ->Just 24;GT->case compare(q,s')(23,5)of{LT->case compare(q,s')(22,10)of{LT->Nothing;EQ->Just 34;GT->Nothing};EQ->Just 33;GT->case compare(q,s')(23,7)of{LT->Nothing;EQ->Just 30;GT->Nothing}}};EQ->Just 31;GT->case compare(q,s')(24,8)of{LT->case compare(q,s')(23,10)of{LT->case compare(q,s')(23,9)of{LT->Nothing;EQ->Just 24;GT->Nothing};EQ->Just 34;GT->case compare(q,s')(24,5)of{LT->Nothing;EQ->Just 33;GT->Nothing}};EQ->Just 32;GT->case compare(q,s')(24,10)of{LT->case compare(q,s')(24,9)of{LT->Nothing;EQ->Just 24;GT->Nothing};EQ->Just 34;GT->case compare(q,s')(25,5)of{LT->Nothing;EQ->Just 28;GT->Nothing}}}}}

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


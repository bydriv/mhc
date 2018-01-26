module Language.HsYacc.Parsing where

import qualified Control.Monad as Monad

type COLONEQ = ()         -- `:='
type DEF = ()             -- `def'
type NONTERMINAL = String -- nonterminal symbols
type PIPE = ()            -- `|'
type PP = ()              -- `%%'
type RULE = ()            -- `rule'
type TERMINAL = String    -- terminal symbols

type Start = (Definitions, Rules, Codes)
type Definitions = () -- TODO.
type Rules = [Rule']
type Rule' = (NONTERMINAL, RuleBodies)
type RuleBodies = [RuleBody]
type RuleBody = [Symbol]
type Codes = () -- TODO.
data Symbol = Terminal Terminal | Nonterminal Nonterminal
  deriving (Eq, Ord, Read, Show)
type Terminal = TERMINAL
type Nonterminal = NONTERMINAL

semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { start_implies_definitions_PP_rules_PP_codes = \defns () rules () codes ->
      return (defns, rules, codes)
  , definitions_implies =
      return ()
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
      return () }

data Token =
    COLONEQ COLONEQ
  | DEF DEF
  | NONTERMINAL NONTERMINAL
  | PIPE PIPE
  | PP PP
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
  | StackValue_DEF DEF
  | StackValue_RULE RULE
  | StackValue_COLONEQ COLONEQ
  | StackValue_PIPE PIPE
  | StackValue_TERMINAL TERMINAL
  | StackValue_NONTERMINAL NONTERMINAL
  | StackValue_start Start
  | StackValue_definitions Definitions
  | StackValue_rules Rules
  | StackValue_codes Codes
  | StackValue_rule' Rule'
  | StackValue_nonterminal Nonterminal
  | StackValue_ruleBodies RuleBodies
  | StackValue_ruleBody RuleBody
  | StackValue_symbol Symbol
  | StackValue_terminal Terminal

data SemanticActions m = SemanticActions
  { start_implies_definitions_PP_rules_PP_codes :: Definitions -> PP -> Rules -> PP -> Codes -> m Start
  , definitions_implies :: m Definitions
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
  , codes_implies :: m Codes }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token (PP _)) -> Just (Reduce 0 1)
    (1, EOF) -> Just (Accept)
    (2, Token (PP _)) -> Just (Reduce 0 2)
    (2, Token (DEF _)) -> Just (Shift 9)
    (3, EOF) -> Just (Reduce 0 13)
    (4, Token (PP _)) -> Just (Shift 2)
    (5, Token (PP _)) -> Just (Shift 3)
    (6, EOF) -> Just (Reduce 5 0)
    (7, Token (PP _)) -> Just (Reduce 0 2)
    (7, Token (DEF _)) -> Just (Shift 9)
    (8, Token (PP _)) -> Just (Reduce 2 3)
    (9, Token (RULE _)) -> Just (Shift 10)
    (10, Token (NONTERMINAL _)) -> Just (Shift 17)
    (11, Token (PP _)) -> Just (Reduce 0 7)
    (11, Token (DEF _)) -> Just (Reduce 0 7)
    (11, Token (RULE _)) -> Just (Reduce 0 7)
    (11, Token (PIPE _)) -> Just (Reduce 0 7)
    (11, Token (TERMINAL _)) -> Just (Shift 23)
    (11, Token (NONTERMINAL _)) -> Just (Shift 16)
    (12, Token (COLONEQ _)) -> Just (Shift 11)
    (13, Token (PP _)) -> Just (Reduce 5 4)
    (13, Token (DEF _)) -> Just (Reduce 5 4)
    (13, Token (RULE _)) -> Just (Reduce 5 4)
    (14, Token (PP _)) -> Just (Reduce 0 7)
    (14, Token (DEF _)) -> Just (Reduce 0 7)
    (14, Token (RULE _)) -> Just (Reduce 0 7)
    (14, Token (PIPE _)) -> Just (Reduce 0 7)
    (14, Token (TERMINAL _)) -> Just (Shift 23)
    (14, Token (NONTERMINAL _)) -> Just (Shift 16)
    (15, Token (PP _)) -> Just (Reduce 0 7)
    (15, Token (DEF _)) -> Just (Reduce 0 7)
    (15, Token (RULE _)) -> Just (Reduce 0 7)
    (15, Token (PIPE _)) -> Just (Reduce 0 7)
    (15, Token (TERMINAL _)) -> Just (Shift 23)
    (15, Token (NONTERMINAL _)) -> Just (Shift 16)
    (16, Token (PP _)) -> Just (Reduce 1 10)
    (16, Token (DEF _)) -> Just (Reduce 1 10)
    (16, Token (RULE _)) -> Just (Reduce 1 10)
    (16, Token (PIPE _)) -> Just (Reduce 1 10)
    (16, Token (TERMINAL _)) -> Just (Reduce 1 10)
    (16, Token (NONTERMINAL _)) -> Just (Reduce 1 10)
    (17, Token (COLONEQ _)) -> Just (Reduce 1 10)
    (18, Token (PP _)) -> Just (Reduce 3 6)
    (18, Token (DEF _)) -> Just (Reduce 3 6)
    (18, Token (RULE _)) -> Just (Reduce 3 6)
    (19, Token (PP _)) -> Just (Reduce 1 5)
    (19, Token (DEF _)) -> Just (Reduce 1 5)
    (19, Token (RULE _)) -> Just (Reduce 1 5)
    (19, Token (PIPE _)) -> Just (Shift 14)
    (20, Token (PP _)) -> Just (Reduce 2 8)
    (20, Token (DEF _)) -> Just (Reduce 2 8)
    (20, Token (RULE _)) -> Just (Reduce 2 8)
    (20, Token (PIPE _)) -> Just (Reduce 2 8)
    (21, Token (PP _)) -> Just (Reduce 1 12)
    (21, Token (DEF _)) -> Just (Reduce 1 12)
    (21, Token (RULE _)) -> Just (Reduce 1 12)
    (21, Token (PIPE _)) -> Just (Reduce 1 12)
    (21, Token (TERMINAL _)) -> Just (Reduce 1 12)
    (21, Token (NONTERMINAL _)) -> Just (Reduce 1 12)
    (22, Token (PP _)) -> Just (Reduce 1 11)
    (22, Token (DEF _)) -> Just (Reduce 1 11)
    (22, Token (RULE _)) -> Just (Reduce 1 11)
    (22, Token (PIPE _)) -> Just (Reduce 1 11)
    (22, Token (TERMINAL _)) -> Just (Reduce 1 11)
    (22, Token (NONTERMINAL _)) -> Just (Reduce 1 11)
    (23, Token (PP _)) -> Just (Reduce 1 9)
    (23, Token (DEF _)) -> Just (Reduce 1 9)
    (23, Token (RULE _)) -> Just (Reduce 1 9)
    (23, Token (PIPE _)) -> Just (Reduce 1 9)
    (23, Token (TERMINAL _)) -> Just (Reduce 1 9)
    (23, Token (NONTERMINAL _)) -> Just (Reduce 1 9)
    (_, _) -> Nothing

production :: Int -> Int
production 0 = 0
production 1 = 1
production 2 = 2
production 3 = 2
production 4 = 4
production 5 = 6
production 6 = 6
production 7 = 7
production 8 = 7
production 9 = 9
production 10 = 5
production 11 = 8
production 12 = 8
production 13 = 3

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 1) -> Just 4
    (2, 2) -> Just 5
    (2, 4) -> Just 7
    (3, 3) -> Just 6
    (7, 2) -> Just 8
    (7, 4) -> Just 7
    (10, 5) -> Just 12
    (11, 5) -> Just 21
    (11, 6) -> Just 13
    (11, 7) -> Just 19
    (11, 8) -> Just 15
    (11, 9) -> Just 22
    (14, 5) -> Just 21
    (14, 6) -> Just 18
    (14, 7) -> Just 19
    (14, 8) -> Just 15
    (14, 9) -> Just 22
    (15, 5) -> Just 21
    (15, 7) -> Just 20
    (15, 8) -> Just 15
    (15, 9) -> Just 22
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
                      Monad.liftM StackValue_rules $ rules_implies actions
                    3 ->
                      Monad.liftM StackValue_rules $ rules_implies_rule'_rules actions (case snd (pop !! 1) of { StackValue_rule' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rules value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_rule' $ rule'_implies_DEF_RULE_nonterminal_COLONEQ_ruleBodies actions (case snd (pop !! 4) of { StackValue_DEF value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RULE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_nonterminal value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLONEQ value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBodies value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_ruleBodies $ ruleBodies_implies_ruleBody actions (case snd (pop !! 0) of { StackValue_ruleBody value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_ruleBodies $ ruleBodies_implies_ruleBody_PIPE_ruleBodies actions (case snd (pop !! 2) of { StackValue_ruleBody value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBodies value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_ruleBody $ ruleBody_implies actions
                    8 ->
                      Monad.liftM StackValue_ruleBody $ ruleBody_implies_symbol_ruleBody actions (case snd (pop !! 1) of { StackValue_symbol value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ruleBody value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_terminal $ terminal_implies_TERMINAL actions (case snd (pop !! 0) of { StackValue_TERMINAL value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_nonterminal $ nonterminal_implies_NONTERMINAL actions (case snd (pop !! 0) of { StackValue_NONTERMINAL value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_symbol $ symbol_implies_terminal actions (case snd (pop !! 0) of { StackValue_terminal value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_symbol $ symbol_implies_nonterminal actions (case snd (pop !! 0) of { StackValue_nonterminal value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_codes $ codes_implies actions
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_start value)] -> return $ Just (value, tokens); _ -> return Nothing }


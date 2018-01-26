import qualified Control.Monad as Monad
---------------------------------------------------
-- user defined header.
import qualified Control.Monad.Identity as Identity

type VAR = String
type LPAREN = ()
type RPAREN = ()
type HAT = ()
type DOT = ()
type Atom = String
type Abstraction = String
type Application = String

semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { atom_implies_VAR = return
  , atom_implies_LPAREN_abstraction_RPAREN = \() x () -> return x
  , application_implies_atom = return
  , application_implies_application_atom = \m n -> return $ "(" ++ m ++ n ++ ")"
  , abstraction_implies_application = return
  , abstraction_implies_HAT_VAR_DOT_abstraction = \() x () m -> return $ "(^" ++ x ++ "." ++ m ++ ")" }

parseTokens :: [Token] -> Maybe (Abstraction, [Token])
parseTokens = Identity.runIdentity . parse semanticActions

-- test
main :: IO ()
main =
  print $ parseTokens [HAT (), VAR "f", DOT (), LPAREN (), HAT (), VAR "x", DOT (), VAR "f", LPAREN (), VAR "x", VAR "x", RPAREN (), RPAREN (), LPAREN (), HAT (), VAR "x", DOT (), VAR "f", LPAREN (), VAR "x", VAR "x", RPAREN (),  RPAREN ()]
  -- Just ("(^f.((^x.(f(xx)))(^x.(f(xx)))))",[])
---------------------------------------------------

data Token =
    DOT DOT
  | HAT HAT
  | LPAREN LPAREN
  | RPAREN RPAREN
  | VAR VAR
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
type GotoState = Int
type GotoSymbol = Int

data StackValue =
    StackValue_EOF
  | StackValue_VAR VAR
  | StackValue_LPAREN LPAREN
  | StackValue_RPAREN RPAREN
  | StackValue_HAT HAT
  | StackValue_DOT DOT
  | StackValue_atom Atom
  | StackValue_abstraction Abstraction
  | StackValue_application Application

data SemanticActions m = SemanticActions
  { atom_implies_VAR :: VAR -> m Atom
  , atom_implies_LPAREN_abstraction_RPAREN :: LPAREN -> Abstraction -> RPAREN -> m Atom
  , application_implies_atom :: Atom -> m Application
  , application_implies_application_atom :: Application -> Atom -> m Application
  , abstraction_implies_application :: Application -> m Abstraction
  , abstraction_implies_HAT_VAR_DOT_abstraction :: HAT -> VAR -> DOT -> Abstraction -> m Abstraction }

dfaActionTransition :: ActionState -> ActionSymbol -> Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token (VAR _)) -> Shift 8
    (0, Token (LPAREN _)) -> Shift 4
    (0, Token (HAT _)) -> Shift 16
    (1, EOF) -> Accept
    (2, Token (VAR _)) -> Shift 8
    (2, Token (LPAREN _)) -> Shift 4
    (2, Token (HAT _)) -> Shift 16
    (3, EOF) -> Reduce 1 4
    (3, Token (VAR _)) -> Shift 8
    (3, Token (LPAREN _)) -> Shift 4
    (4, Token (VAR _)) -> Shift 9
    (4, Token (LPAREN _)) -> Shift 5
    (4, Token (HAT _)) -> Shift 17
    (5, Token (VAR _)) -> Shift 9
    (5, Token (LPAREN _)) -> Shift 5
    (5, Token (HAT _)) -> Shift 17
    (6, Token (VAR _)) -> Shift 9
    (6, Token (LPAREN _)) -> Shift 5
    (6, Token (HAT _)) -> Shift 17
    (7, Token (VAR _)) -> Shift 9
    (7, Token (LPAREN _)) -> Shift 5
    (7, Token (RPAREN _)) -> Reduce 1 4
    (8, EOF) -> Reduce 1 0
    (8, Token (VAR _)) -> Reduce 1 0
    (8, Token (LPAREN _)) -> Reduce 1 0
    (8, Token (HAT _)) -> Reduce 1 0
    (9, Token (VAR _)) -> Reduce 1 0
    (9, Token (LPAREN _)) -> Reduce 1 0
    (9, Token (RPAREN _)) -> Reduce 1 0
    (9, Token (HAT _)) -> Reduce 1 0
    (10, EOF) -> Reduce 3 1
    (10, Token (VAR _)) -> Reduce 3 1
    (10, Token (LPAREN _)) -> Reduce 3 1
    (10, Token (HAT _)) -> Reduce 3 1
    (11, Token (VAR _)) -> Reduce 3 1
    (11, Token (LPAREN _)) -> Reduce 3 1
    (11, Token (RPAREN _)) -> Reduce 3 1
    (11, Token (HAT _)) -> Reduce 3 1
    (12, Token (RPAREN _)) -> Shift 10
    (13, Token (RPAREN _)) -> Shift 11
    (14, Token (DOT _)) -> Shift 2
    (15, Token (DOT _)) -> Shift 6
    (16, Token (VAR _)) -> Shift 14
    (17, Token (VAR _)) -> Shift 15
    (18, EOF) -> Reduce 4 5
    (19, Token (RPAREN _)) -> Reduce 4 5
    (20, EOF) -> Reduce 1 2
    (20, Token (VAR _)) -> Reduce 1 2
    (20, Token (LPAREN _)) -> Reduce 1 2
    (20, Token (HAT _)) -> Reduce 1 2
    (21, Token (VAR _)) -> Reduce 1 2
    (21, Token (LPAREN _)) -> Reduce 1 2
    (21, Token (RPAREN _)) -> Reduce 1 2
    (21, Token (HAT _)) -> Reduce 1 2
    (22, EOF) -> Reduce 2 3
    (22, Token (VAR _)) -> Reduce 2 3
    (22, Token (LPAREN _)) -> Reduce 2 3
    (22, Token (HAT _)) -> Reduce 2 3
    (23, Token (VAR _)) -> Reduce 2 3
    (23, Token (LPAREN _)) -> Reduce 2 3
    (23, Token (RPAREN _)) -> Reduce 2 3
    (23, Token (HAT _)) -> Reduce 2 3

production :: Int -> Int
production 0 = 0
production 1 = 0
production 2 = 2
production 3 = 2
production 4 = 1
production 5 = 1

dfaGotoTransition :: GotoState -> GotoSymbol -> GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> 20
    (0, 1) -> 1
    (0, 2) -> 3
    (2, 0) -> 20
    (2, 1) -> 18
    (2, 2) -> 3
    (3, 0) -> 22
    (4, 0) -> 21
    (4, 1) -> 12
    (4, 2) -> 7
    (5, 0) -> 21
    (5, 1) -> 13
    (5, 2) -> 7
    (6, 0) -> 21
    (6, 1) -> 19
    (6, 2) -> 7
    (7, 0) -> 23
parse :: Monad m => SemanticActions m -> [Token] -> m (Maybe (Abstraction, [Token]))
parse actions = parse' [] where
  parse' stack tokens =
    let p =
          case stack of
            [] -> 0
            ((q, _) : _) -> q in
    let symbol =
          case tokens of
            [] -> EOF
            (token : _) -> Token token in
      case dfaActionTransition p symbol of
        Shift n ->
          let value =
                case symbol of
                  EOF ->
                    StackValue_EOF
                  Token (VAR semanticValue) ->
                    StackValue_VAR semanticValue
                  Token (LPAREN semanticValue) ->
                    StackValue_LPAREN semanticValue
                  Token (RPAREN semanticValue) ->
                    StackValue_RPAREN semanticValue
                  Token (HAT semanticValue) ->
                    StackValue_HAT semanticValue
                  Token (DOT semanticValue) ->
                    StackValue_DOT semanticValue
          in parse' ((n, value) : stack) (tail tokens)
        Reduce n m ->
          let (pop, stack') = splitAt n stack in
          let q =
                case stack' of
                  [] -> dfaGotoTransition 0 m
                  ((q', _) : _) -> dfaGotoTransition q' m in do
            value <-
              case m of
                0 ->
                  Monad.liftM StackValue_atom $ atom_implies_VAR actions (case snd (pop !! 0) of { StackValue_VAR value -> value; _ -> undefined })
                1 ->
                  Monad.liftM StackValue_atom $ atom_implies_LPAREN_abstraction_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_abstraction value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                2 ->
                  Monad.liftM StackValue_application $ application_implies_atom actions (case snd (pop !! 0) of { StackValue_atom value -> value; _ -> undefined })
                3 ->
                  Monad.liftM StackValue_application $ application_implies_application_atom actions (case snd (pop !! 1) of { StackValue_application value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atom value -> value; _ -> undefined })
                4 ->
                  Monad.liftM StackValue_abstraction $ abstraction_implies_application actions (case snd (pop !! 0) of { StackValue_application value -> value; _ -> undefined })
                5 ->
                  Monad.liftM StackValue_abstraction $ abstraction_implies_HAT_VAR_DOT_abstraction actions (case snd (pop !! 3) of { StackValue_HAT value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_VAR value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_abstraction value -> value; _ -> undefined })
            parse' ((q, value) : stack') tokens
        Accept ->
          case stack of { [(_, StackValue_abstraction value)] -> return $ Just (value, tokens); _ -> return Nothing }


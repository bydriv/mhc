import qualified Control.Monad as Monad
import qualified Control.Monad.Identity as Identity

data Token =
    DOT
  | HAT
  | LPAREN
  | RPAREN
  | VAR
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
type GotoState = Int
type GotoSymbol = Int

type Atom = String
type Application = String
type Abstraction = String

data StackValue =
    StackValueToken
  | StackValue_atom Atom
  | StackValue_abstraction Abstraction
  | StackValue_application Application

data SemanticActions m = SemanticActions
  { atom_implies_VAR :: m Atom
  , atom_implies_LPAREN_abstraction_RPAREN :: Abstraction -> m Atom
  , application_implies_atom :: Atom -> m Application
  , application_implies_application_atom :: Application -> Atom -> m Application
  , abstraction_implies_application :: Application -> m Abstraction
  , abstraction_implies_HAT_VAR_DOT_abstraction :: Abstraction -> m Abstraction }

dfaActionTransition :: ActionState -> ActionSymbol -> Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token VAR) -> Shift 8
    (0, Token LPAREN) -> Shift 4
    (0, Token HAT) -> Shift 16
    (1, EOF) -> Accept
    (2, Token VAR) -> Shift 8
    (2, Token LPAREN) -> Shift 4
    (2, Token HAT) -> Shift 16
    (3, EOF) -> Reduce 1 4
    (3, Token VAR) -> Shift 8
    (3, Token LPAREN) -> Shift 4
    (4, Token VAR) -> Shift 9
    (4, Token LPAREN) -> Shift 5
    (4, Token HAT) -> Shift 17
    (5, Token VAR) -> Shift 9
    (5, Token LPAREN) -> Shift 5
    (5, Token HAT) -> Shift 17
    (6, Token VAR) -> Shift 9
    (6, Token LPAREN) -> Shift 5
    (6, Token HAT) -> Shift 17
    (7, Token VAR) -> Shift 9
    (7, Token LPAREN) -> Shift 5
    (7, Token RPAREN) -> Reduce 1 4
    (8, EOF) -> Reduce 1 0
    (8, Token VAR) -> Reduce 1 0
    (8, Token LPAREN) -> Reduce 1 0
    (8, Token HAT) -> Reduce 1 0
    (9, Token VAR) -> Reduce 1 0
    (9, Token LPAREN) -> Reduce 1 0
    (9, Token RPAREN) -> Reduce 1 0
    (9, Token HAT) -> Reduce 1 0
    (10, EOF) -> Reduce 3 1
    (10, Token VAR) -> Reduce 3 1
    (10, Token LPAREN) -> Reduce 3 1
    (10, Token HAT) -> Reduce 3 1
    (11, Token VAR) -> Reduce 3 1
    (11, Token LPAREN) -> Reduce 3 1
    (11, Token RPAREN) -> Reduce 3 1
    (11, Token HAT) -> Reduce 3 1
    (12, Token RPAREN) -> Shift 10
    (13, Token RPAREN) -> Shift 11
    (14, Token DOT) -> Shift 2
    (15, Token DOT) -> Shift 6
    (16, Token VAR) -> Shift 14
    (17, Token VAR) -> Shift 15
    (18, EOF) -> Reduce 4 5
    (19, Token RPAREN) -> Reduce 4 5
    (20, EOF) -> Reduce 1 2
    (20, Token VAR) -> Reduce 1 2
    (20, Token LPAREN) -> Reduce 1 2
    (20, Token HAT) -> Reduce 1 2
    (21, Token VAR) -> Reduce 1 2
    (21, Token LPAREN) -> Reduce 1 2
    (21, Token RPAREN) -> Reduce 1 2
    (21, Token HAT) -> Reduce 1 2
    (22, EOF) -> Reduce 2 3
    (22, Token VAR) -> Reduce 2 3
    (22, Token LPAREN) -> Reduce 2 3
    (22, Token HAT) -> Reduce 2 3
    (23, Token VAR) -> Reduce 2 3
    (23, Token LPAREN) -> Reduce 2 3
    (23, Token RPAREN) -> Reduce 2 3
    (23, Token HAT) -> Reduce 2 3

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
          parse' ((n, StackValueToken) : stack) (tail tokens)
        Reduce n m ->
          let (pop, stack') = splitAt n stack in
          let q =
                case stack' of
                  [] -> dfaGotoTransition 0 m
                  ((q', _) : _) -> dfaGotoTransition q' m in do
            value <-
              case m of
                0 ->
                  Monad.liftM StackValue_atom $ atom_implies_VAR actions
                1 ->
                  Monad.liftM StackValue_atom $ atom_implies_LPAREN_abstraction_RPAREN actions (case snd (pop !! 1) of { StackValue_abstraction value -> value; _ -> undefined })
                2 ->
                  Monad.liftM StackValue_application $ application_implies_atom actions (case snd (pop !! 0) of { StackValue_atom value -> value; _ -> undefined })
                3 ->
                  Monad.liftM StackValue_application $ application_implies_application_atom actions (case snd (pop !! 1) of { StackValue_application value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atom value -> value; _ -> undefined })
                4 ->
                  Monad.liftM StackValue_abstraction $ abstraction_implies_application actions (case snd (pop !! 0) of { StackValue_application value -> value; _ -> undefined })
                5 ->
                  Monad.liftM StackValue_abstraction $ abstraction_implies_HAT_VAR_DOT_abstraction actions (case snd (pop !! 0) of { StackValue_abstraction value -> value; _ -> undefined })
            parse' ((q, value) : stack') tokens
        Accept ->
          case stack of { [(_, StackValue_abstraction value)] -> return $ Just (value, tokens); _ -> return Nothing }

semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { atom_implies_VAR = return "x" -- TODO: semantic value of token
  , atom_implies_LPAREN_abstraction_RPAREN = return
  , application_implies_atom = return
  , application_implies_application_atom = \m n -> return $ "(" ++ m ++ n ++ ")"
  , abstraction_implies_application = return
  , abstraction_implies_HAT_VAR_DOT_abstraction = \m -> return $ "(^x." ++ m ++ ")" }

parseTokens :: [Token] -> Maybe (Abstraction, [Token])
parseTokens = Identity.runIdentity . parse semanticActions

-- test
main :: IO ()
main =
  print $ parseTokens [LPAREN, HAT, VAR, DOT, VAR, VAR, RPAREN, LPAREN, HAT, VAR, DOT, VAR, VAR, RPAREN]
  -- Just ("((^x.(xx))(^x.(xx)))",[])

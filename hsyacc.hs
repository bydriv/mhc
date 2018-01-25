import Language.HsYacc

-- Test grammar.
-- TODO: parse *.hsy files.
grm :: Grammar String String
grm =
  [ ("atom", [T "VAR"])
  , ("atom", [T "LPAREN", N "abstraction", T "RPAREN"])
  , ("application", [N "atom"])
  , ("application", [N "application", N "atom"])
  , ("abstraction", [N "application"])
  , ("abstraction", [T "HAT", T "VAR", T "DOT", N "abstraction"]) ]

main :: IO ()
main =
  case generateParser "TestParsing" "abstraction" grm of
    Nothing ->
      putStrLn "shift/reduce or reduce/reduce conflict."
    Just s ->
      putStrLn s

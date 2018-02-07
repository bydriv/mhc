module Language.Haskell2010.Layout where

import qualified Control.Monad.State          as State
import qualified Data.Char                    as Char
import qualified Language.Haskell2010.Lexing  as Lexing
import qualified Language.Haskell2010.Parsing as Parsing

data Token =
    Zero
  | Brace Int
  | Chevron Int
  | Token Parsing.Token
  deriving (Eq, Ord, Show, Read)

calcWidth :: String -> Int
calcWidth [] = 0
calcWidth ('\t' : s) = calcWidth s + 8
calcWidth (_ : s) = calcWidth s + 1

preprocess :: [Either (Parsing.Pos, String) Parsing.Token] -> State.State (Int, Bool) [Token]
preprocess [] = return []
preprocess (Left (_, "\n") : tokens) = do { State.put (0, True); preprocess tokens }
preprocess (Left (_, "\r") : tokens) = do { State.put (0, True); preprocess tokens }
preprocess (Left (_, "\n\r") : tokens) = do { State.put (0, True); preprocess tokens }
preprocess (Left (_, sp) : tokens) = do { State.modify (\(i, first) -> (i + calcWidth sp, first)); preprocess tokens }
preprocess (Right token : tokens) =
  case token of
    Parsing.MODULE _ -> do
      let (_, n) = Parsing.posOf token
      State.modify (\(i, first) -> (i + n, False))
      tokens' <- preprocess' tokens
      return $ Token token : tokens'
    Parsing.LBRACE _ -> do
      let (_, n) = Parsing.posOf token
      State.modify (\(i, first) -> (i + n, False))
      tokens' <- preprocess' tokens
      return $ Token token : tokens'
    _ ->
      preprocessMod $ Right token : tokens

preprocessMod :: [Either (Parsing.Pos, String) Parsing.Token] -> State.State (Int, Bool) [Token]
preprocessMod [] = return []
preprocessMod (Left (_, "\n") : tokens) = do { State.put (0, True); preprocessMod tokens }
preprocessMod (Left (_, "\r") : tokens) = do { State.put (0, True); preprocessMod tokens }
preprocessMod (Left (_, "\n\r") : tokens) = do { State.put (0, True); preprocessMod tokens }
preprocessMod (Left (_, sp) : tokens) = do { State.modify (\(i, first) -> (i + calcWidth sp, first)); preprocessMod tokens }
preprocessMod (Right token : tokens) = do
  n <- State.gets fst
  let (_, m) = Parsing.posOf token
  State.modify (\(i, first) -> (i + m, False))
  tokens' <- preprocess' tokens
  return $ Brace n : Token token : tokens'

preprocess' :: [Either (Parsing.Pos, String) Parsing.Token] -> State.State (Int, Bool) [Token]
preprocess' [] = return []
preprocess' (Left (_, "\n") : tokens) = do { State.put (0, True); preprocess' tokens }
preprocess' (Left (_, "\r") : tokens) = do { State.put (0, True); preprocess' tokens }
preprocess' (Left (_, "\n\r") : tokens) = do { State.put (0, True); preprocess' tokens }
preprocess' (Left (_, sp) : tokens) = do { State.modify (\(i, first) -> (i + calcWidth sp, first)); preprocess' tokens }
preprocess' (Right token : tokens) = do
  isFirst <- State.gets snd
  m <- State.gets fst

  let (_, n) = Parsing.posOf token
  State.modify (\(i, first) -> (i + n, False))

  case token of
    Parsing.LET _ ->
      if isFirst then do
        tokens' <- preprocessKw token tokens
        return $ Chevron m : tokens'
      else
        preprocessKw token tokens
    Parsing.WHERE _ ->
      if isFirst then do
        tokens' <- preprocessKw token tokens
        return $ Chevron m : tokens'
      else
        preprocessKw token tokens
    Parsing.DO _ ->
      if isFirst then do
        tokens' <- preprocessKw token tokens
        return $ Chevron m : tokens'
      else
        preprocessKw token tokens
    Parsing.OF _ ->
      if isFirst then do
        tokens' <- preprocessKw token tokens
        return $ Chevron m : tokens'
      else
        preprocessKw token tokens
    _ ->
      if isFirst then do
        tokens' <- preprocess' tokens
        return $ Chevron m : Token token : tokens'
      else do
        tokens' <- preprocess' tokens
        return $ Token token : tokens'

preprocessKw :: Parsing.Token -> [Either (Parsing.Pos, String) Parsing.Token] -> State.State (Int, Bool) [Token]
preprocessKw kw [] = return [Token kw, Zero]
preprocessKw kw (Left (_, "\n") : tokens) = do { State.put (0, True); preprocessKw kw tokens }
preprocessKw kw (Left (_, "\r") : tokens) = do { State.put (0, True); preprocessKw kw tokens }
preprocessKw kw (Left (_, "\n\r") : tokens) = do { State.put (0, True); preprocessKw kw tokens }
preprocessKw kw (Left (_, sp) : tokens) = do { State.modify (\(i, first) -> (i + calcWidth sp, first)); preprocessKw kw tokens }
preprocessKw kw (Right token : tokens) =
  case token of
    Parsing.LBRACE _ -> do
      tokens' <- preprocess' $ Right token : tokens
      return $ Token kw : tokens'
    _ -> do
      n <- State.gets fst
      tokens' <- preprocess' $ Right token : tokens
      return $ Token kw : Brace n : tokens'

layout :: [Token] -> [Int] -> Parsing.Pos -> [Parsing.Token]
layout (Chevron n : ts) (m : ms) pos
  | m == n = Parsing.SEMICOLON pos : layout ts (m : ms) pos
  | n < m = Parsing.RBRACE pos : layout (Chevron n : ts) ms pos
layout (Chevron n : ts) ms pos =
  layout ts ms pos
layout (Brace n : ts) (m : ms) pos
  | n > m = Parsing.LBRACE pos : layout ts (n : m : ms) pos
layout (Brace n : ts) [] pos
  | n > -1 = Parsing.LBRACE pos : layout ts [n] pos
layout (Brace n : ts) ms pos =
  Parsing.LBRACE pos : Parsing.RBRACE pos : layout (Chevron n : ts) ms pos
layout (Token token@(Parsing.RBRACE _) : ts) (-1 : ms) _ =
  let pos = Parsing.posOf token in
    Parsing.RBRACE pos : layout ts ms pos
layout (Token token@(Parsing.RBRACE _) : ts) ms _ =
  undefined
layout (Token token@(Parsing.LBRACE _) : ts) ms _ =
  let pos = Parsing.posOf token in
    Parsing.LBRACE pos : layout ts (-1 : ms) pos
layout (Token token : ts) (m : ms) pos
  | m /= -1 && parseError token = Parsing.RBRACE pos : token : layout ts ms pos
  where
    parseError (Parsing.IN _) = True
    parseError _ = False
layout (Token token : ts) ms _ =
  token : (layout ts ms (Parsing.posOf token))
layout [] [] _ =
  []
layout [] (_ : ms) pos =
  Parsing.RBRACE pos : layout [] ms pos

test :: String -> [Parsing.Token]
test s =
  let ((tokens0, s'), (pos, _)) = flip State.runState (0, 0) $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s in
  let tokens = layout (State.evalState (preprocess tokens0) (0, True)) [] (0, 0) in
    tokens

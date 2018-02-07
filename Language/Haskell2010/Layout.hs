module Language.Haskell2010.Layout where

import qualified Control.Monad.State          as State
import qualified Data.Char                    as Char
import qualified Language.Haskell2010.Lexing  as Lexing
import qualified Language.Haskell2010.Parsing as Parsing

data LayoutContext =
    Unknown
  | Module
  | Let
  | Where
  | Do
  | Of
  deriving (Eq, Ord, Show, Read)

data Token =
    Zero
  | Brace LayoutContext Int
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
  tokens' <- preprocess' $ Right token : tokens
  return $ Brace Module n : tokens'

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
        tokens' <- preprocessKw Let token tokens
        return $ Chevron m : tokens'
      else
        preprocessKw Let token tokens
    Parsing.WHERE _ ->
      if isFirst then do
        tokens' <- preprocessKw Where token tokens
        return $ Chevron m : tokens'
      else
        preprocessKw Where token tokens
    Parsing.DO _ ->
      if isFirst then do
        tokens' <- preprocessKw Do token tokens
        return $ Chevron m : tokens'
      else
        preprocessKw Do token tokens
    Parsing.OF _ ->
      if isFirst then do
        tokens' <- preprocessKw Of token tokens
        return $ Chevron m : tokens'
      else
        preprocessKw Of token tokens
    _ ->
      if isFirst && notIn token then do
        tokens' <- preprocess' tokens
        return $ Chevron m : Token token : tokens'
      else do
        tokens' <- preprocess' tokens
        return $ Token token : tokens'
      where
        notIn (Parsing.IN _) = False
        notIn _ = True

preprocessKw :: LayoutContext -> Parsing.Token -> [Either (Parsing.Pos, String) Parsing.Token] -> State.State (Int, Bool) [Token]
preprocessKw cxt kw [] = return [Token kw, Zero]
preprocessKw cxt kw (Left (_, "\n") : tokens) = do { State.put (0, True); preprocessKw cxt kw tokens }
preprocessKw cxt kw (Left (_, "\r") : tokens) = do { State.put (0, True); preprocessKw cxt kw tokens }
preprocessKw cxt kw (Left (_, "\n\r") : tokens) = do { State.put (0, True); preprocessKw cxt kw tokens }
preprocessKw cxt kw (Left (_, sp) : tokens) = do { State.modify (\(i, first) -> (i + calcWidth sp, first)); preprocessKw cxt kw tokens }
preprocessKw cxt kw (Right token : tokens) =
  case token of
    Parsing.LBRACE _ -> do
      tokens' <- preprocess' $ Right token : tokens
      return $ Token kw : tokens'
    _ -> do
      n <- State.gets fst
      tokens' <- preprocess' $ Right token : tokens
      return $ Token kw : Brace cxt n : tokens'

layout :: [Token] -> Int -> [(LayoutContext, Int, Int)] -> Parsing.Pos -> [Parsing.Token]
layout (Chevron n : ts) i ((mcxt, m, j) : ms) pos
  | m == n = Parsing.SEMICOLON pos : layout ts i ((mcxt, m, j) : ms) pos
  | n < m = Parsing.RBRACE pos : layout (Chevron n : ts) i ms pos
layout (Chevron n : ts) i ms pos =
  layout ts i ms pos
layout (Brace ncxt n : ts) i ((mcxt, m, j) : ms) pos
  | n > m = Parsing.LBRACE pos : layout ts i ((ncxt, n, i) : (mcxt, m, j) : ms) pos
layout (Brace ncxt n : ts) i [] pos
  | n > -1 = Parsing.LBRACE pos : layout ts i [(ncxt, n, i)] pos
layout (Brace _ n : ts) i ms pos =
  Parsing.LBRACE pos : Parsing.RBRACE pos : layout (Chevron n : ts) i ms pos
layout (Token token@(Parsing.RBRACE _) : ts) i ((_, -1, _) : ms) _ =
  let pos = Parsing.posOf token in
    Parsing.RBRACE pos : layout ts (i - 1) ms pos
layout (Token token@(Parsing.RBRACE _) : ts) i ms _ =
  undefined
layout (Token token@(Parsing.LBRACE _) : ts) i ms _ =
  let pos = Parsing.posOf token in
    Parsing.LBRACE pos : layout ts (i + 1) ((Unknown, -1, i + 1) : ms) pos
layout (Token token : ts) i ((mcxt, m, j) : ms) pos
  | m /= -1 && isIn token =
      let n = countDropLet ((mcxt, m, j) : ms) in
      let ms' = drop n ((mcxt, m, j) : ms) in
        replicate n (Parsing.RBRACE pos) ++ (token : layout ts i ms' pos)
  | m /= -1 && isWhere token =
      let n = countDropWhere ((mcxt, m, j) : ms) in
      let ms' = drop n ((mcxt, m, j) : ms) in
        replicate n (Parsing.RBRACE pos) ++ (token : layout ts i ms' pos)
  | m /= -1 && isClose token =
      let n = countDropClose ((mcxt, m, j) : ms) in
      let ms' = drop n ((mcxt, m, j) : ms) in
        replicate n (Parsing.RBRACE pos) ++ (token : layout ts (i-1) ms' pos)
  where
    isIn (Parsing.IN _) = True
    isIn _ = False

    countDropLet [] = 0
    countDropLet ((Unknown, _, _) : _) = 0
    countDropLet ((Let, _, _) : _) = 1
    countDropLet (_ : stack) = countDropLet stack + 1

    isWhere (Parsing.WHERE _) = True
    isWhere _ = False

    countDropWhere [] = 0
    countDropWhere ((Unknown, _, _) : _) = 0
    countDropWhere ((Module, _, _) : _) = 0
    countDropWhere ((Where, _, _) : _) = 0
    countDropWhere (_ : stack) = countDropWhere stack + 1

    isClose (Parsing.RPAREN _) = True
    isClose (Parsing.RBRACKET _) = True
    isClose (Parsing.OF _) = True
    isClose _ = False

    countDropClose [] = 0
    countDropClose ((Unknown, _, _) : stack) = 0
    countDropClose ((_, _, j) : stack)
      | i > j = 0
      | otherwise = countDropClose stack + 1
layout (Token token : ts) i ms _
  | isOpen token = token : (layout ts (i+1) ms (Parsing.posOf token))
  | otherwise = token : (layout ts i ms (Parsing.posOf token))
  where
    isOpen (Parsing.LPAREN _) = True
    isOpen (Parsing.LBRACKET _) = True
    isOpen (Parsing.CASE _) = True
    isOpen _ = False
layout [] _ [] _ =
  []
layout [] i (_ : ms) pos =
  Parsing.RBRACE pos : layout [] i ms pos

test :: String -> [Parsing.Token]
test s =
  let ((tokens0, s'), (pos, _)) = flip State.runState (0, 0) $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s in
  let tokens = layout (State.evalState (preprocess tokens0) (0, True)) 0 [] (0, 0) in
    tokens

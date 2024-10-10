module Lexer where

import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Data.List.NonEmpty               (cons)
import qualified Data.Text                        as T
import           Types

type Lexer a = State Input a

data LexerBranch = Num | Str | KeyVar | Special | Err | Eof | WS

nextToken :: Lexer SyntaxToken
nextToken = do
  p1 <- gets pos
  s <- gets src
  c <- current
  case getBranch c of
    Eof -> return $ SyntaxToken EofToken p1 "enf of file"
    Err -> next >> (return $ SyntaxToken ErrorToken p1 $ "Unknown character: '" <> [c] <> "'")
    Num -> consume isDigit >> gets pos >>= \p2 -> return $ SyntaxToken NumberToken p1 (T.unpack . T.take (p2 - p1) . T.drop (p1 - 1) $ s)
    WS -> consume (flip elem " \t") >> (return $ SyntaxToken WhiteSpace p1 " ")
    Special -> case c of
      '+' -> next >> (return $ SyntaxToken PlusToken p1 "+")
      '-' -> next >> (return $ SyntaxToken MinusToken p1 "-")
      '*' -> next >> (return $ SyntaxToken MultToken p1 "*")
      '/' -> next >> (return $ SyntaxToken DivisionToken p1 "/")
      '(' -> next >> (return $ SyntaxToken OpenParenToken p1 "(")
      ')' -> next >> (return $ SyntaxToken CloseParenToken p1 ")")
      _   -> undefined -- never going to happen
    _ -> next >> (return $ SyntaxToken ErrorToken p1 $ "Unknown character: '" <> [c] <> "'")

current :: Lexer Char
current = do
  s <- gets src
  p <- gets pos
  if p > T.length s
    then return '\NUL'
    else return (T.index s (p - 1))

next :: Lexer ()
next = modify (\i -> i {pos = 1 + pos i})

consume :: (Char -> Bool) -> Lexer ()
consume p = do
  c <- current
  if p c
    then next >> consume p
    else return ()

getBranch :: Char -> LexerBranch
getBranch c
  | isDigit c = Num
  | c == '"' = Str
  | isAlpha c = KeyVar
  | elem c "+-/*()" = Special
  | elem c " \t" = WS
  | c == '\NUL' = Eof
  | otherwise = Err

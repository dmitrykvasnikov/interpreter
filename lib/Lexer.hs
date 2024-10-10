module Lexer where

import           Control.Monad.Trans.RWS.Strict
-- import           Control.Monad.Trans.State.Strict
import           Data.Char
import qualified Data.Text                      as T
import           Types

type Lexer a = RWS Source Diagnostics Program a

data LexerBranch = Num | Special | Err | Eof | WS

nextToken :: Lexer SyntaxToken
nextToken = do
  p1 <- gets lexerP
  c <- current
  case getBranch c of
    Eof -> return $ SyntaxToken EofToken p1 "enf of file" Null
    Err -> next >> tell [Message Error ("Lexer error, position " <> show p1 <> ", unknown character " <> [c])] >> (return $ SyntaxToken ErrorToken p1 [c] Null)
    Num -> consume p1 isDigit >>= \num -> return $ SyntaxToken NumberToken p1 num (NumberValue $ read num)
    WS -> consume p1 (flip elem " \t") >> (return $ SyntaxToken WhiteSpace p1 " " Null)
    Special -> case c of
      '+' -> next >> (return $ SyntaxToken PlusToken p1 "+" Null)
      '-' -> next >> (return $ SyntaxToken MinusToken p1 "-" Null)
      '*' -> next >> (return $ SyntaxToken MultToken p1 "*" Null)
      '/' -> next >> (return $ SyntaxToken DivisionToken p1 "/" Null)
      '(' -> next >> (return $ SyntaxToken OpenParenToken p1 "(" Null)
      ')' -> next >> (return $ SyntaxToken CloseParenToken p1 ")" Null)
      _   -> undefined -- never going to happe Nulln

current :: Lexer Char
current = do
  s <- ask
  p <- gets lexerP
  if p > T.length s
    then return '\NUL'
    else return (T.index s (p - 1))

next :: Lexer ()
next = modify (\p -> p {lexerP = 1 + lexerP p})

consume :: Position -> (Char -> Bool) -> Lexer String
consume p1 pr = do
  c <- current
  case pr c of
    True -> next >> consume p1 pr
    False -> gets lexerP >>= \p2 -> ask >>= \s -> return (T.unpack . T.take (p2 - p1) . T.drop (p1 - 1) $ s)

getBranch :: Char -> LexerBranch
getBranch c
  | isDigit c = Num
  | elem c "+-/*()" = Special
  | elem c " \t" = WS
  | c == '\NUL' = Eof
  | otherwise = Err

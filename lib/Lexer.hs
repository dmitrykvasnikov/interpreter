module Lexer where

-- import           Control.Monad.Trans.RWS.Strict
import           Control.Monad.Trans.State.Strict
import           Data.Char
import qualified Data.Text                        as T
import           Types

type Lexer a = State Input a

data LexerBranch = Num | Str | KeyVar | Special | Err | Eof | WS

nextToken :: Lexer SyntaxToken
nextToken = do
  p1 <- gets pos
  c <- current
  case getBranch c of
    Eof -> return $ SyntaxToken EofToken p1 "enf of file" Null
    Err -> next >> (return $ SyntaxToken ErrorToken p1 ("Unknown character: '" <> [c] <> "'") Null)
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
    _ -> next >> (return $ SyntaxToken ErrorToken p1 ("Unknown character: '" <> [c] <> "'") Null)

current :: Lexer Char
current = do
  s <- gets src
  p <- gets pos
  if p > T.length s
    then return '\NUL'
    else return (T.index s (p - 1))

next :: Lexer ()
next = modify (\i -> i {pos = 1 + pos i})

consume :: Position -> (Char -> Bool) -> Lexer String
consume p1 pr = do
  c <- current
  case pr c of
    True -> next >> consume p1 pr
    False -> gets pos >>= \p2 -> gets src >>= \s -> return (T.unpack . T.take (p2 - p1) . T.drop (p1 - 1) $ s)

getBranch :: Char -> LexerBranch
getBranch c
  | isDigit c = Num
  | c == '"' = Str
  | isAlpha c = KeyVar
  | elem c "+-/*()" = Special
  | elem c " \t" = WS
  | c == '\NUL' = Eof
  | otherwise = Err

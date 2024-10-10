module Parser where

import           Control.Monad                  (when)
import           Control.Monad.Trans.RWS.Strict
import qualified Data.Vector                    as V
import           Debug.Trace
import qualified Lexer                          as L
import           Types

type Parser a = RWST Source Diagnostics Program IO a

peek :: Int -> Parser SyntaxToken
peek offset = do
  p <- gets parserPos
  ts <- gets tokens
  case p >= V.length ts of
    True  -> return $ V.last ts
    False -> return $ ts V.! (p + offset - 1)

current :: Parser SyntaxToken
current = do
  c <- peek 0
  case stkind c of
    WhiteSpace -> next >> current
    _          -> return c

nextToken :: Parser SyntaxToken
nextToken = do
  g <- gets tokens
  when (V.null g) (error "ERROR: lexer must be run before parser, use tokenize first")
  c <- current
  next
  return c

next :: Parser ()
next = modify (\pr -> pr {parserPos = 1 + parserPos pr})

match :: SyntaxKind -> Parser SyntaxToken
match sk = do
  c <- current
  case stkind c == sk of
    True -> next >> return c
    False -> tell [Message Error $ "Parser error, position " <> (show . stpos $ c) <> ", expected kind " <> show sk <> ", got " <> (show . stkind $ c)] >> (return $ SyntaxToken sk (stpos c) "" Null)

parse :: Parser Expression
parse = do
  left <- parsePrimaryExpression
  c <- current
  if elem (stkind c) binaryOperations
    then do
      operand <- nextToken
      parse >>= return . BinaryExpression operand left
    else return left

parsePrimaryExpression :: Parser Expression
parsePrimaryExpression = match NumberToken >>= return . NumberExpression

binaryOperations :: [SyntaxKind]
binaryOperations = [MinusToken, PlusToken, DivisionToken, MultToken]

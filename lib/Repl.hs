module Repl where

import           Control.Monad                  (when)
import           Control.Monad.Trans.RWS.Strict
import qualified Data.Text                      as T (pack)
import           Lexer
import           Types

initProgram :: Program
initProgram = Program 1 [] 1

repl :: IO ()
repl = do
  putStr ">>> "
  i <- T.pack <$> getLine
  runProgram i
  repl

runProgram :: Source -> IO ()
runProgram i = do
  let (tokens, _, msgs) = runRWS listOfTokens i initProgram
  mapM_ (putStrLn . show) tokens
  when (not . null $ msgs) (putStrLn "Diagnostics :" >> mapM_ (putStrLn . show) msgs)

listOfTokens :: Lexer [SyntaxToken]
listOfTokens = do
  t <- nextToken
  if (stkind t) == EofToken
    then return [t]
    else (t :) <$> listOfTokens

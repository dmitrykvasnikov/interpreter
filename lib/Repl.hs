module Repl where

import           Control.Monad.Trans.State.Strict
import qualified Data.Text                        as T (pack)
import           Lexer
import           Types

makeInput :: String -> Input
makeInput str = Input 1 (T.pack str)

repl :: IO ()
repl = do
  putStr ">>> "
  i <- makeInput <$> getLine
  printTokens i
  repl

printTokens :: Input -> IO ()
printTokens i = do
  let tokens = evalState listOfTokens i
  mapM_ (putStrLn . show) tokens

listOfTokens :: Lexer [SyntaxToken]
listOfTokens = do
  t <- nextToken
  if (stkind t) == EofToken
    then return [t]
    else (t :) <$> listOfTokens

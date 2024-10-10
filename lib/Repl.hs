module Repl where

import           Control.Monad                  (when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.RWS.Strict
import qualified Data.Text                      as T (pack)
import qualified Data.Vector                    as V
import           Lexer                          (Lexer)
import qualified Lexer                          as L
import           Parser                         (Parser)
import qualified Parser                         as P
import           Print
import           Types

initProgram :: Program
initProgram = Program 1 V.empty 1

repl :: IO ()
repl = do
  putStr ">>> "
  i <- T.pack <$> getLine
  runProgram i
  repl

repl1 :: RWST Source Diagnostics Program IO Expression
repl1 = do
  put initProgram
  i <- liftIO getLine
  return undefined

runProgram :: Source -> IO ()
runProgram i = do
  --   let (tokens, _, msgs) = runRWS listOfTokens i initProgram
  --   mapM_ (putStrLn . show) tokens
  let (expr, _, msgs) = runRWS expression i initProgram
  putStrLn . show $ expr
  prettyPrint "" [expr]
  when (not . null $ msgs) (putStrLn "Diagnostics :" >> mapM_ (putStrLn . show) msgs)

listOfTokens :: Lexer [SyntaxToken]
listOfTokens = L.tokenize >> V.toList <$> gets tokens

expression :: Parser Expression
expression = L.tokenize >> P.parse

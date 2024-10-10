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
import           System.Console.Pretty
import           Types

initProgram :: Program
initProgram = Program 1 V.empty 1

repl :: IO ()
repl = fst <$> evalRWST repl1 (T.pack "") initProgram

repl1 :: RWST Source Diagnostics Program IO ()
repl1 = do
  put initProgram
  liftIO $ putStr ">>> "
  liftIO getLine >>= \case
    ":exit" -> return ()
    i -> do
      (e, m) <- listen . local (const (T.pack i)) $ (L.tokenize >> P.parse)
      liftIO . putStrLn . show $ e
      liftIO . prettyPrint "" $ [e]
      when (not . null $ m) ((liftIO . putStrLn $ color Cyan "Diagnostics:") >> (mapM_ (liftIO . putStrLn . show) m))
      repl1

-- listOfTokens :: Lexer [SyntaxToken]
-- listOfTokens = L.tokenize >> V.toList <$> gets tokens

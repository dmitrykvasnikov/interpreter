module Repl where

import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Lexer.Lexer
import           Types.Common                     (Error (Error))
import           Types.Repl

makeSource :: String -> Source
makeSource src =
  Source
    { src = T.pack src,
      srcLen = length src,
      lPos = 0,
      sPos = (1, 1),
      pPos = 1,
      tokens = V.empty
    }

repl :: IO ()
repl = do
  liftIO $ putStr ">>> "
  l <- liftIO getLine
  res <- (runStateT . runExceptT) tokenize $ (makeSource l)
  case res of
    (Right _, s) -> do
      putStrLn "TOKENS:"
      mapM_ (putStrLn . show) (tokens s)
    (Left err, _) -> putStrLn $ show err
  repl

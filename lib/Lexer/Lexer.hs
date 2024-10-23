module Lexer.Lexer where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Text.Regex.TDFA.Text
import           Types.Lexer
import           Types.Repl
import           Utils

type Lexer = Program ()

advanceN :: Int -> Program ()
advanceN n = lgs lPos >>= \lp -> lgs sPos >>= \(l, c) -> lm (\s -> s {lPos = lp + n, sPos = (l, c + n)})

current :: Program Char
current = lgs src >>= \s -> lgs lPos >>= return . (T.index s)

remainder :: Program Text
remainder = lgs lPos >>= \lp -> lgs src >>= return . (T.drop lp)

lexer :: IO ()
lexer = putStrLn "this is lexer"

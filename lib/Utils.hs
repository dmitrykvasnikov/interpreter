module Utils where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Types.Repl

-- helpers for State modad
lgs :: (Source -> a) -> Program a
lgs = lift . gets

lm :: (Source -> Source) -> Program ()
lm = lift . modify

lg :: Program Source
lg = lift get

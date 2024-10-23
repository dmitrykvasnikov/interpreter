module Types.Repl where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Text                        (Text)
import qualified Data.Vector                      as V
import           Types.Common
import           Types.Lexer

type Program a = ExceptT Error (StateT Source IO) a

data Source = Source { src    :: Text
                     , srcLen :: Int
                     , lPos   :: Int
                       -- sPos - source Position in as (Line, Col)
                     , sPos   :: Pos
                     , pPos   :: Int
                     , tokens :: V.Vector Token
                     }

module Types.Common where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Text                        (Text)

type Line = Int

type Col = Int

type Pos = (Line, Col)

type ErrorMessage = String

data ErrorStage = LErr | PErr | TErr | EErr

instance Show ErrorStage where
  show LErr = "[Lexer Error]"
  show PErr = "[Parser Error]"
  show TErr = "[Type Error]"
  show EErr = "[Evaluation Error]"

data Error = Error ErrorStage ErrorMessage

instance Show Error where
  show (Error est emsg) = show est <> " " <> emsg

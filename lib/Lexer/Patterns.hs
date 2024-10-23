module Lexer.Patterns where

import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text
import           Types.Lexer
import           Types.Repl

type Pattern = String

type Patterms = [Pattern]

type PatternHandler = Pattern -> Program ()

type PatternMap = [(Pattern, PatternHandler)]

patterns :: PatternMap
patterns =
  [ ("+", defaultHandler PlusToken "+"),
    ("-", defaultHandler MinusToken "-")
  ]

defaultHandler :: TokenKind -> String -> Pattern -> Program ()
defaultHandler tokenKind value = undefined

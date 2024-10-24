module Lexer.Lexer where

import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Vector     as V
import           Lexer.Patterns
import           Text.Regex.TDFA
import           Types.Lexer
import           Types.Repl
import           Utils

type Lexer = Program ()

advanceN :: Int -> Program ()
advanceN n = lgs lPos >>= \lp -> lgs sPos >>= \(l, c) -> lm (\s -> s {lPos = lp + n, sPos = (l, c + n)})

newLine :: Program ()
newLine = lgs sPos >>= \(l, c) -> lm (\s -> s {sPos = (l + 1, 1)})

current :: Program Char
current =
  lgs srcLen >>= \l ->
    lgs src >>= \s ->
      lgs lPos >>= \p ->
        case p >= l of
          True  -> return '\0'
          False -> return $ T.index s p

remainder :: Program Text
remainder = lgs lPos >>= \lp -> lgs src >>= return . (T.drop lp)

push :: Token -> Program ()
push token = lm (\s -> s {tokens = V.snoc (tokens s) token})

tokenize :: Program ()
tokenize =
  current >>= \case
    '\0' -> (lgs sPos) >>= \p -> push $ Token EOFToken p (SyntaxValue "EOF")
    _ -> do
      r <- remainder
      let (s, h) = matchPattern r
      h s >>= \t ->
        push t >> case (tKind t) of
          NewLineToken -> advanceN (1) >> newLine >> tokenize
          _            -> advanceN (length s) >> tokenize

matchPattern :: Text -> (String, PatternHandler)
matchPattern rem = go rem patterns
  where
    e :: Text
    e = T.pack ""
    go _ [] = error "PANIC: error handler in parser didn't catch unknown symbol. This mistake should never happen by design"
    go t ((p, h) : ps) =
      let (b, r, _) = (t =~ p) :: (Text, Text, Text)
       in if r /= e && b == e
            then (T.unpack r, h)
            else go t ps

module Lexer.Patterns where

import           Control.Monad.Trans.Except (throwE)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           System.Console.Pretty
import           Text.Read.Lex              (Lexeme (String))
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text
import           Types.Common
import           Types.Lexer
import           Types.Repl
import           Utils

type Pattern = String

type PatternHandler = String -> Program Token

type Patterns = [(Pattern, PatternHandler)]

keywords :: [(String, TokenKind)]
keywords =
  [ ("if", IfToken),
    ("then", ThenToken),
    ("else", ElseToken),
    ("return", ReturnToken),
    ("let", LetToken),
    ("func", FunctionToken)
  ]

patterns :: Patterns
patterns =
  [ ("[0-9]+", intLiteralHandler),
    ("true", boolLiteralHandler "true"),
    ("false", boolLiteralHandler "false"),
    ("\"[^(\"|\n)]*\"", stringLiteralHandler),
    ("[a-zA-Z][a-zA-Z0-9_']*", keywordHandler),
    ("\\+\\+", defaultHandler PlusPlusToken "++"),
    ("--", defaultHandler MinusMinusToken "--"),
    ("==", defaultHandler EqualToken "=="),
    ("!=", defaultHandler NotEqualToken "!="),
    (">=", defaultHandler GREToken ">="),
    ("=>", defaultHandler GREToken ">="),
    ("<=", defaultHandler LSEToken "<="),
    ("=<", defaultHandler LSEToken "<="),
    ("&&", defaultHandler AndToken "AND"),
    ("\\|\\|", defaultHandler OrToken "OR"),
    ("<", defaultHandler LSToken "<"),
    (">", defaultHandler GRToken ">"),
    ("!", defaultHandler ExclamationToken "NOT"),
    ("\\+", defaultHandler PlusToken "+"),
    ("-", defaultHandler MinusToken "-"),
    ("\\*", defaultHandler StarToken "*"),
    ("/", defaultHandler SlashToken "/"),
    ("=", defaultHandler AssignToken "="),
    ("\\(", defaultHandler LParenToken "("),
    ("\\)", defaultHandler RParenToken ")"),
    (" *", defaultHandler WhiteSpaceToken " "),
    ("\n", defaultHandler NewLineToken "<CR>"),
    ("\\[", defaultHandler LBraceToken "["),
    ("\\]", defaultHandler RBraceToken "]"),
    ("}", defaultHandler RCurlyToken "}"),
    ("{", defaultHandler LCurlyToken "{"),
    (":", defaultHandler ColonToken ":"),
    (";", defaultHandler SemicolonToken ";"),
    (",", defaultHandler CommaToken ","),
    ("\\?", defaultHandler QuestionToken "?"),
    -- error handler, works always, return an error
    (".*", errorHandler)
  ]

defaultHandler :: TokenKind -> String -> PatternHandler
defaultHandler tk v = \_ -> do
  p <- lgs sPos
  ts <- lgs tokens
  return $ Token tk p (SyntaxValue v)

boolLiteralHandler :: String -> PatternHandler
boolLiteralHandler s = \_ -> do
  p <- lgs sPos
  return $ Token BoolLiteralToken p (BoolVal (s == "true"))

errorHandler, stringLiteralHandler, intLiteralHandler, keywordHandler :: PatternHandler
intLiteralHandler s = do
  p <- lgs sPos
  return $ Token IntLiteralToken p (IntVal $ read s)
stringLiteralHandler s = do
  p <- lgs sPos
  return $ Token StringLiteralToken p (StringVal (take (length s - 2) (tail s)))
keywordHandler s = do
  p <- lgs sPos
  case lookup s keywords of
    (Just kind) -> return $ Token kind p (SyntaxValue s)
    Nothing     -> return $ Token IdentifierToken p (SyntaxValue s)
errorHandler s = do
  (l, c) <- lgs sPos
  errLine <- head . (drop (l - 1)) . lines . T.unpack <$> (lgs src)
  throwE $
    Error LErr $
      "Unknown character at line "
        <> show l
        <> " column "
        <> show c
        <> " '"
        <> [head s]
        <> "'\n[Source] "
        <> (take (c - 1) errLine)
        <> (color Red (take 1 $ drop (c - 1) errLine))
        <> (drop c errLine)

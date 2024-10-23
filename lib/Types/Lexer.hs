module Types.Lexer where

import           Types.Common

data Value = BoolVal Bool
           | IntVal Int
           | DoubleVal Double
           | StringVal String
           -- SyntaxVal = for other kind of tokens : operands, keywords, identifiers ...
           | SyntaxValue String

instance Show Value where
  show (BoolVal b)     = show b
  show (IntVal i)      = show i
  show (DoubleVal d)   = show d
  show (StringVal s)   = s
  show (SyntaxValue s) = s

data TokenKind -- tokens
               = LiteralToken
               | IdentifierToken
               -- operation tokens
               | PlusToken
               | MinusToken
               | StarToken
               | SlashToken
               | PlusPlusToken
               | MinusMinusToken
               -- keywords Token
               | AssignToken
               | ReturnToken
               | IfToken
               | ThenToken
               | ElseToken
               -- separators
               | WhiteSpaceToken
               | LParenToken
               | RParenToken
               | LBraceToken
               | RBraceToken
               | ColonToken
               | SemicolonToken
               | CommaToken
               | NewLineToken
               | LCurlyToken
               | RCurlyToken
               | EOFToken
  deriving (Eq, Ord, Show)

data Token = Token { tKind :: TokenKind
                   , tPos  :: Pos
                   , tVal  :: Value
                   }

instance Show Token where
  show token = "<" ++ (show $ tKind token) ++ "> '" ++ (show $ tVal token) ++ "'"

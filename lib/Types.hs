module Types where

import           Data.Text             (Text)
import           System.Console.Pretty

data SyntaxKind = NumberToken | PlusToken | MinusToken | DivisionToken | MultToken | OpenParenToken | CloseParenToken | EofToken | ErrorToken | WhiteSpace deriving
  ( Eq
  , Show
  )

data Value = NumberValue Int
           | Null

instance Show Value where
  show (NumberValue num) = "INTEGER : " <> show num
  show Null              = "NULL"

data SyntaxToken = SyntaxToken { stkind :: SyntaxKind
                               , stpos  :: Position
                               , strep  :: String
                               , stval  :: Value
                               }

instance Show SyntaxToken where
  show st@(SyntaxToken ErrorToken _ _ _) = (color Red $ show (stkind st) <> " " <> (strep st))
  show st = show (stkind st) <> " '" <> (strep st) <> "' | value : " <> show (stval st)

data MessageKind = Warning | Error

data Message = Message { messageKind :: MessageKind
                       , message     :: String
                       }

instance Show Message where
  show msg = color (msgClr $ messageKind msg) (message msg)
    where
      msgClr Warning = Yellow
      msgClr Error   = Red

type Diagnostics = [Message]

type Position = Int

type Source = Text

data Program = Program { lexerP  :: Position
                       , tokens  :: [SyntaxToken]
                       , parserP :: Position
                       }

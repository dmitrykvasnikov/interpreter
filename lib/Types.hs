module Types where

import           Data.Text (Text)

type Position = Int

data Input = Input { pos :: Position
                   , src :: Text
                   }

data SyntaxKind = NumberToken | PlusToken | MinusToken | DivisionToken | MultToken | OpenParenToken | CloseParenToken | EofToken | ErrorToken | WhiteSpace deriving
  ( Eq
  , Show
  )

data SyntaxToken = SyntaxToken { stkind :: SyntaxKind
                               , stpos  :: Position
                               , stval  :: String
                               }

instance Show SyntaxToken where
  show st = show (stkind st) <> " '" <> (stval st) <> "'"

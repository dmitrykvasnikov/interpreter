module Print where

import           Types

type Indent = String

prettyPrint :: Indent -> [Expression] -> IO ()
prettyPrint _ [] = return ()
prettyPrint indent [e] = do
  putStrLn $ indent <> lastMarker <> output e
  prettyPrint (indent <> "    ") (getChildren e)
prettyPrint indent (e : es) = do
  putStrLn $ indent <> notLastMarker <> output e
  prettyPrint (indent <> notLastIndent) (getChildren e)
  prettyPrint indent es

--   putStr indent
--   putStrLn $ output expr
--   mapM_ (prettyPrint (indent <> "    ")) (getChildren expr)

notLastMarker, notLastIndent, lastMarker :: String
notLastMarker = "├───"
lastMarker = "└───"
notLastIndent = "│   "

output :: Expression -> String
output expr =
  let st = expst expr
      value = if stval st == Null then "" else show (stval st)
   in show (stkind st) <> " " <> value

getChildren :: Expression -> [Expression]
getChildren (NumberExpression _)     = []
getChildren (BinaryExpression o l r) = [l, r]

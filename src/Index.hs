module Index where

import Text.Parsec
import Text.Parsec.String
import Data.List
import Lib

generateIndex :: IO ()
generateIndex = do
  files <- findAllSource
  meta <- getMetaInfo files
  writeFile "dest/index.html" =<< renderIndex meta

renderIndex :: [Meta] -> IO String
renderIndex metaInfo = do
  layout <- readFile "index.html"
  case parse p "" layout of
    Left err -> error "ParseError"
    Right html -> return html
  where
    p :: Parser String
    p = do
      parts <- many (
        try (string "{{" *> spaces *> string "body" *> spaces *> string "}}" *> return ("<ul>" ++ lst ++ "</ul>")) <|>
        (anyChar >>= (\c -> return [c]))
        )
      return $ Prelude.concat parts
    lst = foldl (\x m -> x ++ "<li>" ++ formatPublishedAt m "%Y-%m-%d" ++ ": <a href=\"" ++ metaToPath m ++ "\">" ++ metaTitle m ++ "</a></li>") "" sorted
    sorted = sortWithDesc (\m -> metaPublishedAt m) metaInfo

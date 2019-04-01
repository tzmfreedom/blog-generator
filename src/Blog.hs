module Blog where

import Text.Parsec
import Text.Parsec.String
import Data.List as L
import System.Posix.Time
import CMark
import Data.Time.Clock
import Data.Text
import Lib

generate :: IO ()
generate = do
  files <- findUnprocessedSource
  meta <- getMetaInfo files
  processFiles meta
  current <- epochTime
  writeFile lastUpdatedFile $ show current

processFiles :: [Meta] -> IO [()]
processFiles metaInfo = do
  (flip mapM) metaInfo $ \m -> do
    let src = metaSrc m
    content <- readFile src
    html <- generateHTML (render content) m
    writeFile (destFile src) html

render :: String -> String
render md = unpack $ commonmarkToHtml [] $ pack md

generateHTML :: String -> Meta -> IO String
generateHTML content meta = do
  f <- readFile layoutFile
  case parse (parser content meta) "" f of
    Left err -> error "ParseError"
    Right html -> return html

parser :: String -> Meta -> Parser String
parser src meta = do
  ls <- many $ (
    try (string "{{" *> spaces *> string "body" *> spaces *> string "}}" *> return src) <|>
    try (string "{{" *> spaces *> string "meta" *> spaces *> string "}}" *> return (renderMeta meta)) <|>
    try (string "{{" *> spaces *> string "title" *> spaces *> string "}}" *> return (metaTitle meta)) <|>
    try (string "{{" *> spaces *> string "publishedAt" *> spaces *> string "}}" *> return (formatPublishedAt meta "%Y-%m-%d")) <|>
    (anyChar >>= (\c -> return [c]))
    )
  return $ Prelude.concat ls

renderMeta :: Meta -> String
renderMeta meta = do
  "<title>" ++ metaTitle meta ++ "</title>\n" ++
    "<meta name=\"keywords\" content=\"" ++ keys ++ "\"/>\n" ++
    "<meta name=\"description\" content=\"" ++ metaTitle meta ++ "\"/>"
    where
      keys :: String
      keys = L.intercalate "," $ metaKeywords meta

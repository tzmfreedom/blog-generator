module Rss where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Time.Clock
import Data.Time.Format
import Data.List as L
import Data.Char
import System.FilePath.Posix
import Lib

data ItemNode = NVar String
  | NText String
  deriving Show

generateRSS :: IO ()
generateRSS = do
  files <- findAllSource
  meta <- getMetaInfo files
  content <- renderRSS meta
  writeFile (destDirectory ++ "/feed.xml") content

renderRSS :: [Meta] -> IO String
renderRSS metaInfo = do
  siteInfo <- readSiteInfo
  layout <- readFile "rss-layout.xml"
  current <- getCurrentTime
  case parse (p siteInfo current) "" layout of
    Left err -> error "ParseError"
    Right xml -> return xml
  where
    p :: SiteInfo -> UTCTime -> Parser String
    p siteInfo current = do
      parts <- many (
        try (string "{{" *> spaces *> string "site_name" *> spaces *> string "}}" *> return (siName siteInfo)) <|>
        try (string "{{" *> spaces *> string "description" *> spaces *> string "}}" *> return (siDescription siteInfo)) <|>
        try (string "{{" *> spaces *> string "feed_update_period" *> spaces *> string "}}" *> return (siFeedUpdatePeriod siteInfo)) <|>
        try (string "{{" *> spaces *> string "feed_update_frequency" *> spaces *> string "}}" *> return (show $ siFeedUpdateFrequency siteInfo)) <|>
        try (string "{{" *> spaces *> string "site_url" *> spaces *> string "}}" *> return (siSiteUrl siteInfo)) <|>
        try (string "{{" *> spaces *> string "feed_path" *> spaces *> string "}}" *> return (siFeedPath siteInfo)) <|>
        try (string "{{" *> spaces *> string "last_build_date" *> spaces *> string "}}" *> return (formatTime defaultTimeLocale formatRFC822 current)) <|>
        try (parseItemsBlock sorted) <|>
        (anyChar >>= (\c -> return [c]))
        )
      return $ Prelude.concat parts
    sorted :: [Meta]
    sorted = sortWithDesc (\m -> metaPublishedAt m) metaInfo


parseItemsBlock :: [Meta] -> Parser String
parseItemsBlock metaInfo = do
  try (string "{%" *> spaces *> string "items" *> spaces *> string "%}")
  let end = (string "{%" *> spaces *> string "end_items" *> spaces *> string "%}")
  parts <- many (notFollowedBy end *> anyChar >>= (\c -> return [c]))
  end
  renderItems (Prelude.concat parts) metaInfo

renderItems :: String -> [Meta] -> Parser String
renderItems layout metaInfo = do
  case parse p "" (L.dropWhile isSpace layout) of
    Left err -> error "ParseError"
    Right itemNodes -> return $ renderItemsByNode itemNodes (L.take 20 metaInfo)
  where
    p :: Parser [ItemNode]
    p = do
      many (
        try parseTextNode <|>
        try parseVarNode
        )
    sorted = sortWithDesc (\m -> metaPublishedAt m) metaInfo

renderItemsByNode :: [ItemNode] -> [Meta] -> String
renderItemsByNode items metaInfo = do
  L.foldl (\x y -> x ++ renderItemByNode items y) "" metaInfo

renderItemByNode :: [ItemNode] -> Meta -> String
renderItemByNode items meta = do
  L.foldl (\x y -> x ++ renderNode y meta) "" items

renderNode :: ItemNode -> Meta -> String
renderNode (NVar str) m = do
  case str of
    "post_title" -> metaTitle m
    "post_content" -> metaDescription m
    "post_date" -> formatPublishedAt m formatRFC822
    "site_url" -> "https://blog.freedom-man.com"
    "post_url" -> "/" ++ takeBaseName (metaSrc m)
    _ -> "foobar"
  where
renderNode (NText str) _ = str


parseTextNode :: Parser ItemNode
parseTextNode = do
  lookAhead $ noneOf "{"
  chars <- manyTill anyChar ((lookAhead $ string "{{") <|> (eof *> return ""))
  return $ NText chars
--  NText <$> (many (notFollowedBy parseVarNode *> anyChar)) -- noneOf "{"))

parseVarNode :: Parser ItemNode
parseVarNode = do
  string "{{"
  spaces
  var <- many (noneOf " ")
  spaces
  string "}}"
  return $ NVar var

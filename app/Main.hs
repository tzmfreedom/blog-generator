{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Directory
import System.Posix.Time
import System.Posix.Files
import System.Posix.Types
import System.Environment
import Foreign.C.Types
import Control.Monad
import Control.Monad.Trans(lift)
import ListT
import CMark
import Data.Text
import System.FilePath.Posix
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Yaml (decodeFileEither)
import Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Char
import Data.List as L
import Data.Time.Clock
import Data.Time.Format
import GHC.Exts
import Debug.Trace
import Lib

data Meta = Meta{
  metaTitle :: String,
  metaDescription :: String,
  metaKeywords :: [String],
  metaSrc :: String,
  metaPublishedAt :: UTCTime
} deriving Show

data SiteInfo = SiteInfo{
  siName :: String,
  siDescription :: String,
  siSiteUrl :: String,
  siFeedPath :: String,
  siFeedUpdatePeriod :: String,
  siFeedUpdateFrequency :: Integer
} deriving Show

main :: IO ()
main = do
  args <- getArgs
  case if L.length(args) == 0 then "generate" else args !! 0 of
    "generate" -> generate
    "index" -> generateIndex
    "static" -> generateStatic
    "rss" -> generateRSS
    "new" -> newBlog args
    otherwise -> print $ "No such subcommand" ++ args !! 0
  return ()

generate :: IO ()
generate = do
  files <- findUnprocessedSource
  meta <- getMetaInfo files
  processFiles meta
  current <- epochTime
  writeFile lastUpdatedFile $ show current

generateIndex :: IO ()
generateIndex = do
  files <- findAllSource
  meta <- getMetaInfo files
  writeFile "dest/index.html" =<< renderIndex meta

generateStatic :: IO ()
generateStatic = processStatic ""

generateRSS :: IO ()
generateRSS = do
  files <- findAllSource
  meta <- getMetaInfo files
  content <- renderRSS meta
  writeFile (destDirectory ++ "/feed.xml") content

processStatic :: String -> IO ()
processStatic dir = do
  let dir' = (staticDirectory ++ "/" ++ dir)
  files <- listDirectory dir'
  mapM (generateStaticFile dir) files
  return ()

generateStaticFile :: FilePath -> FilePath -> IO ()
generateStaticFile dir f = do
  let relativePath = if dir == "" then f else dir ++ "/" ++ f
      src = staticDirectory ++ "/" ++ relativePath
  exist <- doesDirectoryExist src
  if exist then do
    let destDirectory = "dest" ++ L.drop (L.length staticDirectory) src
    exist <- doesDirectoryExist destDirectory
    if exist then return () else createDirectory destDirectory
    processStatic relativePath
  else do
    copyFile src $ "dest/" ++ L.drop (L.length staticDirectory) src
  return ()

newBlog :: [String] -> IO ()
newBlog args = do
  let fname = srcDirectory ++ "/new.md"
  let mname = metaDirectory ++ "/new.yml"
  let title = if L.length(args) == 2 then args !! 1 else "new article"
  doesExist <- doesFileExist fname
  if not doesExist then do
    writeFile fname $ "# " ++ title
    writeFile mname $ "description:\nkeywords: []\ntitle: " ++ title
    else return ()

lastUpdatedFile :: FilePath
lastUpdatedFile = "lastUpdated"

srcDirectory :: FilePath
srcDirectory = "blogs"

metaDirectory :: FilePath
metaDirectory = "meta"

layoutFile :: String
layoutFile = "layout.html"

staticDirectory :: String
staticDirectory = "static"

destDirectory :: String
destDirectory = "dest"

readSiteInfo :: IO SiteInfo
readSiteInfo = do
  siteInfo <- decodeFileEither siteFile
  return (getSiteInfo siteInfo siteFile)
  where
    siteFile :: FilePath
    siteFile = "site.yml"
    getSiteInfo :: Either a SiteInfo -> FilePath -> SiteInfo
    getSiteInfo (Right a) _ = a
    getSiteInfo (Left err) f = error f

lastUpdated :: IO String
lastUpdated = readFile lastUpdatedFile

findAllSource :: IO [FilePath]
findAllSource = do
  files <- listDirectory srcDirectory
  return $ Prelude.map (\f -> srcDirectory ++ "/" ++ f) files

findUnprocessedSource :: IO [FilePath]
findUnprocessedSource = do
  files <- listDirectory srcDirectory
  layoutFileChanged <- shouldProcessed layoutFile
  let src = Prelude.map (\f -> srcDirectory ++ "/" ++ f) files
  if layoutFileChanged then return src else ListT.toList $ findUnprocessedSource' src

findUnprocessedSource' :: [FilePath] -> ListT IO FilePath
findUnprocessedSource' files = do
  f <- fromFoldable files
--  lift $ print f
  continue <- lift (shouldProcessed f)
  guard continue
  return f

shouldProcessed :: FilePath -> IO Bool
shouldProcessed f = do
  status <- getFileStatus f
  last <- lastUpdated
  let last' = read last :: EpochTime
  return (last' < modificationTime status)

-- get layout html
-- Markdown => HTML
-- write HTML
processFiles :: [Meta] -> IO [()]
processFiles metaInfo = do
  (flip mapM) metaInfo $ \m -> do
    let src = metaSrc m
    content <- readFile src
    html <- generateHTML (render content) m
    writeFile (destFile src) html

getMetaInfo:: [FilePath] -> IO [Meta]
getMetaInfo files = do
  (flip mapM) files $ \f -> do
    meta <- decodeFileEither $ metaFile f
    return $ getMeta meta f

getMeta :: Either a Meta -> FilePath -> Meta
getMeta (Right a) _ = a
getMeta (Left err) f = error f

metaFile :: FilePath -> String
metaFile f = metaDirectory ++ "/" ++ (Prelude.drop (Prelude.length srcDirectory) (replaceExtension f ".yml"))

destFile :: FilePath -> String
destFile f = "dest/" ++ (Prelude.drop (Prelude.length srcDirectory) (replaceExtension f ".html"))

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
    "<meta name=\"description\" contents=\"" ++ metaDescription meta ++ "\"/>"
    where
      keys :: String
      keys = L.intercalate "," $ metaKeywords meta

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
    lst = L.foldl (\x m -> x ++ "<li>" ++ formatPublishedAt m "%Y-%m-%d" ++ ": <a href=\"" ++ metaToPath m ++ "\">" ++ metaTitle m ++ "</a></li>") "" sorted
    sorted = sortWithDesc (\m -> metaPublishedAt m) metaInfo

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

formatRFC822 :: String
formatRFC822 = "%a, %d %b %Y %T %z"

parseItemsBlock :: [Meta] -> Parser String
parseItemsBlock metaInfo = do
  try (string "{%" *> spaces *> string "items" *> spaces *> string "%}")
  let end = (string "{%" *> spaces *> string "end_items" *> spaces *> string "%}")
  parts <- many (notFollowedBy end *> anyChar >>= (\c -> return [c]))
  end
  renderItems (Prelude.concat parts) metaInfo

data ItemNode = NVar String
  | NText String
  deriving Show

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

formatPublishedAt :: Meta -> String -> String
formatPublishedAt m format = formatTime defaultTimeLocale format $ metaPublishedAt m

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

sortWithDesc :: Ord b => (a -> b) -> [a] -> [a]
sortWithDesc f = sortBy (\x y -> compare (f y) (f x))

metaToPath :: Meta -> String
metaToPath meta = "/" ++ (takeBaseName $ metaSrc meta)

deriveJSON defaultOptions { fieldLabelModifier = firstLower . Prelude.drop 4 } ''Meta
deriveJSON defaultOptions { fieldLabelModifier = firstLower . Prelude.drop 2 } ''SiteInfo

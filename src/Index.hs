module Index where

import Text.Parsec
import Text.Parsec.String
import Data.List
import Lib

generateIndexCommand :: [String] -> IO ()
generateIndexCommand args = do
  let srcDirectory = if length args == 0 then defaultSrcDirectory else args !! 0
      metaDir = if length args < 2 then metaDirectory else args !! 1
  files <- findAllSource srcDirectory
  meta <- getMetaInfo metaDir files
  writeFile "dist/index.html" =<< renderIndex "All" meta
  renderYearCategory meta

renderIndex :: String -> [Meta] -> IO String
renderIndex title metaInfo = do
  layout <- readFile "index.html"
  case parse p "" layout of
    Left err -> error "ParseError"
    Right html -> return html
  where
    p :: Parser String
    p = do
      parts <- many (
        try (string "{{" *> spaces *> string "body" *> spaces *> string "}}" *> return ("<ul>" ++ lst ++ "</ul>")) <|>
        try (string "{{" *> spaces *> string "title" *> spaces *> string "}}" *> return ("<h1>" ++ title ++ "</h1>")) <|>
        (anyChar >>= (\c -> return [c]))
        )
      return $ Prelude.concat parts
    lst :: String
    lst = foldl (\x year -> x ++ "<li><a href=\"/" ++ year ++ "\">" ++ year ++ "</a></li>") "" years
    years :: [String]
    years = do
      let years' = foldl (\xs m -> if (formatPublishedAt m "%Y") `elem` xs then xs else formatPublishedAt m "%Y":xs) [] metaInfo
      sortBy(\x y -> compare y x) years'

renderCategoryOne :: String -> [Meta] -> IO String
renderCategoryOne title metaInfo = do
  layout <- readFile "index.html"
  case parse p "" layout of
    Left err -> error "ParseError"
    Right html -> return html
  where
    p :: Parser String
    p = do
      parts <- many (
        try (string "{{" *> spaces *> string "body" *> spaces *> string "}}" *> return ("<ul>" ++ lst ++ "</ul>")) <|>
        try (string "{{" *> spaces *> string "title" *> spaces *> string "}}" *> return ("<h1>" ++ title ++ "</h1>")) <|>
        (anyChar >>= (\c -> return [c]))
        )
      return $ Prelude.concat parts
    lst :: String
    lst = foldl (\x m -> x ++ "<li>" ++ formatPublishedAt m "%Y-%m-%d" ++ ": <a href=\"" ++ metaToPath m ++ "\">" ++ metaTitle m ++ "</a></li>") "" sorted
    sorted :: [Meta]
    sorted = sortWithDesc (\m -> metaPublishedAt m) metaInfo

renderYearCategory :: [Meta] -> IO ()
renderYearCategory metaInfo =
  mapM_ (\y -> render y) years
  where
    render :: String -> IO ()
    render year = do
      content <- renderCategoryOne year $ filter (\m -> formatPublishedAt m "%Y" == year) metaInfo
      writeFile ("dist/" ++ year ++ ".html") content
    years :: [String]
    years = foldl (\xs m -> if (formatPublishedAt m "%Y") `elem` xs then xs else formatPublishedAt m "%Y":xs) [] metaInfo

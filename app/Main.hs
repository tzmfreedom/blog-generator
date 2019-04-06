module Main where

import System.Environment
import System.IO
import Rss
import Sitemap
import Static
import Index
import Blog

main :: IO ()
main = do
  args <- getArgs
  let (x:xs) = if length args == 0 then ["generate"] else args
  dispatch x xs
  where
    dispatch :: String -> [String] -> IO ()
    dispatch command = do
      case command of
        "generate" -> generateCommand
        "index" -> generateIndexCommand
        "static" -> generateStaticCommand
        "rss" -> generateRSSCommand
        "sitemap" -> generateSitemapCommand
        otherwise -> noSuchCommand

    noSuchCommand :: [String] -> IO ()
    noSuchCommand args = do
      hPutStrLn stderr $ "No such subcommand: " ++ args !! 0

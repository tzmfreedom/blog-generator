module Main where

import System.Environment
import Rss
import Sitemap
import Static
import Index
import Blog

main :: IO ()
main = do
  args <- getArgs
  case if length(args) == 0 then "generate" else args !! 0 of
    "generate" -> generate
    "index" -> generateIndex
    "static" -> generateStatic
    "rss" -> generateRSS
    "sitemap" -> generateSitemap
    otherwise -> print $ "No such subcommand" ++ args !! 0
  return ()

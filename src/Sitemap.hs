{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Sitemap where

import System.Directory
import Data.List as L
import Lib

generateSitemap :: IO ()
generateSitemap = do
  files <- findAllSource
  meta <- getMetaInfo files
  content <- renderSitemap meta
  writeFile (distDirectory ++ "/sitemap.xml") content

renderSitemap :: [Meta] -> IO String
renderSitemap metaInfo = do
  siteInfo <- readSiteInfo
  return (start ++ (L.foldl (\x m -> x ++ "<url><loc>" ++ siSiteUrl siteInfo ++ metaToPath m ++ "</loc></url>") "" metaInfo) ++ end)
  where
    start :: String
    start = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
    end :: String
    end = "</urlset>"

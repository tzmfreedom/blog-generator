{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import System.Directory
import System.Posix.Files
import System.Posix.Types
import System.Environment
import Foreign.C.Types
import Control.Monad
import Control.Monad.Trans(lift)
import ListT
import System.FilePath.Posix
import Data.Text
import Data.Yaml (decodeFileEither)
import Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Char
import Data.List as L
import GHC.Exts
import Debug.Trace
import Data.Time.Clock
import Data.Time.Format
import Util

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

lastUpdatedFile :: FilePath
lastUpdatedFile = "lastUpdated"

defaultSrcDirectory :: FilePath
defaultSrcDirectory = "blogs"

metaDirectory :: FilePath
metaDirectory = "meta"

layoutFile :: String
layoutFile = "layout.html"

staticDirectory :: String
staticDirectory = "static"

distDirectory :: String
distDirectory = "dist"

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

findAllSource :: String -> IO [FilePath]
findAllSource srcDirectory = do
  files <- listDirectory srcDirectory
  return $ Prelude.map (\f -> srcDirectory ++ "/" ++ f) files

findUnprocessedSource :: String -> IO [FilePath]
findUnprocessedSource srcDirectory = do
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

getMetaInfo:: FilePath -> [FilePath] -> IO [Meta]
getMetaInfo dir files = do
  (flip mapM) files $ \f -> do
    meta <- decodeFileEither $ metaFile dir f
    return $ getMeta meta f

getMeta :: Either a Meta -> FilePath -> Meta
getMeta (Right a) _ = a
getMeta (Left err) f = error f

metaFile :: FilePath -> FilePath -> String
metaFile dir f = do
  let d:ds = splitDirectories f
  dir ++ "/" ++ (replaceExtension (joinPath ds) ".yml")

destFile :: FilePath -> String
destFile f = do
  let d:ds = splitDirectories f
  distDirectory ++ "/" ++ (replaceExtension (joinPath ds) ".html")

formatRFC822 :: String
formatRFC822 = "%a, %d %b %Y %T %z"

formatPublishedAt :: Meta -> String -> String
formatPublishedAt m format = formatTime defaultTimeLocale format $ metaPublishedAt m

sortWithDesc :: Ord b => (a -> b) -> [a] -> [a]
sortWithDesc f = sortBy (\x y -> compare (f y) (f x))

metaToPath :: Meta -> String
metaToPath meta = "/" ++ (takeBaseName $ metaSrc meta)

deriveJSON defaultOptions { fieldLabelModifier = firstLower . Prelude.drop 4 } ''Meta
deriveJSON defaultOptions { fieldLabelModifier = firstLower . Prelude.drop 2 } ''SiteInfo

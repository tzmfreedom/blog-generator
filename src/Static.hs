{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Static where

import System.Directory
import Data.List as L
import Lib

generateStaticCommand :: [String] -> IO ()
generateStaticCommand _ = processStatic ""

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
    let distDirectory = "dist" ++ L.drop (L.length staticDirectory) src
    exist <- doesDirectoryExist distDirectory
    if exist then return () else createDirectory distDirectory
    processStatic relativePath
  else do
    copyFile src $ "dist/" ++ L.drop (L.length staticDirectory) src
  return ()

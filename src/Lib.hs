module Lib
    ( firstLower,
    ) where

import Data.Char

firstLower :: String -> String
firstLower [] = []
firstLower (x:xs) = toLower x:xs

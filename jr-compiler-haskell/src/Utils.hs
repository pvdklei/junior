module Utils where

import Data.List
import qualified Control.Monad as CM

-- does not work 
split :: (a -> Bool) -> [a] -> [[a]]
split on lst = case break on lst of
    ([], rest) -> [rest]
    (rest, []) -> [rest]
    (prefix, rest) -> prefix : split on rest

splitExc :: (a -> Bool) -> [a] -> [[a]]
splitExc on lst = case break on lst of
    ([], a : rest) -> splitExc on rest
    (rest, []) -> [rest]
    (prefix, a : rest) -> prefix : splitExc on rest

getFileNameFromPath :: String -> String
getFileNameFromPath path = 
    let name = last $ splitExc (=='/') path in
    case stripPrefix "/" name of
        Just name' -> name'
        Nothing -> name

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix qs xs0 = go xs0 zs
  where
    zs = drp qs xs0
    drp (_:ps) (_:xs) = drp ps xs
    drp [] xs = xs
    drp _  [] = []
    go (_:xs) (_:ys) = go xs ys
    go xs [] = zipWith const xs0 zs <$ CM.guard (xs == qs)
    go [] _  = Nothing -- impossible



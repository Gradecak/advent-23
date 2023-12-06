module Main (main) where

import Control.Applicative
import Control.Monad (sequence)
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

firstDigit :: String -> Maybe Int
firstDigit [] = Nothing
firstDigit (x : xs)
  | isDigit x = Just $ digitToInt x
  | otherwise = firstDigit xs

lineCalibration :: String -> Maybe Int
lineCalibration line = (+) <$> ((*) <$> firstDigit line <*> Just 10) <*> firstDigit (reverse line)

calibrationCode :: [String] -> Maybe Int
calibrationCode lines = sum <$> mapM lineCalibration lines

window :: [a] -> Int -> [[a]]
window inp@(x : xs) size
  | length inp == size = [inp]
  | otherwise = take size inp : window xs size

wordToDigit :: String -> Maybe Int
wordToDigit inp@(x : xs)
  | isDigit x = read [x]
  | "one" `isPrefixOf` inp = Just 1
  | "two" `isPrefixOf` inp = Just 2
  | "three" `isPrefixOf` inp = Just 3
  | "four" `isPrefixOf` inp = Just 4
  | "five" `isPrefixOf` inp = Just 5
  | "six" `isPrefixOf` inp = Just 6
  | "seven" `isPrefixOf` inp = Just 7
  | "eight" `isPrefixOf` inp = Just 8
  | "nine" `isPrefixOf` inp = Just 9
  | otherwise = Nothing

convertToDigit :: String -> [Maybe Int]
convertToDigit inp = map wordToDigit (window inp 5)

main :: IO ()
main = do
  inp <- readFile "input.txt" >>= \content -> return (lines content)
  print $ calibrationCode inp

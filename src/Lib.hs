module Lib
    ( square, lowerAlphabet, upperAlphabet, digits, isUpper, isLower, isDigit, isMisc
    ) where

square :: Int -> Int
square x = x * x

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet
isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet
isDigit :: Char -> Bool
isDigit char = char `elem` digits
isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

listLength [] = 0
listLength (x:xs) = 1 + listLength xs

indexOf :: Char -> Alphabet -> Int
indexOf ch [] = undefined
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

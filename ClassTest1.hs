-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1 (checkParity, substitution, largestPrimeBetween, strongPrimes, executeCommands, atmChange) where

import Types
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
-- takes a string of bits, checks that the string size is a multiple of 8 and each byte in the string has even parity (if there is an even number of 1s)

checkParity :: String -> Bool
checkParity xs = (length xs `mod` 8 == 0) && (length ((filter (=='1')) xs)) `mod` 2 == 0


{- Question 2 -}
-- takes a plaintext string and a substitution cipher key and returns the ciphertext string
-- capitalisation must be preserved and non-alphabetic characters must be preserved
-- if a letter was lowercase before, then it should still be lowercase after the conversion

substitution :: String -> String -> String
substitution xs ys = [if isLetter x then if isLower x then toLower (ys !! (ord x - ord 'a')) else ys !! (ord x - ord 'A') else x | x <- xs]

{- Question 3 -}
-- function should return the largest prime between n and 2n

largestPrimeBetween :: Int -> Int
largestPrimeBetween n = largestPrimeBetweenHelper (2*n) n

largestPrimeBetweenHelper :: Int -> Int -> Int
largestPrimeBetweenHelper n m
  | isPrime n = n
  | otherwise = largestPrimeBetweenHelper (n-1) m

-- find all strong prime numbers up to the number n
-- a strong prime is a prime number which is larger than the average of the next prime above and below it
-- iterate all primes up to n and check if they are strong primes
-- return a list of strong primes
:
strongPrimes :: Int -> [Int]
strongPrimes n = undefined
-- I can't get this to work just fucking kill me next time please


{- Question 4 -}
-- take a list of commands (directions) and for each command, change the coordinates to reflect the command
-- recurse until all commands are executed
-- return the final coordnates after all commands have been executed

executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands [] (x,y) = (x,y)
executeCommands (x:xs) (a,b) = executeCommands xs (executeCommand x (a,b))

executeCommand :: Command -> (Int, Int) -> (Int, Int)
executeCommand (MoveLeft, n) (x, y) = (x - n, y)
executeCommand (MoveRight, n) (x, y) = (x + n, y)
executeCommand (MoveUp, n) (x, y) = (x, y + n)
executeCommand (MoveDown, n) (x, y) = (x, y - n)


{- Question 5 -}
-- take an int which is the required amount of change, and a list of denominations
-- return a list of denominations that add up to the required amount of change
-- denominations will always be given in ascending order, so will need to flip it round
-- return should be a list of touples, (denomination, qty)


atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange n [] = []
atmChange n (x:xs) = atmChangeHelper n (reverse (x:xs))

atmChangeHelper :: Int -> [Int] -> [(Int, Int)]
atmChangeHelper n [] = []
atmChangeHelper n (x:xs) = (x, n `div` x) : atmChangeHelper (n `mod` x) xs
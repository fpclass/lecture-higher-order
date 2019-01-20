--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 7: Higher-order functions                                          --
--------------------------------------------------------------------------------

module Lecture7 where

import Prelude hiding ( map, filter, and, product, length, foldr, foldl, (.)
                      , curry, uncurry )
import Data.Char (isUpper)

--------------------------------------------------------------------------------
-- xor (with explicit brackets):
-- functions and function types associate to the right

xor :: Bool -> (Bool -> Bool)
xor = \a -> (\b -> (a || b) && not (a && b))

-- function application associates to the left
e0 :: Bool
e0 = (xor True) True

--------------------------------------------------------------------------------
-- map

incByOneLC :: [Int] -> [Int]
incByOneLC xs = [x+1 | x <- xs]

incByOne :: [Int] -> [Int]
incByOne []     = []
incByOne (x:xs) = x+1 : incByOne xs

evensLC :: [Int] -> [Bool]
evensLC xs = [even x | x <- xs]

evens :: [Int] -> [Bool]
evens []     = []
evens (x:xs) = even x : evens xs

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

incByOne' :: [Int] -> [Int]
incByOne' = map (+1)

evens' :: [Int] -> [Bool]
evens' = map even

--------------------------------------------------------------------------------
-- filter

greaterThan42LC :: [Int] -> [Int]
greaterThan42LC xs = [x | x <- xs, x > 42]

greaterThan42 :: [Int] -> [Int]
greaterThan42 [] = []
greaterThan42 (x:xs)
    | x > 42    = x : greaterThan42 xs
    | otherwise =     greaterThan42 xs

uppersLC :: [Char] -> [Char]
uppersLC xs = [x | x <- xs, isUpper x]

uppers :: [Char] -> [Char]
uppers [] = []
uppers (x:xs)
    | isUpper x = x : uppers xs
    | otherwise =     uppers xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise =     filter p xs

greaterThan42' :: [Int] -> [Int]
greaterThan42' = filter (> 42)

uppers' :: [Char] -> [Char]
uppers' = filter isUpper

--------------------------------------------------------------------------------
-- curried vs uncurried

uncurriedAdd :: (Int, Int) -> Int
uncurriedAdd (x,y) = x + y

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

uncurriedAdd' :: (Int, Int) -> Int
uncurriedAdd' = uncurry (+)

curriedAdd :: Int -> Int -> Int
curriedAdd = curry uncurriedAdd

addPairs :: [Int]
addPairs = map (uncurry (+)) [(1,2), (4,8)]

--------------------------------------------------------------------------------
-- folds

and :: [Bool] -> Bool
and []     = True
and (b:bs) = b && and bs

product :: Num a => [a] -> a
product []     = 1
product (n:ns) = n * product ns

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

and' :: [Bool] -> Bool
and' = foldr (&&) True

product' :: Num a => [a] -> a
product' = foldr (*) 1

length' :: [a] -> Int
length' = foldr (\x n -> n + 1) 0

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z
foldl f z (x:xs) = f (foldl f z xs) x

and'' :: [Bool] -> Bool
and'' = foldl (&&) True

product'' :: Num a => [a] -> a
product'' = foldl (*) 1

length'' :: [a] -> Int
length'' = foldl (\n x -> n + 1) 0

--------------------------------------------------------------------------------
-- count

-- count 't' "witter" => 2

--------------------------------------------------------------------------------

--FICHA 4 

module Test where

import Data.List
import Data.Char

--ex3

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isDigit x = (x:a,b)
                  | isAlpha x = (a,x:b)
                  | otherwise = (a,b)
                       where (a,b) = digitAlpha xs 

--      OU    --
--digitAlpha :: String -> (String,String)
--digitAlpha string = foldl (\(alpha,digit) x -> if isDigit x then (alpha,digit ++ [x]) else if isAlpha x then (alpha ++ [x],digit) else (alpha,digit)) ("","") string

--ex 4

nzp :: [Int] -> (Int,Int,Int)
nzp numeros = foldl (\(neg,zero,pos) x -> if x < 0 then (neg+1,zero,pos) else if x == 0 then (neg,zero+1,pos) else (neg,zero,pos+1)) (0,0,0) numeros 

--ex 5 

divMod' :: Integral a => a -> a -> (a, a)
divMod' x y = (divisao x y , resto x y)
               where divisao x y | x >= y = 1 + divisao (x-y) y  
                                 | otherwise = 0
                     resto x y | x >= y = resto (x-y) y 
                               | otherwise = x 
-- divMod' x y = foldl (\(a,b) n -> (a+1,b-y)) (0,x) [y..x]

--ex 6 

fromDigits' :: [Int] -> Int
fromDigits' = foldl (\ acc x -> x + 10 * acc) 0

--ex7

 


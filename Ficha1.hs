module Ficha01 where

import Data.Char

--1
--a)
perimetro :: Double -> Double
perimetro r = 2 * pi * r 

--b)
dist :: (Num a, Eq a, Floating a) => (a,a) -> (a,a) -> a
dist (x1,y1) (x2,y2) = sqrt((x2-x1)^2+(y2-y2)^2)

--c)
primUlt :: [a] -> (a,a)
primUlt l = (head l , last l)

--d)
multiplo :: Int -> Int -> Bool
multiplo m n = if (mod m n) == 0 then True else False 

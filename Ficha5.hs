module Test where

import Data.List
import Data.Char

-- ex 1

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) | (f x == True) = True
              | otherwise = any' f xs 

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (h:t) = (f x h) : (zipWith' f xs t)

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = [] 
takeWhile' f (x:xs) | (f x == True) = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = [] 
dropWhile' f (x:xs) | (f x == True) = dropWhile' f xs
                    | otherwise = (x:xs)

span' :: (a-> Bool) -> [a] -> ([a],[a])
-- ([lista com takeWhile],[lista com dropWhile])
span' f [] = ([],[]) 
span' f (x:xs) | (f x == True) = (x:a,b)
               | otherwise = (a,x:b)
                 where (a,b) = span f xs

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f y [] = []
deleteBy' f y (x:xs) | (f y x ==True) = xs
                     | otherwise = x : deleteBy' f y xs

sortOn' :: Ord b => (a -> b) -> [a] -> [a] 
sortOn' f [] = []
sortOn' f (x:xs) = insere (x) (sortOn' f xs) 
                where insere x [] = [x]
                      insere x (h:t) | (f x > f h) = h : insere x t 
                                     | otherwise = x : h : t 

--ex 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n ps = filter (\x -> snd x==n) ps 

conta :: Int -> Polinomio -> Int 
conta n ps = length (filter (\x -> snd x ==n) ps)

--ex 3 

type Mat a = [[a]]

--a)
dimOK :: Mat a -> Bool 
dimOK [x] = True
dimOK (x:xs) | length x == length (head xs) = dimOK xs
             | otherwise = False 

--b)
dimMat :: Mat a -> (Int,Int) 
dimMat [] = (0,0) 
dimMat (x:xs) = (a,b) 
                  where a = length (x:xs) 
                        b = length x

--c)
addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat [] [] = []
addMat m n = (addlist (head m) (head n)) : addMat (tail m) (tail n)
                  where addlist l [] = l
                        addlist [] l = l
             	        addlist (x:xs) (y:ys) = (x+y) : addlist xs ys 

--d)
transpose :: Mat a -> Mat a 
transpose [] = []
transpose m = (map (head) m) : transpose (map (tail) m)

--e)
multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat [] m = []
multMat m [] = []
multMat m n = (multAux (head m) n) : multMat (tail m) n 
              where multAux [] l = []
                    multAux l [] = []
                    multAux l ls = let k = map (head) ls 
                                   in (sum (zipwith (*) l k)) : multAux l (map (tail) ls)

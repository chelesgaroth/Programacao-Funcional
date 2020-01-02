module Test where

import Data.List
import Data.Char
import Data.Either 

--1)
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y = if (x>=y) then [x] else  x : enumFromTo' (x+1) y

--2)
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z = if (x>z) then [] else x : enumFromThenTo' y (2 * y - x) z

--3)
plus' :: [a] -> [a] -> [a]
plus' [] l = l 
plus' (h:t) l= h : plus' t l 

--4)
exclamation' :: [a] -> Int -> a
exclamation' (h:t) 0 = h
exclamation' (h:t) x = exclamation' t (x-1)

--5)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--6)
take' :: Int -> [a] -> [a]
take' 0 l = []
take' x (h:t) = h : take' (x-1) t 

--7)
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' x (h:t) = drop' (x-1) t 

--8)
zip' :: [a] -> [b] -> [(a,b)]
zip' l [] = []
zip' [] l = []
zip' (x:y) (h:t) = (x,h) : zip' y t   

--9)
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (h:t) = if (x==h) then True else elem' x t 

--10)
replicate' :: Int -> a -> [a]
replicate' 0 y = []
replicate' x y = [y] ++ replicate' (x-1) y

--11)
intersperse' :: a -> [a] -> [a]
intersperse' x [] = []
intersperse' x (h:[]) = [h] 
intersperse' x (h:t) = h : x : intersperse' x t

--12)
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h : takeWhile (==h) t) : group' (dropWhile (==h) t)

--faz uma lista com todos os nºs iguais a h , e depois vai dropa los 
--para fazer o resto das listas da lista com os outros nºs
--ATENCAO tbm podiamos fazer com uma funcao auxiliar 

grupo :: Eq a => [a] -> [[a]]
grupo [] = [[]]
grupo (h:t) = aux [h] t 
                where aux l [] = [l]
                      aux l (h:t) | elem h l = aux (h:l) t 
                                  | otherwise = l : aux [h] t
               
--13) 
concat' :: [[a]] -> [a]
concat' [] = []
concat (h:t) = h ++ concat' t 

--14)
inits' :: [a] -> [[a]]
inits' [] = [[]] 
inits' l = inits' (init l) ++ [l] 

--15)
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails (tail l)

--16)
isPrefixof' :: Eq a => [a] -> [a] -> Bool 
isPrefixof' [] l = True 
isPrefixof' (x:xs) (y:ys) = if (x==y) then isPrefixof' xs ys else False

--17)
isSuffixof' :: Eq a => [a] ->[a] -> Bool
isSuffixof' [] l = True
isSuffixof' l ls = if (last l) == (last ls) then isSuffixof' (init l) (init ls) else False

--18)
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (x:xs) (y:ys) = if (x==y) then isSubsequenceOf' xs ys else isSubsequenceOf' (x:xs) ys

--19)
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' a [] = []
elemIndices' a l = aux 0 a l 
                       where aux i a [] = [] 
                             aux i a (x:xs) | (a==x) = [i] ++ aux (i+1) a xs
                                            | otherwise = aux (i+1) a xs

--20)
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = [x] ++ nub'(aux x xs) 
              where aux a [] = []
                    aux a (x:xs) | (a==x) = aux a xs
                                 | otherwise = [x] ++ (aux a xs)

--21)
delete' :: Eq a => a -> [a] -> [a]
delete' a [] = []
delete' a (x:xs) = if (a==x) then xs else x : delete' a xs 

--22) (//)
hme :: Eq a => [a] -> [a] -> [a]
hme l [] = l
hme [] l = []
hme (x:xs) (y:ys)= if (x==y) then hme xs ys else x : hme xs (y:ys) 

--23)
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' [] l = l
union' (x:xs) (y:ys) = if (x==y) then x : union' xs ys else x : union' xs (y:ys) 

--24)
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' l [] = []
intersect' [] l = []
intersect' (x:xs) (y:ys) = if (x==y) then x : intersect xs ys else intersect xs (y:ys)

--25)
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs) |(a<x) = a : x : xs
                 | otherwise = x : insert' a xs

--26)
unwords' :: [String] -> String
unwords' [] = ""
unwords' [h] = h 
unwords' (h:t) = h ++ " " ++ unwords' t

--27)
unlines' :: [String] -> String
unlines' [] = ""
unlines' [h] = h ++ "\n"
unlines' (h:t) = h ++ "\n" ++ unlines' t

--28)
pMaior' :: Ord a => [a] -> Int
pMaior' [x] = 0
pMaior' (x:xs) = aux x xs 0 0 
                 where aux x [] a i = a 
                       aux x xs a i |(x >= (head xs)) = aux x (tail xs) a (i+1)
                                    |otherwise = aux (head xs) (tail xs) (i+1) (i+1)

--29)
temRepetidos' :: Eq a => [a] -> Bool
temRepetidos' [] = False
temRepetidos' (x:xs) = aux x xs
                       where aux x [] = temRepetidos' xs
                             aux x xs |(x==(head xs)) = True
                                      |otherwise = aux x (tail xs)  

--30)
algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' (x:xs) = if (isDigit x ==True) then x : algarismos' xs else algarismos' xs 

--31)
posImpares' :: [a] -> [a]
posImpares' [] = []
posImpares' [x] = []
posImpares' (x:xs) = (head xs) : posImpares' (tail xs)

--32)
posPares' :: [a] -> [a]
posPares' [] = []
posPares' [x] = [x]
posPares' (x:xs) = x : posPares' (tail xs)

--33)
isSorted' :: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' (x:xs) = aux x xs 
                 where aux x [] = True
                       aux x (h:t) |(x<=h) = aux h t 
                                   |otherwise = False

--34)
iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' [x] = [x]
iSort' (h:t) = insert' h (iSort' t)

--35)
menor' :: String -> String -> Bool
menor' "" x = True
menor' x "" = False
menor' (x:xs) (y:ys) |(x<y) = menor' xs ys
                     |(x==y) = menor' xs ys 
                     | otherwise = False

--36)
elemMSet' ::Eq a => a -> [(a,Int)] -> Bool
elemMSet' x [] = False
elemMSet' x ((y,ys):xs) = if (x==y) then True else elemMSet' x xs

--37)
lengthMSet' ::[(a,Int)] -> Int
lengthMSet' [] = 0
lengthMSet' ((x,y):xs) = y + lengthMSet' xs 

--38)
converteMSet' ::[(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,y):xs) = aux (x,y) ++ converteMSet' xs
                        where aux (x,0) = []
                              aux (x,y) = [x] ++ aux (x,y-1)

--39)
insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' a [] = [(a,1)]
insereMSet' a ((x,y):xs) |(a==x) = ((x,y+1):xs)
                         |otherwise= (x,y) : insereMSet' a xs 

--40)
removeMSet' ::Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' a [] = []
removeMSet' a ((x,y):xs) | (x==a) && (y==1) = xs
                         | (x==a) && (y>1) = ((x,y-1):xs) 
                         | otherwise = (x,y) : removeMSet' a xs

--41) 
constroiMSet' ::Ord a => [a] -> [(a,Int)] 
constroiMSet' [] = []
constroiMSet' l = aux 1 l 
                where aux i [x] = [(x,i)] 
                      aux i (x:y:xs) | (x==y) = aux (i+1) (y:xs) 
                                     | otherwise = (x,i) : aux 1 (y:xs)

--42)
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a) :t) = (a:as,bs)
         where (as,bs) = partitionEithers' t 
partitionEithers' ((Right b) :t) = (as,b:bs)
         where (as,bs) = partitionEithers' t 

--43)
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just a : xs) = a : catMaybes' xs
catMaybes' (Nothing : xs) = catMaybes' xs 

--44)
data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' (x,y) [] = (x,y)
posicao' (x,y) (Norte : xs) = posicao' (x,y+1) xs
posicao' (x,y) (Sul : xs) = posicao' (x,y-1) xs
posicao' (x,y) (Este : xs) = posicao' (x+1,y) xs 
posicao' (x,y) (Oeste : xs) = posicao' (x-1,y) xs

--45)
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1==x2 && y1==y2 = []
                        | x1<x2            = [Este]  ++ caminho (x1+1,y1) (x2,y2)
                        | x1>x2            = [Oeste] ++ caminho (x1,y1) (x2+1,y2)
                        | y1<y2            = [Norte] ++ caminho (x1,y1+1) (x2,y2)
                        | y1>y2            = [Sul]   ++ caminho (x1,y1) (x2,y2+1)

--46)
vertical :: [Movimento] -> Bool
vertical []         = True
vertical (Norte:xs) = vertical xs
vertical (Sul:xs)   = vertical xs
vertical (Este:xs)  = False
vertical (Oeste:xs) = False

--47)
data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao 
maisCentral [x] = x
maisCentral (x:y:xs) | (aux(x) < aux(y)) = maisCentral (x:xs)
                     | otherwise  = maisCentral (y:xs) 
                            where aux (Pos x y) = sqrt(fromIntegral (x^2 + y^2)) 

--48)
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos

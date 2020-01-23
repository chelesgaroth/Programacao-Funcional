module Ficha8 where 

import Data.Char 
import Data.List
import Data.Maybe 

--ex1:

data Frac = F Integer Integer
--b) c) d) e)

instance Eq Frac where
    (F a b) == (F c d) = a * d == c * b

instance Ord Frac where 
	(F a b) <= (F c d) = a * d <= c * b

instance Show Frac where
	show (F a b) = show a ++ "/" ++ show b

instance Num Frac where
    (F a b) + (F c d) | b == d = (F (a + b) b)
                      | otherwise = (F (a * d) (b * d)) + (F (c * b) (d * b))
    
    (F a b) - (F c d) | b == d = (F (a + (negate b)) b)
                      | otherwise = (F (a * d) (b * d)) - (F (c * b) (d * b))
    
    (F a b) * (F c d) = (F (a * c) (b * d))
    
    negate (F a b) = (F (-a) b)
    
    abs (F a b) = (F (abs a) (abs b))
    
    signum (F a b) | a == 0 = 0 --sinal do número 
                   | a * b > 0 = 1
                   | otherwise = (-1)
    
    fromInteger x = F x 1

--a)
normaliza :: Frac -> Frac
normaliza (F x y) = let p = mdc (abs x) (abs y)
                     in sinal $ F (div x p) (div y p)

sinal :: Frac -> Frac
sinal (F x y) | (x<0 && y<0) || (x>=0 && y>=0) = (F (abs x) (abs y))
              | (x<0 && y>=0) = (F x y)
              | otherwise = (F (-x) (abs y))

mdc :: Integer -> Integer -> Integer
mdc x 0 = 0 
mdc 0 x = 0
mdc x y | x== y = x 
        | x < y = mdc x (y-x)
        | otherwise = mdc (x-y) y 

--f)
maioresQueDobro :: Frac -> [Frac] -> [Frac]
maioresQueDobro f l = filter (> 2 * f) l


--ex2:
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

infixa' :: (Show a) => Exp a -> String
infixa' (Const x) = show x
infixa' (Simetrico x) = "- (" ++ infixa' x ++ ")"
infixa' (Mais x y) = "(" ++ (infixa' x) ++ " + " ++ (infixa' y) ++ ")"
infixa' (Menos x y) = "(" ++ (infixa' x) ++ " - " ++ (infixa' y) ++ ")"
infixa' (Mult x y) = "(" ++ (infixa' x) ++ " * " ++ (infixa' y) ++ ")"

calcula' :: (Eq a,Num a) => Exp a -> a
calcula' (Const x) = x
calcula' (Simetrico x) = - (calcula' x)
calcula' (Mais x y) = (calcula' x) + (calcula' y)
calcula' (Menos x y) = (calcula' x) - (calcula' y)
calcula' (Mult x y) = (calcula' x) * (calcula' y)

--a)
instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

--b)
instance (Eq a,Num a) => Eq (Exp a) where
    (==) x y = (calcula' x) == (calcula' y)

 --c)
instance (Num a,Eq a) => Num (Exp a) where
    (+) x y = Const (calcula' x + calcula' y)
    (-) x y = Const (calcula' x - calcula' y)
    (*) x y = Const (calcula' x * calcula' y)
    negate x = Const (calcula' (Simetrico x))
    abs x = Const (abs (calcula' x))
    fromInteger x = Const (fromInteger x)
    signum x = Const (signum (calcula' x))


--NOTAS

type MSet a = [(a,Int)]

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((a,x):xs) = (aux (a,x)) ++ converteMSet xs
                       where aux (a,0) = []
                             aux (a,x) = [a] ++ aux (a,x-1)   

module Ficha7 where 

import Data.Char 
import Data.List
import Data.Maybe 


--ex1)
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

--a)
calcula :: ExpInt -> Int 
calcula (Const x) = x 
calcula (Simetrico x) = (- (calcula x))
calcula (Mais x y) = (calcula x)+(calcula y)
calcula (Menos x y) = (calcula x)-(calcula y)
calcula (Mult x y) = (calcula x)*(calcula y)

--b)
infixa :: ExpInt -> String 
infixa (Const x) = show x
infixa (Simetrico x) = "(-" ++ infixa x ++ ")"
infixa (Mais x y) =  "(" ++ infixa x ++ "+" ++ infixa y ++ ")"
infixa (Menos x y) = "(" ++ infixa x ++ "-" ++ infixa y ++ ")"
infixa (Mult x y) = "(" ++ infixa x ++ "*" ++ infixa y ++ ")"

--c) 
posfixa :: ExpInt -> String
posfixa (Const x) = show x 
posfixa (Simetrico x) = '-' : (posfixa x)
posfixa (Mais x y) = (posfixa x) ++ " " ++ (posfixa y) ++ "+"
posfixa (Menos x y) = (posfixa x) ++ " " ++ (posfixa y) ++ "-"
posfixa (Mult x y) = (posfixa x) ++ " " ++ (posfixa y) ++ "*"


--ex 2 

--a)
soma :: Num a => RTree a -> a 
soma (R a []) = a
soma (R a l) = a + sum (map soma l)

--b)
altura :: RTree a -> Int
altura (R a []) = 1 
altura (R a l) = 1 + maximum (map altura l)

--c) 
prune :: Int -> RTree a -> RTree a 
prune x (R a []) = R a []
prune 0 (R a l) = R a []
prune x (R a l) = R a (map (prune(x-1)) l)

--d) 
mirror :: RTree a -> RTree a
mirror (R a []) = R a [] 
mirror (R a l) = R a (map (mirror) (reverse l))

--e)
postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a l) = (concatMap (postorder) l) ++ [a] 


--ex:3

data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

--a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork x xs) = ltSum x + ltSum xs 

--b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork x xs) = listaLT x ++ listaLT xs 

--c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1 
ltHeight (Fork x xs) = 1 + max (ltHeight x) (ltHeight xs)

--ex4:

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

--a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty,(Tip n))
splitFTree (No a e d) = ((Node a l1 r1), (Fork l2 r2))
                      where l1 = fst (splitFTree e)
                            r1 = fst (splitFTree d)
                            l2 = snd (splitFTree e)
                            r2 = snd (splitFTree d)

--b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node a e d) (Fork l r) = Just (No a fe fd)
                                  where Just fe = joinTrees e l
                                        Just fd = joinTrees d r 
joinTrees _ _ = Nothing 

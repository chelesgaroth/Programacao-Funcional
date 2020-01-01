module Ficha6 where 

import Data.Char 
import Data.List 

--ex1 

data BTree a = Empty
             | Node a (BTree a) (BTree a) 
           deriving Show

--a)
altura :: BTree a -> Int 
altura Empty = 0 
altura (Node a e d) =max (1 + altura (e)) (1 + altura (d))

--b)
contaNodos :: BTree a -> Int 
contaNodos Empty = 0 
contaNodos (Node a e d) = 1 + contaNodos(e) + contaNodos (d)

--c) 
folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a e d) = folhas (e) + folhas (d) 

--d) 
prune :: Int -> BTree a -> BTree a
prune x Empty = Empty 
prune 0 (Node a e d) = Empty
prune x (Node a e d) = (Node a (prune (x-1) e) (prune (x-1) d))

--e) 
path :: [Bool] -> BTree a -> [a] 
path l Empty = [] 
path [] (Node a e d) = [a]
path (x:xs) (Node a e d) | (x== False) = a : (path xs e)
                         | otherwise = a : (path xs d) 

--f)
mirror:: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node a e d) = Node a (mirror d) (mirror e)

--g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node a e d) (Node c l r) =  Node (f a c) (zipWithBT f e l) (zipWithBT f d r)
zipWithBT f _ _ = Empty 

--h) 
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT 

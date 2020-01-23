module Ficha6 where 

import Data.Char 
import Data.List
import Data.Maybe 

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
unzipBT (Node (a,b,c) e d) = ((Node a e1 d1),(Node b e2 d2),(Node c e3 d3))
                            where (e1,e2,e3) = unzipBT e 
                                  (d1,d2,d3) = unzipBT d

{- ex 2 (Árvores Binárias de Procura)
os números menores que a raíz encontram-se no lado esquerdo
da árvore, os maiores encomtram-se no lado direito -}

--a)
minimo :: Ord a => BTree a -> a 
minimo (Node a Empty d) = a 
minimo (Node a e d) = minimo e 

--b)
semMinimo :: Ord a => BTree a -> BTree a 
semMinimo (Node a Empty d) = Empty
semMinimo (Node a e d) = Node a (semMinimo e) d 

--c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node a Empty d) = (a,d)
minSmin (Node a e d) = (c,(Node a l d))
                     where (c,l) = minSmin e 

--d)
{- Quando queremos remover a raíz, depois colocamos
ou o menor elemento do lado direito ou o maior elemento
do lado esquerdo -}
remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x t@(Node a e d) | x < a  = (Node a (remove x e) d) 
                        | x > a  = (Node a e (remove x d))
                        | otherwise = removeRaiz t

removeRaiz :: Ord a => BTree a -> BTree a 
removeRaiz Empty = Empty
removeRaiz (Node a Empty d) = d
removeRaiz (Node a e Empty) = e 
removeRaiz (Node a e d) = (Node (maior e) (semMaior e) d)

maior :: Ord a => BTree a -> a
maior (Node a e Empty) = a
maior (Node a e d) = maior d

semMaior :: Ord a => BTree a -> BTree a  
semMaior Empty = Empty
semMaior (Node a e Empty) = e
semMaior (Node a e d) = Node a e (semMaior d)                                    

--ex3)

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
    deriving Show
type Turma = BTree Aluno 

--árvore binária de procura (ordenada por número)

--a)
inscNum :: Numero -> Turma -> Bool 
inscNum x Empty = False
inscNum x (Node (num,nom,reg,cla) e d) | x == num = True 
                                       | x > num = inscNum x d 
                                       | otherwise = inscNum x e 

--b)
inscNome :: Nome -> Turma -> Bool
inscNome (x:xs) Empty = False
inscNome l (Node (num,nom,reg,cla) e d) | l == nom = True
                                        | otherwise = inscNome l e && inscNome l d 

--c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,reg,cla) e d) = (case reg of TE -> [(num,nom)];otherwise -> []) ++ trabEst e ++ trabEst d

--d)
nota :: Numero -> Turma -> Maybe Classificacao
nota x Empty = Nothing 
nota x t@(Node (num,nom,reg,cla) e d) | x > num = nota x d 
                                      | x < num = nota x e 
                                      | otherwise = Just cla 

--e)
percFaltas :: Turma -> Float 
percFaltas t@(Node (num,nom,reg,cla) e d) = ((/) (numT t) (totalT t)) * 100

numT :: Turma -> Float
numT Empty = 0 
numT (Node (num,nom,reg,Faltou) e d) = 1 + numT e + numT d 
numT (Node (num,nom,reg,_) e d) = numT e + numT d 

totalT :: Turma -> Float
totalT Empty = 0
totalT (Node (num,nom,reg,cla) e d) = 1 + totalT e + totalT d 

--f) 
mediaAprov :: Turma -> Float
mediaAprov t@(Node (num,nom,reg,cla) e d) = ((/) (aproV t) (aprovados t)) * 100

aproV :: Turma -> Float
aproV Empty = 0 
aproV (Node (num,nom,reg,Aprov nota) e d) = fromIntegral nota + aproV e + aproV d
aproV (Node (num,nom,reg,cla) e d) = aproV e + aproV d 

aprovados :: Turma -> Float
aprovados Empty = 0 
aprovados (Node (num,nom,reg,Aprov nota) e d) = 1 + aprovados e + aprovados d 
aprovados (Node (num,nom,reg,cla) e d) = aprovados e + aprovados d 

--g)
aprovAv :: Turma -> Float 
aprovAv Empty = 0
aprovAv t = ((/) (passou t) (avaliado t)) * 100 

passou :: Turma -> Float
passou Empty = 0
passou (Node (num,nom,reg,Aprov nota) e d) |((fromIntegral nota) >= 9.5) = 1 + passou e + passou d
                                           | otherwise = passou e + passou d
passou (Node (num,nom,reg,cla) e d) = passou e + passou d 

avaliado :: Turma -> Float
avaliado Empty = 0 
avaliado (Node (num,nom,reg,Aprov nota) e d) = 1 + avaliado e + avaliado d 
avaliado (Node (num,nom,reg,cla) e d) = avaliado e + avaliado d 

             

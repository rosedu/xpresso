{- Priority Queue
 - Taken from haskell wiki
 - -}
module PriorityQueue (
    PriorityQueue
    , singleton
    , fromList
    , empty
    , deleteFindMin
    , findMin
    , deleteMin
    , insert
    , union
    )where

import Data.List (foldl')

data PriorityQueue k a =
    Nil
    | Branch k a (PriorityQueue k a) (PriorityQueue k a)

singleton :: Ord k => k -> a -> PriorityQueue k a
singleton k a = Branch k a Nil Nil

fromList :: Ord k => [(k,a)] -> PriorityQueue k a
fromList = foldl' (\q (k,a) -> (Branch k a Nil Nil) `union` q) Nil
 
empty :: Ord k => PriorityQueue k a -> Bool
empty Nil = True
empty _   = False
 
deleteFindMin :: Ord k => PriorityQueue k a -> Maybe ((k,a), PriorityQueue k a)
deleteFindMin Nil = Nothing
deleteFindMin (Branch k a l r) = Just ((k,a), union l r)
 
deleteMin :: Ord k => PriorityQueue k a -> Maybe (PriorityQueue k a)
deleteMin h = fmap snd (deleteFindMin h)
 
findMin :: Ord k => PriorityQueue k a -> Maybe (k, a)
findMin h = fmap fst (deleteFindMin h)
 
insert :: Ord k => k -> a -> PriorityQueue k a -> PriorityQueue k a
insert k a h = union (Branch k a Nil Nil) h
 
union :: Ord k => PriorityQueue k a -> PriorityQueue k a -> PriorityQueue k a
union l Nil = l
union Nil r = r
union l@(Branch kl _ _ _) r@(Branch kr _ _ _)
    | kl <= kr  = link l r
    | otherwise = link r l

link:: Ord k => PriorityQueue k a -> PriorityQueue k a -> PriorityQueue k a
link (Branch k a Nil m) r = Branch k a r m
link (Branch k a ll lr) r = Branch k a Nil (union (union r ll) lr)

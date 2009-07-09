{- Quine-McCluskey minimisation -}
module Quine where

-- Dunno if we need this.
import Data.List
import Parser 

data QuineToken = Care Bool | Dash deriving (Eq, Show)
type Implicant = [QuineToken]

cutZeros :: TruthTable -> [Implicant]
cutZeros [] = []
cutZeros ((ins,True):l) = (map Care ins) : (cutZeros l)
cutZeros (_ : l) = cutZeros l

compatibleImp :: Implicant -> Implicant -> Bool
compatibleImp [] [] = True
compatibleImp (Dash:lx) (y:ly)
	| y == Dash = compatibleImp lx ly
	| otherwise = False
compatibleImp (x:lx) (Dash:ly) 
	| x == Dash = compatibleImp lx ly
	| otherwise = False
compatibleImp (x:lx) (y:ly) = compatibleImp lx ly

genImp :: Implicant -> Implicant -> Implicant
genImp [] [] = []
genImp (x:lx) (y:ly) 
	| x /= y = Dash : (genImp lx ly)
	| otherwise = x : (genImp lx ly)

reductibleImp :: Implicant -> Implicant -> Bool
reductibleImp [] [] = True
reductibleImp (x:lx) (y:ly) = if x==y then (reductibleImp lx ly)
			else lx == ly

getFirstOrder :: [Implicant] -> [([Implicant], Int)]
getFirstOrder = (folding []). (sortBy sorting) . (map mapping)
    where
	mapping element = (element, length.(filter (== (Care True))) $ element)
	sorting i1 i2
	    | (snd i1) < (snd i2) = LT
	    | (snd i1) > (snd i2) = GT
	    | otherwise = EQ
	folding (x:_) (y:ys)
	    | (snd y) == (snd x) = folding [(((fst y):(fst x)), (snd y))] ys
	    | otherwise = [x] ++ (folding [([(fst y)], (snd y))] ys) 
	folding [] (y:ys) = folding [([(fst y)], (snd y))] ys
	folding list [] = list

{- test: *Quine> getNextOrder $ getNextOrder $ getFirstOrder $ cutZeros $ makeTableFromExpr $ Maybe.fromJust $ play "a+b'*c+c*a" -}
getNextOrder :: [([Implicant],Int)] -> [([Implicant], Int)]
getNextOrder (x:[]) = [x]
getNextOrder ( (imp1,ord1) : (imp2, ord2) : rest )
	| ord2 /= (ord1+1) = ((imp1,ord1) : (getNextOrder ((imp2, ord2):rest)))
	| otherwise = (imp, ord1) : (getNextOrder ((imp2, ord2):rest))
		where imp = reduce imp1 imp2 []
	
reduce :: [Implicant] -> [Implicant] -> [Implicant] -> [Implicant]
reduce [] imp2 imp = imp
reduce (i:imp1) imp2 imp = if (null matches) then reduce imp1 imp2 (i:imp)
			else reduce imp1 imp2 (union imp (map (genImp i) matches))
    where 
    	matches = filter f imp2
	f	= (\x -> ((reductibleImp i x) && (compatibleImp i x))) 
			

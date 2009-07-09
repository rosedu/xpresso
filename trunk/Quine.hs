{- Quine-McCluskey minimisation -}
module Quine where

-- Dunno if we need this.
import Data.List
import Parser 

{- QuineToken = True, False or Dash -}
data QuineToken = Care Bool | Dash deriving (Eq, Show)
type Implicant = [QuineToken]

{- eliminates False rows from a TruthTable -}
cutZeros :: TruthTable -> [Implicant]
cutZeros [] = []
cutZeros ((ins,True):l) = (map Care ins) : (cutZeros l)
cutZeros (_ : l) = cutZeros l

{- checks to see if two implicants are reductible 
	(dashes in the same place and differ by one QuineToken) -}	
compatibleImp :: Implicant -> Implicant -> Bool
compatibleImp i x = sameDash i x && difByOne i x
    where
	sameDash [] [] = True
	sameDash (Dash:lx) (y:ly) = if y == Dash then sameDash lx ly else False
	sameDash (x:lx) (Dash:ly) = if x == Dash then sameDash lx ly else False
	sameDash (x:lx) (y:ly) = sameDash lx ly
	difByOne [] [] = True
	difByOne (x:lx) (y:ly) = if x == y then (difByOne lx ly) else lx == ly

{- reduces two implicants and returns the result -}
genImp :: Implicant -> Implicant -> Implicant
genImp [] [] = []
genImp (x:lx) (y:ly) 
	| x /= y = Dash : (genImp lx ly)
	| otherwise = x : (genImp lx ly)

{- sorts a list of implicants 
	according to the number of True QuineTokens in each -}
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

{- computes all possible combinations of implicants from one size to the next -}
getNextOrder :: [([Implicant],Int)] -> [([Implicant], Int)]
getNextOrder (x:[]) = [x]
getNextOrder ( (imp1,ord1) : (imp2, ord2) : rest )
	| ord2 /= (ord1+1) = ((imp1,ord1) : (getNextOrder ((imp2, ord2):rest)))
	| otherwise = (imp, ord1) : (getNextOrder ((imp2, ord2):rest))
	where 
	    imp = reduce imp1 imp2 []
  	    reduce [] imp2 imp = imp
	    reduce (i:imp1) imp2 imp = 
	        if (null (matches i)) then reduce imp1 imp2 (i:imp)
	        else reduce imp1 imp2 (union imp (map (genImp i) (matches i)))
	    matches i = filter (compatibleImp i) imp2

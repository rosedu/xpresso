{- Quine-McCluskey minimisation -}
module Quine where

-- Dunno if we need this.
import Data.List
import Data.Maybe
import ExprParser (TruthTable)

{- QuineToken = True, False or Dash -}
data QuineToken = Care Bool | Dash deriving Eq
type Implicant = [QuineToken]

instance Ord QuineToken where
    x <= y = x == y || (x == Care False && y == Care True) || (x == Dash)

instance Show QuineToken where
    show token 
    	| token == Care True = "1"
    	| token == Care False = "0"
    	| token == Dash = "-"

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

{- computes all possible combinations of implicants from one size to the next -}
getNextOrder :: [([Implicant],Int)] -> [([Implicant], Int)]
getNextOrder (x:[]) = [x]
getNextOrder ( (imp1,ord1) : (imp2, ord2) : rest )
    | ord2 /= (ord1+1) = ((imp1,ord1) : (getNextOrder ((imp2, ord2):rest)))
    | otherwise = (imp, ord1) : (getNextOrder ((imp2, ord2):rest))
    where 
        imp = sort (reduce imp1 imp2 [])
	reduce [] imp2 imp = imp
	reduce (i:imp1) imp2 imp = 
            if (null (matches i)) then reduce imp1 imp2 (i:imp)
            else reduce imp1 imp2 (union imp (map (genImp i) (matches i)))
	matches i = filter (compatibleImp i) imp2

{- given a list of implicants, it repeatedly applies the Quine-McCluskey
	reduction until no more changes occur -}
getPrimeImplicants :: [Implicant] -> [Implicant]
getPrimeImplicants impList = concat $ map fst $ 
			apply (reduce.getNextOrder) (getFirstOrder impList)
    where
    	apply f x = if (x == (f x)) then (f x) else apply f (f x)
	reduce (imp:[]) = [imp]
	reduce ((imp1,ord1):(imp2,ord2):rest)
	    | ord2 /= (ord1 + 1) = (imp1,ord1):(reduce ((imp2,ord2):rest))
	    | otherwise		 = (imp1,ord1):(reduce ((imp,ord2):rest))
	    where
	        {- imp = implicants from imp2 that aren't included in
			any implicant from imp1 -}
	    	imp = filter (\x -> length (filter (derived x) imp1) == 0) imp2

		{- checks to see if implicant x includes implicant y -}
		derived [] [] = True
		derived (x:lx) (y:ly) 
		    | x==y = derived lx ly
		    | x/=Dash && y==Dash = lx == ly
		    | otherwise = False
		derived _ _ = False

{- returns the numbers of the minterms that an implicant stands for -}
impNumbers :: Implicant -> [Int]
impNumbers [] = [0]
impNumbers imp = case (last imp) of
		    Care True -> caseTrue
		    Care False -> caseFalse
		    Dash -> merge caseFalse caseTrue
	where
	    caseTrue = map addPlus (impNumbers (init imp))
	    caseFalse = map (2*) (impNumbers (init imp))
	    addPlus x = 1+2*x
	    merge [] ly = ly 
	    merge lx [] = lx 
	    merge (x:lx) (y:ly) = if x<y then x:(merge lx (y:ly))
	    				else y:(merge (x:lx) ly)

{- from a given set of implicants defining a logical function, finds 
	minimal groups that provide complete coverage of its minterms -}
petrick :: [Implicant] -> [[Implicant]]
petrick impList = if (length essentialImps == length impList) 
			then [essentialImps]
    else map ((++) essentialImps) (multiply otherSums)
    where
	sums imps = map (selectImps imps) (nub (concat (map impNumbers imps)))
	otherSums = map (selectImps otherImps) impNum
	impNum = (nub (concat (map impNumbers impList))) \\ 
				(nub (concat (map impNumbers essentialImps)))
	selectImps imps x = filter (elem x . impNumbers) imps
    	essentialImps = nub $ concat $ filter (\x -> length x==1) (sums impList)
	otherImps = impList \\ essentialImps

{- multiplies the product of sums into the sum of products and 
	performs the X+XY = X reduction -}
multiply :: [[Implicant]] -> [[Implicant]]
multiply impList = reduce $ nub $ myfoldl folding [] impList
    where
        {- performs the X+XY = X reduction step -}
	reduce list = filter (\x -> length (filter (includes x) list ) ==1) list

	{- multiplies two sums of implicants into a sum of products -}
	{- this function is folded over the product of sums in order to
		obtain the sum of products in step 4 of Petrick's method -}
	folding [] list = map ( :[]) list
	folding list [] = [] 
	folding lx (y:ly) = union (map (sort.union [y]) lx) (folding lx ly)

	{- verifies if an implicant contains another -}
	includes lx [] = True
	includes lx (y:ly) = elem y lx && includes lx ly  

	{- foldl function which also reduces the accumulator on every step -}
	myfoldl f acc [] = acc
	myfoldl f acc (x:lx) = myfoldl f (reduce (f acc x)) lx

{- number of p-input gates required for a k-term conjunction -}
gateNo :: Int -> Int -> Int
gateNo p k 
	| k <= p = 1
	| k `mod` p == 0 = (k `div` p) + gateNo (k `div` p) p 
	| k `mod` p <= 1 = (k `div` p) + gateNo (k `div` p + 1) p 
	| otherwise = (k `div` p + 1) + gateNo (k `div` p) p

{- chooses the min-gate implicant for an AND-OR implementation -}
minAndOr :: [[Implicant]] -> Int -> Int ->  [Implicant]
minAndOr impList pAnd pOr = pickMin impList (dimAndOr pAnd pOr)
    where 
	dimAndOr pAnd pOr imp = sum (map dimImp imp) + gateNo pOr (length imp)
	dimImp = gateNo pAnd . length . filter (/= Dash)

{- chooses the min-gate implicant for a NAND expression -}
minNand impList pNand = minAndOr impList pNand pNand
{- chooses the min-gate implicant for a NOR expression -}
minNor impList pNor = minAndOr impList pNor pNor

{- picks the min-value element from a list according to a val function -}
pickMin :: (Num b, Ord b) => [a] -> (a -> b) -> a
pickMin (x:[]) val = x
pickMin (x:lx) val = if (val x) < (val min) then x else min
	where min = pickMin lx val

{- from a TruthTable, finds groups of implicants with complete coverage -}
getMinImps :: TruthTable -> [[Implicant]]
getMinImps table =  petrick $ getPrimeImplicants $ 
	cutZeros $ table 

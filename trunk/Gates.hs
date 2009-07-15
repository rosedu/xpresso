module Gates where

import Parser
import Quine
import IDraw

import List
import Maybe
import Data.Map (keys)

data CircuitType = AndOr | Nand | Nor

data Options = Options {
    circType :: CircuitType,
    pAnd     :: Int,
    pOr      :: Int,
    pNand    :: Int,
    pNor     :: Int
}

{- gets the gates to implement an expression according to certain options -}
getGates :: String -> Options -> [Component]
getGates str opts = case circType opts of
		AndOr -> prettyNames $ getOrGate opts vars $ 
			(\x -> minAndOr  x (pAnd opts) (pNor opts)) 
			$ getMinImps $ str
 		Nand -> prettyNames $ getNandGate opts vars $ 
			(\x->minAndOr x inf inf) $ getMinImps $ str
		Nor -> prettyNames $ getNorGate opts vars $ 
			(\x->minAndOr x inf inf) $ getMinImps $ str

   where	 
   	inf  = floor (1/0) 
	expr = fromJust $ play str
	vars = keys $ getVars expr

{- given a list of variable names and a list of implicants, returns the
	corresponding OR gate plus an AND gate for each implicant -}
getOrGate :: Options -> [String] -> [Implicant] -> [Component]
getOrGate opts vars imps = orGate ++ (concat andGates)
    where
    	andGates = map (splitGate (pAnd opts))
				$ concat $ map (getAndGate vars) imps 
	lastGates = map last andGates
	singleImps = map (fst.head.filter (\x -> snd x == Care True)) 
			$ map (zip vars) $ filter singleImp imps
	singleImp imp = length (filter (== Care True) imp) == 1 &&
			length (filter (== Care False) imp) == 0
	orGate = if (length imps <= 1) then [] else splitGate (pOr opts) 
	    	(Component "OR" ((map (head.cOutputs) lastGates) ++ singleImps) 			[name])
	name = tail $ concat $ map (\x -> "+"++show x) imps

{- given a list of variable names and an implicant, returns the corresponding
	AND gate and NOT gates if needed -}
getAndGate :: [String] -> Implicant ->  [Component]
getAndGate vars imp = andGate ++ notGates 
    where
    	notGates = map fromJust $ filter isJust $ map getNotGate varPairs 
	andGate = if (length (filter (/= Dash) imp) <= 1 ) then [] else
	    [Component "AND" (filter (/= []) (map getName varPairs)) [name]]
	name = concat (map show imp)
	varPairs = zip imp vars
	getName (token, var) 
	    | token == Dash = []
	    | token == Care True = var
	    | token == Care False = var++"'"

getNorGate :: Options -> [String] -> [Implicant] -> [Component]
getNorGate opts vars imps = map transform motherGate
    where
    	transform gate = if (cType gate /= "NAND") then gate else
		Component "NOR" (cInputs gate) (cOutputs gate) 
    	motherGate = if notNegated == [] then [] else
		if (cType.head) notNegated == "NAND" then 
		(Component "NOT" [(head.cOutputs.head) notNegated] [name])
			:notNegated
		else notNegated
	name = "(" ++ ((head.cOutputs.head) notNegated) ++ ")'"
    	notNegated = if length imps == 1&&(length.filter (/=Dash).head) imps==1 
		then getNandGate opts vars imps else
			getNandGate opts vars (map negateImp imps)
	negateImp [] = [] 
	negateImp (x:lx) = case x of 
		Care True -> Care False 
		Care False-> Care True
		Dash	  -> Dash
		: (negateImp lx)

getNandGate :: Options -> [String] -> [Implicant] -> [Component]
getNandGate opts vars imps = if length childNands == 1 then 
	    if (length.filter (/=Dash).head) imps == 1 then 
	    getAndGate vars (head imps)
	    else
   	    Component  "NOT" (map (head.cOutputs) lastGates) ["("++name++")'"] 
	    			: (concat childNands)
	    else
 	    Component "NAND" (map (head.cOutputs) lastGates ++ singleImps) 
	    		["("++name++")'"] : (concat childNands)
    where
    	childNands = map (getChildNand vars) imps
	singleImps = map (fst.head.filter (\x -> snd x == Care False)) 
			$ map (zip vars) $ filter singleImp imps
	singleImp imp = length (filter (== Care False) imp) == 1 &&
			length (filter (== Care True) imp) == 0
	name = tail $ concat $ map (\x -> "+"++show x) imps
	lastGates = map last (filter (/=[]) childNands)

getChildNand :: [String] -> Implicant -> [Component]
getChildNand vars imp = if (length (filter (/=Dash) imp) <= 1 ) then singleNot
			else notGates ++ nandGate
    where
	nandGate = [Component "NAND" (filter ( /= []) (map getName varPairs)) 
			[varName]]
   	notGates = map fromJust $ filter isJust $ map getNotGate varPairs 
	singleNot = if fst singleImp == Care True then 
		[(fromJust.getNotGate) (Care False, snd singleImp)] else []
	singleImp = head (filter (\(x,y) -> x /= Dash) varPairs)
	varName = "(" ++ (concat (map show imp)) ++ ")'"
	varPairs = zip imp vars
	getName (token, var) 
	    | token == Dash = []
	    | token == Care True = var
	    | token == Care False = var++"'"

{- if the QuineToken given is negated, it returns a corresponding NOT gate -}
getNotGate :: (QuineToken, String) -> Maybe Component
getNotGate (token, var) = if token == Care False then 
		Just (Component "NOT" [var] [var++"'"])
		else Nothing

splitGate :: Int -> Component -> [Component]
splitGate p gate = if p >= length (cInputs gate) then [gate] else
	childGates ++ splitGate p motherGate
    where
    	tempGates = map makeGate $ zip3 (repeat (cType gate)) 
		      			(splitVars p (cInputs gate))
					(splitName ((head.cOutputs) gate))
	childGates = if (length . cInputs . last) tempGates == 1
		then init tempGates else tempGates
	residualInput = if (length . cInputs . last) tempGates /= 1 then [] 
		else (cInputs . last) tempGates
 	makeGate = \(x,y,z) -> (Component x y z)
	motherGate = makeGate (cType gate, 
		(map (head.cOutputs) childGates) ++ residualInput, [newName])
	newName = ((head.cOutputs) gate) ++"-"++ (show . length . cInputs) gate
	splitName name = map (\x -> [name++"_"++show x]) [1..] 
	splitVars p list = if length list <= p then [list] else
		(take p list):(splitVars p (drop p list))

prettyNames :: [Component] -> [Component]
prettyNames list = map replace list -- replace nameMap list
    where
    	replace (Component name inputs outputs) = Component name
		(map (search nameMap) inputs) (map (search nameMap) outputs) 
	search [] str = str
	search (x:rest) str = if fst x == str then snd x else search rest str
    	nameMap = zip (longNames list) $ map ("w"++) $ map show [1..]
	longNames [] = [] 
	longNames (x:rest) = union (filter long (cInputs x)) $ 
			union (filter long (cOutputs x)) (longNames rest)
	long x = length x > 5 

edgeMap :: [Component] -> [(Component, Component)]
edgeMap list = nub $ concat $ map (gateEdges) list
    where
    	gateEdges x = zip (repeat x) $ filter (commonEdges x) list
	commonEdges c1 c2 = not $ null $ intersect (cOutputs c1) (cInputs c2)

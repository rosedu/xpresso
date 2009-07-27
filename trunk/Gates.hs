module Gates where

import ExprParser
import Quine
import Defs 

import Data.List
import Data.Maybe
import Data.Map (keys)

{- Given a logical expression and some minimization restrictions, returns
	a list of components that implement the minimized expression -}
getGatesFromExpr :: String -> Options -> [Component]
getGatesFromExpr str opts = countInputs $ case circType opts of
		AsIs  -> countInputs $ getGatesAsIs $ fromJust $ play str
		AndOr -> prettyNames $ getOrGate opts vars $ 
			(\x -> minAndOr  x (pAnd opts) (pNor opts)) 
			$ getMinImps $ table
 		Nand  -> prettyNames $ getNandGate opts vars $ 
			(\x->minAndOr x inf inf) $ getMinImps $ table
		Nor   -> prettyNames $ getNorGate opts vars $ 
			(\x->minAndOr x inf inf) $ getMinImps $ table

   where	 
   	inf  = floor (1/0) 
	expr = fromJust $ play str
	vars = keys $ getVars expr
	table = makeTableFromExpr $ fromJust $ play str

getGatesFromTable :: [String] -> TruthTable -> Options -> [Component]
getGatesFromTable vars table opts = countInputs $ case circType opts of
		AndOr -> prettyNames $ getOrGate opts vars $ 
			(\x -> minAndOr  x (pAnd opts) (pNor opts)) 
			$ getMinImps $ table
 		Nand  -> prettyNames $ getNandGate opts vars $ 
			(\x->minAndOr x inf inf) $ getMinImps $ table
		Nor   -> prettyNames $ getNorGate opts vars $ 
			(\x->minAndOr x inf inf) $ getMinImps $ table

   where	 
   	inf  = floor (1/0) 

{- Given a list of variable names and a list of implicants, returns the
	AndOr circuit implementation (an or mothergate and and childgates) -}
getOrGate :: Options -> [String] -> [Implicant] -> [Component]
getOrGate opts vars imps = (concat notGates) ++ (concat lastGates) ++ orGate
    where
    	andGates = map (getAndGate vars ) imps
	notGates = map init $ filter (/=[]) andGates
	lastGates = map (splitGate (pAnd opts) . last) $ filter (/=[]) andGates
	singleImps = map (fst.head.filter (\x -> snd x == Care True)) 
			$ map (zip vars) $ filter singleImp imps
	singleImp imp = length (filter (== Care True) imp) == 1 &&
			length (filter (== Care False) imp) == 0
	orGate = if (length imps <= 1) then [] else splitGate (pOr opts) 
	    	(Component "or" ((map (head.cOutputs.last) lastGates) ++ singleImps) 
		[name])
	name = tail $ concat $ map (\x -> "+"++show x) imps

{- Given a list of variable names and an implicant, returns the corresponding
	and gate and any NOT gates that are needed -}
getAndGate :: [String] -> Implicant ->  [Component]
getAndGate vars imp = notGates ++ andGate
    where
    	notGates = map fromJust $ filter isJust $ map getNotGate varPairs 
	andGate = if (length (filter (/= Dash) imp) <= 1 ) then [] else
	    [Component "and" (filter (/= []) (map getName varPairs)) [name]]
	name = concat (map show imp)
	varPairs = zip imp vars
	getName (token, var) 
	    | token == Dash = []
	    | token == Care True = var
	    | token == Care False = var++"'"

{- Implements the circuit given by the list of implicants using 
	Nor and NOT gates -}
getNorGate :: Options -> [String] -> [Implicant] -> [Component]
getNorGate opts vars imps = map transform motherGate
    where
    	transform gate = if (cType gate /= "nand") then gate else
		Component "nor" (cInputs gate) (cOutputs gate) 
    	motherGate = if notNegated == [] then [] else
		if (cType.head) notNegated == "nand" then 
		(Component "not" [(head.cOutputs.head) notNegated] [name])
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
		:(negateImp lx)

{- Implements the circuit given by the list of implicants using
	Nand and NOT gates -}
getNandGate :: Options -> [String] -> [Implicant] -> [Component]
getNandGate opts vars imps = if length childNands == 1 then 
	if (length.filter (/=Dash).head) imps == 1 then 
	getAndGate vars (head imps)
	else
   	Component  "not" (map (head.cOutputs) lastGates) ["("++name++")'"] 
		: (concat childNands)
	else
 	Component "nand" (map (head.cOutputs) lastGates ++ singleImps) 
		["("++name++")'"] : (concat childNands)
    where
    	childNands = map (getChildNand vars) imps
	singleImps = map (fst.head.filter (\x -> snd x == Care False)) 
		$ map (zip vars) $ filter singleImp imps
	singleImp imp = length (filter (== Care False) imp) == 1 &&
		length (filter (== Care True) imp) == 0
	name = tail $ concat $ map (\x -> "+"++show x) imps
	lastGates = map last (filter (/=[]) childNands)

{- Returns the Nand gate implementation of a single implicant, to be used
	by the getNandGate function -}
getChildNand :: [String] -> Implicant -> [Component]
getChildNand vars imp = if (length (filter (/=Dash) imp) <= 1 ) then singleNot
		else notGates ++ nandGate
    where
	nandGate = [Component "nand" (filter ( /= []) (map getName varPairs)) 
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
	Just (Component "not" [var] [var++"'"])	else Nothing

{- given a logical gate that implements an associative function, it returns 
	an implementation of the function using gates of the same type but with 
	maximum p inputs -}
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

{- Replaces port names longer than 5 characters with "w1", "w2", etc -}
prettyNames :: [Component] -> [Component]
prettyNames list = map replace list
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

{- Given a list of components, it returns a list of pairs (c1, c2), 
	where at least one output of c1 is an input of c2 (needed by MM) -}
edgeMap :: [Component] -> [(Component, Component)]
edgeMap list = nub $ concat $ map (gateEdges) list
    where
    	gateEdges x = zip (repeat x) $ filter (commonEdges x) list
	commonEdges c1 c2 = not $ null $ intersect (cOutputs c1) (cInputs c2)

{- returns the pseudo-Verilog description of a component list -}
getVerilog :: [Component] -> String
getVerilog compList = concat $ map compString compList
    where
    	compString comp = (cType comp) ++ " " ++ 
			((concat . map (++ " ")) (cInputs comp)) ++ " " ++
			((concat . map (++ " ")) (cOutputs comp)) ++ "\n"

{- returns a list of components that implement a non-minimized expression -}
getGatesAsIs :: Expr -> [Component]
getGatesAsIs (Uno Not a) = Component "not" [name] [getName a ++ "'"] : child
    where 
    	child = getGatesAsIs a
	name = if child == [] then getName a else (head.cOutputs.head) child

getGatesAsIs (Duo op a b) = reverse $ mergeGates $ Component opName 
	[name_a, name_b] [getName (Duo op a b)]:(child_a ++ child_b)
    where 
    	child_a = getGatesAsIs a
	child_b = getGatesAsIs b
	name_a = if child_a == [] then getName a 
		else (head.cOutputs.head) child_a
	name_b = if child_b == [] then getName b
		else (head.cOutputs.head) child_b
	opName = case op of
		And -> "and"
		Or -> "or"
		Xor -> "xor"
getGatesAsIs _ = []

mergeGates :: [Component] -> [Component]
mergeGates list = nextStep --if (union list nextStep == list) 
	--then nextStep else mergeGates nextStep
    where nextStep = mergeGatesAux list []  

mergeGatesAux :: [Component] -> [Component] -> [Component]
mergeGatesAux [] result = result
mergeGatesAux (x:rest) result = if null matches 
	then mergeGatesAux rest (x:result)
	else mergeGates $ result ++ (rest \\ [head matches]) 
		++ [mergeGate x (head matches)]
    where
	matches = filter (isMergeable x) rest
	isMergeable (Component t1 i1 o1) (Component t2 i2 o2) =
		(elem (head o1) i2 || elem (head o2) i1) && t1 == t2
	mergeGate (Component t1 i1 o1) (Component t2 i2 o2)
		| elem (head o2) i1 = Component t2 (union i1 i2 \\ [head o2]) o1
		| elem (head o1) i2 = Component t1 (union i1 i2 \\ [head o1]) o2
	
{- returns a string representation of an expression -}
getName :: Expr -> String
getName (Var a) = a
getName (Uno Not a) = getName a ++ "'"
getName (Duo And a b) = getName a ++ "_and_" ++ (getName b)
getName (Duo Or a b) = getName a ++ "_or_" ++ (getName b)
getName (Duo Xor a b) = getName a ++ "_xor_" ++ (getName b)
getName x = show x

countInputs :: [Component] -> [Component]
countInputs list = map count list
    where count (Component ctype cins couts)
		| elem ctype ["or","and","nor","nand","xor","xnor"] = 
			Component newType cins couts
    		| otherwise = Component ctype cins couts
	    	where newType = ctype ++ (show $ length cins)

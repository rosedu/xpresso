module Gates where

import Parser
import Quine
import Maybe
import Data.Map (keys)

data GateLevel = GateLevel {
    glValue ::  String,
    glType  ::  String,
    glVars  ::  [String]
} deriving Show

data CircuitType = AndOr | Nand | Nor

data Options = Options {
    circType :: CircuitType,
    pAnd     :: Int,
    pOr      :: Int,
    pNand    :: Int,
    pNor     :: Int
}

{- gets the gates to implement an expression according to certain options -}
getGates :: String -> Options -> [GateLevel]
getGates str opts = case circType opts of
		AndOr -> getOrGate opts vars $ 
			(\x -> minAndOr  x (pAnd opts) (pNor opts)) 
			$ getMinImps $ str
    where	 
	expr = fromJust $ play str
	vars = keys $ getVars expr

{- given a list of variable names and a list of implicants, returns the
	corresponding OR gate plus an AND gate for each implicant -}
getOrGate :: Options -> [String] -> [Implicant] -> [GateLevel]
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
	    	(GateLevel name "OR" ((map glValue lastGates) ++ singleImps ))
	name = tail $ concat $ map (\x -> "+"++show x) imps

splitGate :: Int -> GateLevel -> [GateLevel]
splitGate p gate = if p >= length (glVars gate) then [gate] else
	childGates ++ splitGate p motherGate
    where
    	tempGates = map makeGate $ zip3 (splitName (glValue gate)) 
		      		 	(repeat (glType gate)) 
		      			(splitVars p (glVars gate))
	childGates = if (length . glVars . last) tempGates == 1
		then init tempGates else tempGates
	residualInput = if (length . glVars . last) tempGates /= 1 then [] 
		else (glVars . last) tempGates
 	makeGate = \(x,y,z) -> (GateLevel x y z)
	motherGate = makeGate (newName, glType gate, 
				(map glValue childGates) ++ residualInput)
	newName = (glValue gate) ++ "-" ++ (show . length . glVars) gate
	splitName name = map (\x -> name++"_"++show x) [1..] 
	splitVars p list = if length list <= p then [list] else
		(take p list):(splitVars p (drop p list))


{- given a list of variable names and an implicant, returns the corresponding
	AND gate and NOT gates if needed -}
getAndGate :: [String] -> Implicant ->  [GateLevel]
getAndGate vars t = andGate ++ notGates 
    where
    	notGates = map fromJust $ filter isJust $ 
	    map getNotGate varPairs 
	andGate = if (length (filter (/= Dash) t) <= 1 ) then [] else
		GateLevel varName "AND" 
		(filter (/= []) (map getName varPairs)) : []
	varName = concat (map show t)
	varPairs = zip t vars
	getName (token, var) 
	    | token == Dash = []
	    | token == Care True = var
	    | token == Care False = var++"'"

{- if the QuineToken given is negated, it returns a corresponding NOT gate -}
getNotGate :: (QuineToken, String) -> Maybe GateLevel
getNotGate (token, var) = if token == Care False then 
		Just (GateLevel (var++"'") "NOT" [var])
		else Nothing

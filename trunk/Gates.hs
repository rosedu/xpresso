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
		AndOr -> getOrGate vars $ 
			(\x -> minAndOr  x (pAnd opts) (pNor opts)) 
			$ getMinImps $ str
    where	 
	expr = fromJust $ play str
	vars = keys $ getVars expr

{- given a list of variable names and a list of implicants, returns the
	corresponding OR gate plus an AND gate for each implicant -}
getOrGate :: [String] -> [Implicant] -> [GateLevel]
getOrGate vars imps = orGate ++ andGates
    where
    	andGates = concat $ map (getAndGate vars) imps 
	orGate = if (length imps <= 1) then [] else
		(GateLevel name "OR" (map glValue andGates)):[]
	name = tail $ concat $ map (\x -> "+"++show x) imps

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

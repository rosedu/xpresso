module Structural where 

-- Imports
-- none yet :P

-- Type declarations
type Port = (PortType, [Var])
data PortType = Input | Output | Wire deriving (Eq, Show)
data Var = Var String deriving (Eq, Show)
type Statement = (String, [Var])

{- Brings the module String to a so-called „clean” form - no newline
	characters, only instructions separated by ';' -}
instrList :: String -> [String]
instrList s = lines s

{- Verifies that the given instruction is a port declaration -}
isPortDeclaration :: String -> Bool
isPortDeclaration s = (direction == "input") || (direction == "output")
	|| (direction == "wire")
	where direction = head $ words s

{- Gets all the ports from an instruction list -}
getPorts :: [String] -> [Port]
getPorts instrs = map mkport $ filter isPortDeclaration instrs
	where
	mkport s 
		| direction == "input" = (Input, map Var (tail atoms))
		| direction == "wire" = (Wire, map Var (tail atoms))
		| otherwise = (Output, map Var (tail atoms))
		where
		atoms = words s
		direction = head atoms


{- Gets the statements from an instruction list -}
getStatmnts :: [String] -> [Statement]
getStatmnts instrs = map mkstatmnt $ 
	filter (\ x -> not $ isPortDeclaration x) instrs where
	mkstatmnt s = (func, vars) where
		atoms = words s
		func = head atoms
		vars = map Var $ tail atoms


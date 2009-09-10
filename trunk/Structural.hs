module Structural where 

-- Imports
import Defs 
import Data.List

-- Type declarations
type Port = (PortType, [Var])
data PortType = Input | Output | Wire deriving (Eq, Show)
data Var = Var String | Const Bool deriving (Eq, Show)
type Statement = (String, [Var])

{- Brings the module String to a so-called „clean” form - no newline
    characters, only instructions separated by '\n' -}
instrList :: String -> [String]
instrList s = lines s

{- Verifies that the given instruction is a port declaration -}
isPortDeclaration :: String -> Bool
isPortDeclaration s = (direction == "input") || (direction == "output")
    || (direction == "wire")
    where direction = head $ words s

{- Gets all the ports from an instruction list -}
getPorts :: [String] -> [Port]
getPorts instrs = ins : outs : wires : []
    where
    ports = map mkport $ filter isPortDeclaration instrs
    ins = foldPorts Input $ filter (equalPort Input) ports
    outs = foldPorts Output $ filter (equalPort Output) ports
    wires = foldPorts Wire $ filter (equalPort Wire) ports    
    foldPorts tp l = foldr (\ (tp1, v1) (tp2, v2) -> (tp1, v1 ++ v2))
        (tp, []) l
    equalPort prt (tp, _) = tp == prt
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

statementToInstance :: Statement -> Component -> Component
statementToInstance  
    (_, vars) (Component name ins outs) = 
    Component name (snd spl) (fst spl) where
    spl = splitAt (length outs) $ map (\ (Var x) -> x) vars

{- We get a list of Components out of our list of Statements. In a
    perfect world, we can find a Component that matches the name
    that we need for our Statement. In a non-perfect world, we will use
    monads, but that'll happen later -}
{- Morphs Statements into Components -}
getComponentList :: [Statement] -> [Component] -> [Component]
getComponentList sts comps = 
    map (\ s -> statementToInstance s (lookupComp s comps)) sts
    where
    lookupComp (nm1, vars) ((Component nm2 ins outs) : rest)
        | nm1 == nm2 = Component nm2 ins outs
        | otherwise = lookupComp (nm1, vars) rest -- TODO: handle errors


-- Testing stuff
comps = [Component "xor2" ["in1","in2"] ["out"],
            Component "or2" ["in1","in2"] ["out"]]
st = "input a b\ninput cin\n \
       \ output s cout\nxor2 cout a b\nor2 s a b"
stats = getStatmnts $ instrList st
ports = getPorts $ instrList st

module IDraw where

import Data.List ((\\), intersect)

data Component = Component {
    cType :: String,
    cInputs :: [String],
    cOutputs :: [String]
    } deriving (Eq, Show)

type Node = Component
type Edge = (Component, Component)

nodes = [Component "AND" ["i", "w"] ["x"], 
      Component "BQ" ["xx", "c"] ["w", "Q'"],
      Component "ID" ["x"] ["xx"]]
edges = [(Component "AND" ["i", "w"] ["x"], Component "ID" ["x"] ["xx"]),
      (Component "ID" ["x"] ["xx"], Component "BQ" ["xx", "c"] ["w", "Q'"]),
      (Component "BQ" ["xx", "c"] ["w", "Q'"], Component "AND" ["i", "w"] ["x"])]

gateinputs = foldl (++) [] $ map cInputs nodes
gateoutputs = foldl (++) [] $ map cOutputs nodes
schemainputs = gateinputs \\ gateoutputs
schemaoutputs = gateoutputs \\ gateinputs
firstgates = filter f nodes
    where
	f x = [] /= schemainputs `intersect` cInputs x
lastgates = filter f nodes
    where
	f x = [] /= schemaoutputs `intersect` cOutputs x
schemawithoutlast = nodes \\ lastgates

striplevel :: [Component] -> [Component]
striplevel schema  = schema \\ last
    where
	last = filter f nodes
	f x = [] /= schemaoutputs `intersect` cOutputs x
	schemaoutputs = gateoutputs \\ gateinputs
	gateoutputs = foldl (++) [] $ map cOutputs schema
	gateinputs = foldl (++) [] $ map cInputs schema

getLevels :: [Component] -> [[Component]]
getLevels n = [head n] : (zipWith (\\) l $ tail l)
    where
	l = takeWhile (/= []) $ iterate striplevel n

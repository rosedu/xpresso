module IDraw where
{--}

import Data.List (
    (\\)
    , intersect
    , sortBy
    , findIndex
    )

{-Structural and combinational component-}
data Component = Component {
    cType :: String,
    cInputs :: [String],
    cOutputs :: [String]
    } deriving (Eq, Show)

{- Resulting SVG point -}
data SVGPoint = SVGPoint {
    svgX :: Int,
    svgY :: Int
    } deriving (Eq, Show)

{- Resulting SVG component port -}
data SVGPort = SVGPort {
    svgStart :: SVGPoint,
    svgStop :: SVGPoint,
    svgLabel :: String
    } deriving (Eq, Show)

{- Resulting SVG component:
 - svgcDefs === the SVG description of the component
 --}
data SVGComponent = SVGComponent {
    svgcType :: String,
    svgcPorts :: [SVGPort],
    svgcDefs :: String,
    svgcW :: Int,
    svgcH :: Int
    } deriving (Eq, Show)

{- Resulting SVG path between two omponents -}
data SVGPath = SVGPath {
    svgcControls :: [SVGPoint]
    } deriving (Eq, Show)

{- Localized instance of a SVGComponent -}
data SVGPComponent = SVGPComponent {
    svgpInstance :: SVGComponent,
    svgCorner :: SVGPoint
    } deriving (Eq, Show)

{- Mapping between wires and port names -}
data WireMap = WireMap {
    wireName :: String,
    portStart :: String,
    portEnd :: String
    } deriving (Eq, Show)

globalDELTAMIN = 10
globalLEVELWIREDELTA = 10

{-tobedeleted-}
nodes = [Component "AND" ["i", "w"] ["x"], 
      Component "BQ" ["xx", "c"] ["w", "Q'"],
      Component "ID" ["x"] ["xx"],
      Component "ID" ["t"] ["c"]]
edges = [(Component "AND" ["i", "w"] ["x"], Component "ID" ["x"] ["xx"]),
      (Component "ID" ["x"] ["xx"], Component "BQ" ["xx", "c"] ["w", "Q'"]),
      (Component "BQ" ["xx", "c"] ["w", "Q'"], Component "AND" ["i", "w"] ["x"]),
      (Component "ID" ["t"] ["c"], Component "BQ" ["xx", "c"] ["w", "Q'"])]
parsedSVGComponents = [
    SVGComponent "AND" [SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "A", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "B", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "X"] "nodefyet" 6 4,
    SVGComponent "ID" [SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "A", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "X"] "nodefyet" 8 6,
    SVGComponent "BQ" [SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "D", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "T", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "Q", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "Q'"] "nodefyet" 8 10,
    SVGComponent "MUX" [SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "A", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "B", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "C", SVGPort (SVGPoint 2 1) (SVGPoint 2 1) "X"] "nodefyet" 8 10]
{-untilhere-}

striplevel :: [Component] -> [Component]
striplevel schema  = schema \\ last
    where
	last = filter f nodes
	f x = [] /= schemaoutputs `intersect` cOutputs x
	schemaoutputs = gateoutputs \\ gateinputs
	gateoutputs = foldl (++) [] $ map cOutputs schema
	gateinputs = foldl (++) [] $ map cInputs schema

getLevels :: [Component] -> [[Component]]
getLevels [] = []
getLevels n = [head n] : (reverse . zipWith (\\) l $ tail l)
    where
	l = takeWhile (/= []) $ iterate striplevel n

getGlobalFanInofLevel :: [Component] -> Int
getGlobalFanInofLevel c = foldl (+) 0 $ map length $ map cInputs c

getGatesInaLevel :: [Component] -> Int
getGatesInaLevel c = length c

getUsedSVGComponents :: [Component] -> [SVGComponent] -> 
    [(Component, SVGComponent)]
getUsedSVGComponents [] svg = []
getUsedSVGComponents (c:cs) svg = l : (getUsedSVGComponents cs svg)
    where
	l = (c, head $ filter f svg)
	f x = svgcType x == cType c

getUsedSVGComponentsSorted :: [Component] -> [SVGComponent] ->
    [(Component, SVGComponent)]
getUsedSVGComponentsSorted c svgc = sortBy f $ getUsedSVGComponents c svgc
    where
	f (c1, _) (c2, _) = ic1 `compare` ic2
	    where
		levels = getLevels c
		ic1 = findIndex p1 levels
		ic2 = findIndex p2 levels
		p1 lvl = c1 `elem` lvl
		p2 lvl = c2 `elem` lvl

getLevelsOfUsedSVGComponents :: [Component] -> [SVGComponent] ->
    [[(Component, SVGComponent)]]
getLevelsOfUsedSVGComponents c svgc = map f levels
    where
	levels = getLevels c
	svglevels = getUsedSVGComponentsSorted c svgc
	f level = map g level
	g gateinlevel = (gateinlevel, snd . head $ filter p svglevels)
	    where
		p svgl = gateinlevel == fst svgl

getGateInputWireMapping :: Component -> SVGComponent -> [(String, String)]
getGateInputWireMapping c svg = zip (cInputs c) (map svgLabel $ svgcPorts svg)

getGateOutputWireMapping :: Component -> SVGComponent -> [(String, String)]
getGateOutputWireMapping c svg = reverse $ zip (reverse $ cOutputs c)
    (reverse $ map svgLabel $ svgcPorts svg)

placeComponents :: [[(Component, SVGComponent)]] -> ([SVGPComponent], [WireMap])
placeComponents lpairs = foldr f ([], []) $ map placeComponentsInLevel lpairs
    where
	f (s, w) (ss, ws) = (s++ss, w++ws)

placeComponentsInLevel :: [(Component, SVGComponent)] -> 
    ([SVGPComponent], [WireMap])
placeComponentsInLevel pairs = error . show $ (input, output)
    where
	input = map f pairs
	f x = getGateInputWireMapping (fst x) (snd x)
	output = map f' pairs
	f' x = getGateOutputWireMapping (fst x) (snd x)

--to be deleted
levels = getLevelsOfUsedSVGComponents nodes parsedSVGComponents
--until here}

--placeLevel :: [(Component, SVGComponent)] -> [SVGComponent]

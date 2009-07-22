module IDraw where

import Data.List (
    (\\)
    , intersect
    , sortBy
    , findIndex
    , unfoldr
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

globalDELTAMIN = 2
globalLEVELWIREDELTA = 2

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
    SVGComponent "AND" [SVGPort (SVGPoint 2 1) (SVGPoint 0 1) "A", SVGPort (SVGPoint 2 3) (SVGPoint 0 3) "B", SVGPort (SVGPoint 4 2) (SVGPoint 6 2) "X"] "nodefyet" 6 4,
    SVGComponent "ID" [SVGPort (SVGPoint 2 2) (SVGPoint 0 2) "A", SVGPort (SVGPoint 6 4) (SVGPoint 8 4) "X"] "nodefyet" 8 6,
    SVGComponent "BQ" [SVGPort (SVGPoint 2 2) (SVGPoint 0 2) "D", SVGPort (SVGPoint 2 8) (SVGPoint 0 8) "T", SVGPort (SVGPoint 6 2) (SVGPoint 8 2) "Q", SVGPort (SVGPoint 6 8) (SVGPoint 8 8) "Q'"] "nodefyet" 8 10,
    SVGComponent "MUX" [SVGPort (SVGPoint 2 2) (SVGPoint 0 2) "A", SVGPort (SVGPoint 2 6) (SVGPoint 0 6) "B", SVGPort (SVGPoint 2 8) (SVGPoint 0 8) "C", SVGPort (SVGPoint 6 6) (SVGPoint 8 6) "X"] "nodefyet" 8 10]
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

getGlobalFanInofLevel :: [(Component, SVGComponent)] -> Int
getGlobalFanInofLevel c = (foldl (+) 0) . (map length) . (map (cInputs.fst)) $ c

getGatesInaLevel :: [(Component, SVGComponent)] -> Int
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
placeComponents lpairs = (components, wires)
    where
	components = foldl (++) [] $ unfoldr placeLevels (0, lpairs)
	p x = True
	wires = []

placeLevels :: (Int, [[(Component, SVGComponent)]]) -> 
    Maybe ([SVGPComponent], (Int, [[(Component, SVGComponent)]]))
placeLevels (_, []) = Nothing
placeLevels (x, lvl:lvls) = Just (components, (y, lvls))
    where
	(y, components) = placeLevel x lvl

placeLevel :: Int -> [(Component, SVGComponent)] -> (Int, [SVGPComponent])
placeLevel x lvl = (y, comps)
    where
	fanin = getGlobalFanInofLevel lvl
	maxw = maximum $ map (svgcW.snd) lvl
	cx = x + fanin * globalLEVELWIREDELTA + globalDELTAMIN
	y = maxw + cx + globalDELTAMIN
	ycorners = scanl scanning 0 $ map (svgcH.snd) lvl
	scanning z x = z + x + globalDELTAMIN
	comps = map f $ zip (map snd lvl) ycorners
	f (svgc, y) = SVGPComponent nsvgc (SVGPoint cx y)
	    where
		nsvgc = svgc { svgcPorts = map g $ svgcPorts svgc }
	        g port = port { 
		    svgStart = SVGPoint (cx + (svgX . svgStart $ port))
					(y + (svgY . svgStart $ port)),
		    svgStop = SVGPoint (cx + (svgX . svgStop $ port)) 
					(y + (svgY . svgStop $ port))}

{--
placeComponentsInLevel :: Int -> [(Component, SVGComponent)] -> 
    ([SVGPComponent], [WireMap])
placeComponentsInLevel x pairs = error . show $ (input, output)
    where
	input = map f pairs
	f x = getGateInputWireMapping (fst x) (snd x)
	output = map f' pairs
	f' x = getGateOutputWireMapping (fst x) (snd x)--}

--to be deleted
levels = getLevelsOfUsedSVGComponents nodes parsedSVGComponents
--until here}

makeSVGFile :: [Component] -> [SVGPComponent] -> IO()
makeSVGFile nodes parsed = do
	let levels = getLevelsOfUsedSVGComponents nodes parsedSVGComponents
	let (components, wires) = placeComponents levels
	-- aici scrierea SVG --
	return ()

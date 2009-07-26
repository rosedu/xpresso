module SvgCircuit where

-- imports, some might be temporary
import Blackbox
import System.IO

-- data definitions, copied from IDraw 
-- TODO: move them somewhere else
{-Structural and combinational component-}
data Component = Component { cType :: String,
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

{- Returns a list of definitions in XML String format -}
defs :: [SVGComponent] -> [String]
defs svgcl = map svgcDefs svgcl

{- Returns a list of XML tags representing the component instances
	in the circuit -}
compInstances :: [SVGPComponent] -> [String]
compInstances pcomp = map mktag pcomp where
	mktag (SVGPComponent comp (SVGPoint x y)) =
		"<use xlink:href=\"#" ++ (svgcType comp) ++ "\" x=\"" ++
		(show x) ++ "\" y=\"" ++ (show y) ++ "\" />"

{- Returns a list of XML tags representing the path instances of the wires
	in the circuit -}
pathInstances :: [SVGPath] -> [String]
pathInstances path = map mktag path where
	mktag (SVGPath (p : rest)) = "<path d=\"" ++ (moveTo p) ++
		" " ++ (mkLines rest) ++ "\"\nstyle=\"stroke: rgb(0, 0, 0); \
		\ stroke-width: 2; stroke-linecap: butt; stroke-linejoin: miter; \
		\ stroke-opacity: 1; fill: none\" />"
	moveTo (SVGPoint x y) = "M" ++ (show x) ++ " " ++ (show y)
	mkline (SVGPoint x y) = "L" ++ (show x) ++ " " ++ (show y) ++ " "
	mkLines l = foldl (++) [] $ map mkline l

{- Return a list of strings representing the tags of our SVG -}
makeSVGTags :: [SVGComponent] -> [SVGPComponent] -> [SVGPath] -> [String]
makeSVGTags comps insts paths = [xmlhead, svghead, (fst defstags)] 
	++ (defs comps) ++ [snd defstags] ++ (compInstances insts)
	++ (pathInstances paths) ++ [snd svgtags]

makeSVGString :: [String] -> String
makeSVGString l = foldl (++) [] $ map (\ s -> s ++ "\n") l

{- Some test stuff -}
and2 = SVGComponent "and2" [SVGPort (SVGPoint 31 15) (SVGPoint 5 15)
	"in1", SVGPort (SVGPoint 31 35) (SVGPoint 5 35) "in2",
	SVGPort (SVGPoint 70 25) (SVGPoint 96 25) "out"]
	"<g id=\"and2\">\n \
    \ <path style=\"fill: none; stroke: rgb(0, 0, 0); stroke-width: 2; stroke-linecap: butt; stroke-linejoin: miter; stroke-opacity: 1;\" d=\"M 70,25 L 96,25\" id=\"out\"/>\n \ 
    \ <path style=\"fill: none; stroke: rgb(0, 0, 0); stroke-width: 2; stroke-linecap: butt; stroke-linejoin: miter; stroke-opacity: 1;\" d=\"M 31,15 5,15\" id=\"in1\"/> \n \
    \ <path style=\"fill: none; stroke: rgb(0, 0, 0); stroke-width: 2; stroke-linecap: butt; stroke-linejoin: miter; stroke-opacity: 1;\" d=\"M 31,35 5,35\" id=\"in2\"/>\n \
    \ <path style=\"overflow: visible; marker: none; font-size: medium; font-style: normal; font-variant: normal; font-weight: normal; font-stretch: normal; text-indent: 0pt; text-align: start; text-decoration: none; line-height: normal; letter-spacing: normal; word-spacing: normal; text-transform: none; direction: ltr; text-anchor: start; fill: rgb(0, 0, 0); fill-opacity: 1; stroke: none; stroke-width: 3; visibility: visible; display: inline; font-family: Bitstream Vera Sans;\" d=\"M 30,5 L 30,6.4285714 L 30,43.571429 L 30,45 L 31.428571,45 L 50.47619,45 C 61.744098,45 70.47619,35.999955 70.47619,25 C 70.47619,14.000045 61.744099,5.0000002 50.47619,5 C 50.47619,5 50.47619,5 31.428571,5 L 30,5 z M 32.857143,7.8571429 C 40.834264,7.8571429 45.918368,7.8571429 48.095238,7.8571429 C 49.285714,7.8571429 49.880952,7.8571429 50.178571,7.8571429 C 50.327381,7.8571429 50.409227,7.8571429 50.446429,7.8571429 C 50.465029,7.8571429 50.471543,7.8571429 50.47619,7.8571429 C 60.236853,7.857143 67.142857,15.497098 67.142857,25 C 67.142857,34.502902 59.760662,42.142857 50,42.142857 L 32.857143,42.142857 L 32.857143,7.8571429 z\" id=\"path2884\"/> \n \
	\ </g>" 100 50
or2 = SVGComponent "or2" [SVGPort (SVGPoint 31 15) (SVGPoint 5 15)
	"in1", SVGPort (SVGPoint 31 35) (SVGPoint 5 35) "in2",
	SVGPort (SVGPoint 70 25) (SVGPoint 96 25) "out"]
	"<g id=\"or2\">\n \
    \ <!--rect width=\"100\" height=\"50\" style=\"fill:rgb(0,0,255);stroke-width:1;   stroke:rgb(0,0,0)\"/--> \
    \ <path style=\"fill: none; stroke: rgb(0, 0, 0); stroke-width: 2; stroke-linecap: butt; stroke-linejoin: miter; stroke-opacity: 1;\" d=\"M 70,25 L 96,25\" id=\"out\"/>\n \
    \ <path style=\"fill: none; stroke: rgb(0, 0, 0); stroke-width: 2; stroke-linecap: butt; stroke-linejoin: miter; stroke-opacity: 1;\" d=\"M 31,15 5,15\" id=\"in1\"/> \n \
    \ <path style=\"fill: none; stroke: rgb(0, 0, 0); stroke-width: 2; stroke-linecap: butt; stroke-linejoin: miter; stroke-opacity: 1;\" d=\"M 31,35 5,35\" id=\"in2\"/> \n \
    \ <g id=\"g2560\" transform=\"translate(26.5, -39.5)\"> \n \
      \ <path style=\"fill: rgb(0, 0, 0); fill-opacity: 1; fill-rule: evenodd; stroke: none; stroke-width: 3; stroke-linecap: butt; stroke-linejoin: miter; stroke-opacity: 1;\" d=\"M -2.40625,44.5 L -0.40625,46.9375 C -0.40625,46.9375 5.25,53.937549 5.25,64.5 C 5.25,75.062451 -0.40625,82.0625 -0.40625,82.0625 L -2.40625,84.5 L 0.75,84.5 L 14.75,84.5 C 17.158076,84.500001 22.439699,84.524514 28.375,82.09375 C 34.310301,79.662986 40.911536,74.750484 46.0625,65.21875 L 44.75,64.5 L 46.0625,63.78125 C 35.759387,44.71559 19.506574,44.5 14.75,44.5 L 0.75,44.5 L -2.40625,44.5 z M 3.46875,47.5 L 14.75,47.5 C 19.434173,47.5 33.03685,47.369793 42.71875,64.5 C 37.951964,72.929075 32.197469,77.18391 27,79.3125 C 21.639339,81.507924 17.158075,81.500001 14.75,81.5 L 3.5,81.5 C 5.3735884,78.391566 8.25,72.45065 8.25,64.5 C 8.25,56.526646 5.3414686,50.599815 3.46875,47.5 z\" id=\"path4973\" /> \n \
	  \ </g> \n \
	\ </g>" 100 50
cmps = [and2, or2]
insts = [SVGPComponent and2 (SVGPoint 10 10),
	  SVGPComponent and2 (SVGPoint 10 100),
	  SVGPComponent or2 (SVGPoint 150 10)]
paths = [
	SVGPath [SVGPoint 106 35, SVGPoint 155 35, SVGPoint 155 25],
	SVGPath [SVGPoint 106 125, SVGPoint 155 125, SVGPoint 155 45]
	]
writeStuff :: IO ()
writeStuff = writeFile "xcircuit.svg" $ makeSVGString $ makeSVGTags
	cmps insts paths
{- delete this -}

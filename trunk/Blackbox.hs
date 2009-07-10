{- output the svg of a custom gate (with an arbitrary number of inputs
 	and outputs -}
module Blackbox where

-- imports
import System.IO

-- XML header
xmlhead :: String
xmlhead = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"

-- SVG header
svghead :: String
svghead = "<svg width=\"100%\" height=\"100%\" version=\"1.1\"\n \
			\xmlns=\"http://www.w3.org/2000/svg\" \n \
			\xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n"

-- SVG tags
svgtags :: (String, String)
svgtags = (svghead,"</svg>")

-- Opening and closing tags for <defs>
defstags :: (String, String)
defstags = ("<defs>","</defs>")

{- Constants - preprocessed, though they could possibly be computed
	by some functions -}
-- Line width
linewidth :: Int
linewidth = 30

-- Box width
boxwidth :: Int
boxwidth = 60

{- Get the gate height, based on the number of inputs and outputs -}
getHeight :: Int -> Int -> Int
getHeight nin nout = h where
	n = max nin nout
	h = (10 * n) + 10 -- Standard distance between ports = 10

{- Get vertical coordinates for ports, given the number of ports and
	the gate height -}
getYs :: Int -> Int -> [Int]
getYs nports height = map (getCurrentY height nports) [1..nports] where
	getCurrentY h np cp = 0 + cp * (h) `div` (np + 1)

{- Gets the path tags based on an initial X and a list of Y coordinates -}
genPathTags :: Int -> [Int]-> [String]
genPathTags x0 ys = map (genPathTag x0) ys where
	genPathTag x0 y = "<path d=\"M" ++ xstart ++ " " ++ (show y) ++ " L" ++
		xend ++ " " ++ (show y) ++ "\" style=\"stroke:black; \
		\stroke-width:2\" />"
		where
		xstart = show x0
		xend = show (x0 + linewidth)

{- Gets a <rect> tag -}
genRectTag :: Int -> Int -> Int -> String
genRectTag x0 w h = "<rect x=\"" ++ (show x0) ++ "\" y=\"0\" \
	\width=\"" ++ (show w) ++ "\" height=\"" ++
 	(show h) ++ "\" style=\"stroke:black;stroke-width:2.3; \
	\fill:none\" />" 
		
{- Generates a <use> tag -}
genUseTag :: String -> Int -> Int -> String
genUseTag href x y = "<use xlink:href=\"#" ++ href ++ "\" x=\"" ++
	(show x) ++ "\" y=\"" ++ (show y) ++ "\" />"

{- Returns the (opening and closing) tags for a group with a given ID -}
makeGroupTags :: String -> (String, String)
makeGroupTags id = ("<g id=\"" ++ id ++ "\">","</g>")

{- Make a complete <defs> tag -}
makeDefs :: Int -> Int -> [String]
makeDefs nin nout = [fst defstags] ++ [fst ingtags]
	++ (genPathTags 0 inys) ++ [snd ingtags]
	++ [fst outgtags] ++ (genPathTags (linewidth + boxwidth) outys) 
	++ [snd outgtags] ++ [fst bboxtags] 
	++ [genRectTag linewidth boxwidth height] ++ [genUseTag "ins" 0 0] 
	++ [genUseTag "outs" 0 0]
	++ [snd bboxtags] ++ [snd defstags]
	where
	ingtags = makeGroupTags "ins"
	outgtags = makeGroupTags "outs"
	bboxtags = makeGroupTags "bbox"
	height = getHeight nin nout
	inys = getYs nin height
	outys = getYs nout height

{- Makes a list of SVG tags -}
makeSVGs :: Int -> Int -> [String]
makeSVGs nin nout = [xmlhead] ++ [svghead] ++ (makeDefs nin nout)
	++ [genUseTag "bbox" 10 10] ++ [snd svgtags]

{- Write a SVG file - example, TODO: pretty print -}
writeSVG :: Int -> Int -> IO ()
writeSVG nin nout = do
	writeFile "xprxmpl.svg" $ foldl (++) "" $ map (\ s -> s ++ "\n") $
		makeSVGs nin nout

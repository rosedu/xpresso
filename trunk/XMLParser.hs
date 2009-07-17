import Text.XML.HaXml
import Text.XML.HaXml.Parse

import IDraw
import System.IO.Unsafe
import System.Directory
import Data.List

{- unsafe -}
getSVGComponents :: [SVGComponent]
getSVGComponents = map (getSVGComponent.("xml/"++)) $ filter isXML $ 
			unsafePerformIO $ getDirectoryContents "xml"
    where
    	isXML fileName = isSuffixOf ".xml" fileName

getSVGComponent :: String -> SVGComponent
getSVGComponent filePath = getGateComp $ unsafeReadFile filePath

unsafeReadFile :: String -> String
unsafeReadFile filePath = unsafePerformIO $ readFile filePath

getGateComp :: String -> SVGComponent
getGateComp xml = SVGComponent svgType svgPorts svgFile w h
    where
    	{- gate type -}
    	gateElem = (\(CElem x) -> x) $ head $ tag "gate" $ CElem root   
	(svgType,[w, h]) = (getVal "type", map (read.getVal) ["width","height"])
	    where
		list = map nameValTuples $ getAttrList gateElem
		getVal var = snd $ head $ filter ((==var) . fst) list

	{- list of ports -}
	svgPorts =  map (makePort . map nameValTuples . getAttrList) $ 
				(portElems "inputs") ++ (portElems "outputs")
    	(Document _ _ root _ ) = xmlParse ("No Document") xml
	
	portElems portType = map (\(CElem x) -> x) 
		$ tag "gate" /> tag portType /> tag "port" $ CElem root   
	getAttrList (Elem _ attrs _) = attrs 
	nameValTuples (name, (AttValue [Left val])) = (name, val)
	makePort list = SVGPort (SVGPoint x1 y1) (SVGPoint x2 y2) label
	    where
	    	[x1, y1, x2, y2] = map (read.getVal) ["x1", "y1", "x2", "y2"]
		getVal var = snd $ head $ filter ((==var) . fst) list
	    	label = getVal "name"

	{- SVG code -}
	svgElem = (\(CElem x)->x) $ head $ tag "gate" /> tag "svg" $ CElem root
	getSvgFile (Elem  _ [(_,AttValue [Left path])] _) = unsafeReadFile path
	svgFile = getSvgFile svgElem

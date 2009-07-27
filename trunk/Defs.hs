{- Type declarations go into this module -}
module Defs where

{- These options help specify restrictions to the minimization method -}
data Options = Options {
    circType :: CircuitType,
    pAnd     :: Int,
    pOr      :: Int,
    pNand    :: Int,
    pNor     :: Int
}
data CircuitType = AsIs | AndOr | Nand | Nor

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


{- Global variables -}
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

{- Global variables for circuit representation -}
globalDELTAMIN :: Int
globalDELTAMIN = 2
globalLEVELWIREDELTA :: Int
globalLEVELWIREDELTA = 2

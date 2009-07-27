module Main where

import Graphics.UI.Gtk hiding (Nand,Nor)

import System.IO.Unsafe

import ExprParser
import IDraw
import Gates
import Structural
import XMLParser
import Defs

import Debug.Trace

main = do
    initGUI
    window <- windowNew
    top <- hBoxNew True 10
    set window [windowDefaultWidth:=800, windowDefaultHeight:=400,
    	containerChild := top, containerBorderWidth:=10]

    textBoxes <- vBoxNew True 10
    controls <- vBoxNew True 10
    boxPackStart top textBoxes PackGrow 0
    boxPackStart top controls PackGrow 0


    input <- textBufferNew Nothing
    output <- textBufferNew Nothing
    inputView <- textViewNewWithBuffer input 
    outputView <- textViewNewWithBuffer output
    mapM_ (\x->textViewSetWrapMode x WrapWord) [inputView,outputView] 
    boxPackStart textBoxes inputView PackGrow 0
    boxPackStart textBoxes outputView PackGrow 0

    inputType <- vBoxNew True 10
    inputButtons <- radioButtons ["Expression","Truth Table 1", 
    	"Truth Table 2", "PseudoVerilog"]
    mapM_ (\x->boxPackStart inputType x PackNatural 0) inputButtons
    boxPackStart controls inputType PackNatural 0

    button <- buttonNewWithLabel "Evaluate"
    boxPackStart controls button PackNatural 0

    circType <- vBoxNew True 10
    circButtons <- radioButtons ["As Is","And-Or","Nand","Nor"]
    mapM_ (\x->boxPackStart circType x PackNatural 0) circButtons
    boxPackStart controls circType PackNatural 0

    --(head inputButtons) `onToggled` do
    --	textBufferSetText input "toggled"

    button `onPressed` do 
    	expr <- get input textBufferText
	fname <- get output textBufferText
    	eval expr fname inputButtons circButtons output

    widgetShowAll window

    onDestroy window mainQuit
    mainGUI

radioButtons :: [String] -> IO [RadioButton]   
radioButtons (button:[]) = do
    b <- radioButtonNewWithLabel button
    return [b]
radioButtons (b1:b2:rest) = do
    b_list <- radioButtons (b2:rest)
    b <- radioButtonNewWithLabelFromWidget (head b_list) b1
    return (b:b_list)

eval :: String -> FilePath -> [RadioButton] -> [RadioButton] -> TextBuffer -> IO ()
eval text fileName [exp, tt1, tt2, pv] circButtons output
    | getActive exp = 
    	makeSVGFile gates svgc fileName
    | getActive tt1 = 
    	textBufferSetText output $ show $ getGatesFromTable vars1 table1 opts
    | getActive tt2 = 
    	textBufferSetText output $ show $ getGatesFromTable vars2 table2 opts
    | getActive pv = 
    	textBufferSetText output $ show $ getComponentList 
				(getStatmnts (lines text)) getComponents
     where
        svgc = getSVGComponents
      	gates = getGatesFromExpr text opts
	(vars1,table1) = parseTT1 text 
	(vars2,table2) = parseTT2 text 
    	getActive button = unsafePerformIO $ toggleButtonGetActive button
	opts = getOptions circButtons

getOptions :: [RadioButton] -> Options
getOptions [as_is, and_or, nand, nor] 
    | getActive as_is = Options {circType=AsIs,pAnd=2,pOr=2,pNand=2,pNor=2}
    | getActive and_or = Options {circType=AndOr,pAnd=2,pOr=2,pNand=2,pNor=2}
    | getActive nand = Options {circType=Nand,pAnd=2,pOr=2,pNand=2,pNor=2}
    | getActive nor = Options {circType=Nor,pAnd=2,pOr=2,pNand=2,pNor=2}
    where
    	getActive button = unsafePerformIO $ toggleButtonGetActive button


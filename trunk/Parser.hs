{- haha, we have a parser -}

{- i can has parser? -}

module Parser where

import Data.Map
import Control.Monad(liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

{- tipul expresie -}
data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr
    deriving Show
{- tipul operator unar -}
data Unop = Not deriving Show
{- tipul operator dual -}
data Duop = And | Or deriving Show

type TruthTable = [([Bool], Bool)]

{- specificatia sintaxei -}
def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "'+*~"
              , opLetter = oneOf "'+*~"
              , reservedOpNames = ["~", "+", "*", "'"]
	      , reservedNames = ["0", "1"]
              }

{- mini-parseruti -}
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

{- expression parser function -}
exprParser :: Parser Expr
exprParser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Postfix (m_reservedOp "'" >> return (Uno Not))]
        , [Infix (m_reservedOp "*" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Or)) AssocLeft]
        ]
term = m_parens exprParser
       <|> liftM Var m_identifier
       <|> (m_reserved "1" >> return (Con True))
       <|> (m_reserved "0" >> return (Con False))

{- insert spaces before ~ and after ' operators -}
insertSpaces :: String -> String
insertSpaces [] = []
insertSpaces (c:str) = if (elem c "'~") then c:' ':(insertSpaces str)
			else c:insertSpaces str

{- string-to-expression function -}
play :: String -> Maybe Expr 
play inp = case parse exprParser "" (insertSpaces inp) of
		Left err -> Nothing
		Right expr -> Just expr

{- evaluates an expression for a var-value binding -}
parseExpr :: Expr -> Map String Bool -> Bool
parseExpr (Var a) list = findWithDefault False a list
parseExpr (Con a) list = a
parseExpr (Uno Not a) list = not (parseExpr a list)
parseExpr (Duo And a b) list = (parseExpr a list) && (parseExpr b list)
parseExpr (Duo Or a b) list = (parseExpr a list) || (parseExpr b list)

{- extracts variable names from an expression -}
getVars :: Expr -> Map String Bool
getVars (Var a) = singleton a False 
getVars (Con a) = empty 
getVars (Uno _ a) = getVars a 
getVars (Duo _ a b) = union (getVars a) (getVars b)

{- builds the truth table for a given expression -}
makeTableFromExpr :: Expr -> TruthTable
makeTableFromExpr expr = makeTableFromExprAux expr n vars []
    where 
	n	= size $ getVars expr
	vars  	= fmap fst (toList (getVars expr))

{- auxiliary function for makeTable -}
makeTableFromExprAux :: Expr -> Int -> [String] -> [Bool] -> TruthTable 
makeTableFromExprAux expr n vars current = if (length current) < n then
    (makeTableFromExprAux expr n vars (False:current)) ++ 
    	(makeTableFromExprAux expr n vars (True:current))
    else [(current, (parseExpr expr pairMap))]
    where 
	pairMap	= fromList $ zip vars current

fullTable :: Int -> [Bool] -> [[Bool]]
fullTable n current = if (length current) < n then 
	(fullTable n (False:current)) ++ (fullTable n (True:current))
	else current:[]


makeTableFromTable :: [[Bool]] -> TruthTable
makeTableFromTable tt = makeTableFromTableAux(fullTable (length(head tt)) []) tt

makeTableFromTableAux :: [[Bool]] -> [[Bool]] -> TruthTable
makeTableFromTableAux [] _ = []
makeTableFromTableAux (x:full) tt = if (elem x tt) then 
				(x, True) : (makeTableFromTableAux full tt)
			else 
				(x, False): (makeTableFromTableAux full tt)


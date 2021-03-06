{- haha, we have a parser -}

{- i can has parser? -}

module ExprParser where

import Data.Map
import Control.Monad(liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

{- "Expression" type -}
data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr
    deriving Show
{- Unary operator type -}
data Unop = Not deriving Show
{- Dual operator type -}
data Duop = And | Or | Xor deriving Show
type TruthTable = [([Bool], Bool)]

xor :: Bool -> Bool -> Bool
xor a b = if a==b then False else True

{- specificatia sintaxei -}
def = emptyDef{ identStart = letter
          , identLetter = alphaNum
          , opStart = oneOf "'+*~^"
          , opLetter = oneOf "'+*~^"
          , reservedOpNames = ["~", "+", "*", "'", "^"]
          , reservedNames = ["0", "1"]
          }

{- mini-parseruti -}
TokenParser { parens = m_parens
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
        , [Infix (m_reservedOp "^" >> return (Duo Xor)) AssocLeft]
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
parseExpr (Duo Xor a b) list = (parseExpr a list) `xor` (parseExpr b list)

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
    n    = size $ getVars expr
    vars      = fmap fst (toList (getVars expr))

{- auxiliary function for makeTableFromExpr -}
makeTableFromExprAux :: Expr -> Int -> [String] -> [Bool] -> TruthTable 
makeTableFromExprAux expr n vars current = if (length current) < n then
    (makeTableFromExprAux expr n vars (False:current)) ++ 
        (makeTableFromExprAux expr n vars (True:current))
    else [(current, (parseExpr expr pairMap))]
    where 
    pairMap    = fromList $ zip vars current

{- generates all possible truth-value combinations of a certain length -}
fullTable :: Int -> [Bool] -> [[Bool]]
fullTable n current = if (length current) < n then 
    (fullTable n (False:current)) ++ (fullTable n (True:current))
    else current:[]

{- transforms f given by {x | f(x) = True} into a TruthTable for f -}
makeTableFromTable :: [[Bool]] -> TruthTable
makeTableFromTable tt = makeTableFromTableAux(fullTable (length(head tt)) []) tt

{- auxiliary function for makeTableFromTable -}
makeTableFromTableAux :: [[Bool]] -> [[Bool]] -> TruthTable
makeTableFromTableAux [] _ = []
makeTableFromTableAux (x:full) tt = if (elem x tt) then 
                (x, True) : (makeTableFromTableAux full tt)
            else 
                (x, False): (makeTableFromTableAux full tt)

parseTT1 :: String -> ([String],TruthTable)
parseTT1 raw = (vars, Prelude.map makeTuple table)
    where
    rows = lines raw
    vars = words $ head rows
    unparsed = Prelude.map words $ tail rows
    table = Prelude.map getRow unparsed
    getRow row = Prelude.map (\x -> if x=="0" then False else True) row
    makeTuple list = (init list, last list)

parseTT2 :: String -> ([String],TruthTable)
parseTT2 raw = (vars, table)
    where
    rows = lines raw
    vars = words $ head rows
    unparsed = Prelude.map words $ tail rows
    table = makeTableFromTable $ Prelude.map getRow unparsed
    getRow row = Prelude.map (\x -> if x=="0" then False else True) row

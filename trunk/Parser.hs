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
exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Postfix (m_reservedOp "'" >> return (Uno Not))]
        , [Infix (m_reservedOp "*" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Or)) AssocLeft]
        ]
term = m_parens exprparser
       <|> liftM Var m_identifier
       <|> (m_reserved "1" >> return (Con True))
       <|> (m_reserved "0" >> return (Con False))

{- string-to-expression function -}
play :: String -> Maybe Expr 
play inp = case parse exprparser "" inp of
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

{- no comment -}
asf :: Maybe Expr -> Expr
asf (Just a) = a


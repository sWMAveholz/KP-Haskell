{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Data.Text (Text)
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char.Lexer (symbol, lexeme)
import qualified Text.Megaparsec.Char.Lexer as L

-- AST
data Node = Or Node Node 
  | And Node Node 
  | Not Node 
  | Var String 
  deriving (Show, Eq, Ord)

-- evaluation function
eval :: Node -> Bool
eval (Or x y) = eval x || eval y
eval (And x y) = eval x && eval y
eval (Not x) = not (eval x)
eval (Var x) = varTF x

-- helper
varTF :: String -> Bool
varTF "T" = True
varTF "True" = True
varTF "true" = True
varTF x = False


-- Parsing

-- define type Parser
type Parser = Parsec Void Text

sc :: Parser ()
sc = space 

sym :: Text -> Parser Text
sym = L.symbol sc                        

pVariable :: Parser Node
pVariable = Var <$>
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

parens :: Parser a -> Parser a
parens = between (sym "(") (sym ")")

pTerm :: Parser Node
pTerm = choice
  [ parens pExpr
  , pVariable
  ]

pExpr :: Parser Node
pExpr = makeExprParser pTerm operatorTable

-- operatorTable with prefix operator Not and binay operater And and Or
operatorTable :: [[Operator Parser Node]]
operatorTable =
  [ [ prefix "!" Not ]
  , [ binary "&" And ]
  , [ binary "|" Or ]
  ]

-- binary function used in operatorTable
binary :: Text -> (Node -> Node -> Node) -> Operator Parser Node
binary  name f = InfixL  (f <$ sym name)

-- prefix function used in operatorTable
prefix :: Text -> (Node -> Node) -> Operator Parser Node
prefix  name f = Prefix  (f <$ sym name)
--postfix name f = Postfix (f <$ chunk name) --here not needed

parseAST = parse pExpr "<stdin>"
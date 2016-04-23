module Main(test) where

import Prelude
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)


test :: String -> Either ParseError Integer
test s = runParser expr 0 "<string>" s

expr    = buildExpressionParser table term
         <?> "expression"

term    =  parens expr
         <|> natural
         <?> "simple expression"

table   = [ [prefix "pre" (+1), binary "bin" (*) AssocLeft , postfix "post" (\x -> x - 1) ]
           , [postfix "++" (+1)]
           , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
--           , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
           ]

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

-- The lexer
lexer       = P.makeTokenParser haskellDef

parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
natural     = P.natural lexer
reservedOp  = P.reservedOp lexer

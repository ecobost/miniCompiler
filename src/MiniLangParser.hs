

module MiniLangParser (parse, start) where 

import Prelude hiding (fail)
import MiniLangSyntax
import ParserCombinators


-- Parsers for the mini language
start :: Parser Program
start = space >> program

-- program is many statements followed by an expression
program = many stmt >>= \s -> 
          return $ Program s 

-----------
-- Types --
-----------

typ = primitiveType

primitiveType = tyint +++ tybool

tyint = keyword "int" >> return TyInt
tybool = keyword "bool" >> return TyBool

----------------
-- Statements --
----------------

-- It is important that assignment is tried last
-- We want that the alternatives tried first fail at the first token, without consuming input
stmt = skip +++ ifstmt +++ block +++ while +++ retstmt +++ decl +++ assignment

skip = symbol ";" >> return Skip

ifstmt = keyword "if" >>
         parens expr >>= \c ->
         stmt >>= \t ->
         keyword "else" >>
         stmt >>= \e ->
         return $ If c t e

block = braces (many stmt) >>= \ss ->
        return (Block ss)
         
while = keyword "while" >>
        parens expr >>= \e ->
        stmt >>= \s -> 
        return (While e s)

retstmt = keyword "return" >> expr >>= \e -> symbol ";" >> return (Return e)

decl = do
     ty <- typ
     n <- token identifier
     res <- vardecl ty n +++ fundecl ty n
     return res

vardecl ty n = 
    ((operator "=" >> expr) +++ return Undefined) >>= \e ->
    symbol ";" >>
    return (VarDecl ty n e)
    
fundecl ty n = do
        pars <- parens (commalist param)
        b <- block
        return $ FunDecl ty n pars b

param = do   
  ty <- typ
  n <- token identifier
  return (n, ty)

commalist p = 
  ( do x <- p
       xs <- many (symbol "," >> p)
       return (x:xs)
  ) +++ return []

-- make sure assignment is tried last
assignment = token identifier >>= \v ->
             operator "=" >>
             expr >>= \e ->
             symbol ";" >> 
             return (Assignment v e) 

-----------------
-- Expressions --
-----------------

expr = composite

atomic = literal +++ funcallOrVarRef +++ parens expr

literal = intLiteral +++ boolLiteral

intLiteral = token nat >>= \i -> return $ IntLit i

boolLiteral = strue +++ sfalse

strue = keyword "true" >> return (BoolLit True)
sfalse = keyword "false" >> return (BoolLit False)

funcallOrVarRef = do 
  n <- token identifier
  funCall n +++ return (Var n)

funCall n = do
  args <- parens (commalist expr)
  return $ FunCall n args

-- general strategy to deal with left associative operators
chainLeftAssoc p ops = p >>= \left -> chainX p ops left
chainX p ops left = ( foldl1 (+++) (map operator ops) >>= \op -> p >>= \right -> 
                      chainX p ops $ BinOp (stringToOp op) left right
                    ) +++ return left


composite = chainLeftAssoc comparison ["==", "<>"]
comparison = chainLeftAssoc summation ["<=", ">=", "<", ">"]
summation = chainLeftAssoc term ["+", "-"]
term = chainLeftAssoc atomic ["*", "/"]

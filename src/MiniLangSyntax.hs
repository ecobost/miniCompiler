

module MiniLangSyntax where

import Data.List (intersperse)

-- This it the abstract syntax of our small language
-- First the expressions in the language

data Ty = TyInt
        | TyBool
        | TyFun Ty [Ty] -- first Ty is the return type, [Ty] are the parameter types
        deriving Eq

instance Show Ty where
    show TyInt = "int"
    show TyBool = "bool"
    show (TyFun ret params) = "(" ++ concat (intersperse ", " (map show params)) ++ ") -> " ++ show ret

data Expr = IntLit Int 
          | BoolLit Bool
          | BinOp Op Expr Expr
          | Var String
          | FunCall String [Expr] -- String has the name of the function, [Expr] the arguments
          | FunDef [String] Stmt -- A function is just a value, consisting list of paratemeter names
                                 -- and a statement (the function's body)
          | Undefined          
--            deriving Eq

-- The different kinds of operations that are supported
data Op = OpPlus | OpMinus | OpMultiplies | OpEqual | OpNotEqual |
          OpDivides | OpLess | OpLessE | OpGreater | OpGreaterE 
        deriving Eq

-- Statements of our language
data Stmt = Skip 
          | If Expr Stmt Stmt
          | Assignment String Expr
          | Block [Stmt]
          | While Expr Stmt
          | VarDecl Ty String Expr
          | FunDecl Ty String [(String, Ty)] Stmt
            -- name, return type, param list, body stmts, return expr
          | Return Expr

-- A program is a list of statements, followed by a single expression
data Program = Program [Stmt]

instance PP Program where
  pp _ (Program ss) =
      concatMap (pp 0) ss

-- The primitive functions of our language
primEval OpPlus (IntLit i) (IntLit j) = IntLit $ i + j
primEval OpMinus (IntLit i) (IntLit j) = IntLit $ i - j
primEval OpMultiplies (IntLit i) (IntLit j) = IntLit $ i * j
primEval OpDivides (IntLit i) (IntLit j) = IntLit $ i `div` j
primEval OpEqual (IntLit i) (IntLit j) = BoolLit $ i == j
primEval OpEqual (BoolLit i) (BoolLit j) = BoolLit $ i == j
primEval OpNotEqual (IntLit i) (IntLit j) = BoolLit $ i /= j
primEval OpNotEqual (BoolLit i) (BoolLit j) = BoolLit $ i /= j
  -- Booleans are not ordered in our language
primEval OpLess (IntLit i) (IntLit j) = BoolLit $ i < j
primEval OpLessE (IntLit i) (IntLit j) = BoolLit $ i <= j
primEval OpGreater (IntLit i) (IntLit j) = BoolLit $ i > j
primEval OpGreaterE (IntLit i) (IntLit j) = BoolLit $ i >= j
primEval x l r = error $ 
                 "ICE: no definition for primitive operation: " ++ show x ++ " " ++ show l ++ " " ++ show r

-- A helper function to turn a string representation to the opcode, 
-- to be used in the parser
stringToOp :: String -> Op
stringToOp "+" = OpPlus
stringToOp "-" = OpMinus
stringToOp "*" = OpMultiplies
stringToOp "/" = OpDivides
stringToOp "==" = OpEqual
stringToOp "<>" = OpNotEqual
stringToOp "<" = OpLess
stringToOp "<=" = OpLessE
stringToOp ">" = OpGreater
stringToOp ">=" = OpGreaterE



indent :: Int -> String
indent n = replicate n ' '

-- A tiny bit of smarts on how to lay out code, so that it is less tedious to read
class PP a where 
  pp :: Int -> a -> String

instance PP Stmt where
  pp ind (If c t e) = indent ind ++ 
                      "if (" ++ show c ++ ") \n" ++ 
                      pp (ind + 2) t ++
                      indent ind ++ "else\n" ++
                      pp (ind + 2) e
  pp ind (While c b) = indent ind ++ 
                        "while (" ++ show c ++ ") \n" ++
                        pp (ind + 2) b
  pp ind (Skip) = indent ind ++ ";\n"
  pp ind (Assignment v e) = indent ind ++ v ++ "=" ++ show e ++ ";\n"
  pp ind (Block ss) = indent ind ++ "{\n" ++ concatMap (pp (ind + 2)) ss ++ indent ind ++ "}\n"
  pp ind (VarDecl ty v e) = indent ind ++ show ty ++ " " ++ v ++ "=" ++ show e ++ ";\n"
  pp ind (FunDecl tyRet name params (Block ss)) =
    indent ind ++ show tyRet ++ " " ++ name ++ "(" ++ 
    (concat $ intersperse ", " $ map (\(n, t) -> show t ++ " " ++ n) params) ++ 
    ")\n" ++ pp ind (Block ss)
  pp ind (Return expr) = indent ind ++ "return " ++ show expr ++ ";\n"


instance Show Op where
         show OpPlus = "+"
         show OpMinus = "-"
         show OpMultiplies = "*"
         show OpDivides = "/"
         show OpEqual = "=="
         show OpNotEqual = "<>"
         show OpLess = "<"
         show OpLessE = "<="
         show OpGreater = ">"
         show OpGreaterE = ">="


instance Show Expr where
    show (IntLit i) = show i
    show (BoolLit b) = show b
    show (Var x) = x
    show (BinOp op a b) = addParens $ show a ++ show op ++ show b
    show (FunCall name exprs) = name ++ addParens (concat $ intersperse ", " $ map show exprs)
    show (FunDef params s) = "function: (" ++ concat (intersperse ", " params) ++ ")\n" ++ pp 0 s
    show (Undefined) = "undefined"

addParens s = "(" ++ s ++ ")"

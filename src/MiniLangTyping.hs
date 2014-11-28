module MiniLangTyping where

import MiniLangSyntax
import WSE

import Control.Monad (when)

import Data.List (nub, (\\))

import Environment

type Message = String
type TypingM a = WSE Message (Env VarName Ty) Message a

-- The "typing monad"
-- W: the log is a list of type error messages
--   tell function adds to the log
-- S: the state is an association list mapping variable names to types
--   get, put, and modify functions access and modify the environment
-- E: Error type is String
--   throwError and catchError deal with errors 
--   (none are thrown in the evaluator)


-- add a type error to the error message log
tellTE :: Message -> TypingM ()
tellTE s = tell $ "Type error: " ++ s

-- throw a type error 
throwTE :: Message -> TypingM a
throwTE s = throwError $ "Type error: " ++ s

--------------------------
-- Type check a program --
--------------------------

typeProg :: Program -> IO Ty
typeProg (Program ss) = do
  let checker = 
          catchError ( mapM_ typeOK ss >> 
                       typeOf (FunCall "main" [])) 
            (\s -> tell s >> return undefined)
            -- if type checking fails, handle the error
            -- by adding it to the error log

  case runWSE checker emptyEnv of
    (Right ty, _, []) -> return ty
    (_, _, msgs) | not (null msgs) 
                     -> do 
                     mapM_ putStrLn msgs
                     fail "Type-checking failed"
                 | otherwise -> fail "ICE"
                     
------------------------
-- Type an expression --
------------------------

typeOf :: Expr -> TypingM Ty

typeOf (IntLit _) = return TyInt

typeOf (BoolLit _) = return TyBool

typeOf (BinOp op e1 e2) = do
       ty1 <- typeOf e1 
       ty2 <- typeOf e2
       primTypeOf op ty1 ty2

typeOf (Var n) = do
       env <- get
       case lookup n env of
         Nothing -> throwTE $ "Undefined variable " ++ n
         Just v -> return v

typeOf f@(FunCall n args) = do
	env <- get
	case lookup n env of
		Just (TyFun ret params) -> do --1
			--if(checkArgumentList args params) then --4
			checkArgumentList args params
			return ret
			--else throwTE $ "Type error in argument list of function " ++ n
		
		_ -> throwTE $ "Undefined function " ++ n --2
-- FIXED
-- 1. The variable n has to be defined 
-- 2. n has to be a function
-- 3. Each element of args has to be well typed
-- 4. The type of each argument in arg must be the same as the corresponding type
--    of the parameter in the functions parameter list

typeOf _ = error "ICE: Unrecognized expression in typeOf"

-----------------------------
-- Type check a statement  --
-----------------------------

-- This wrapper function will call typeOK', catch any type errors, 
-- and write them to the error log. Hence, if you wish to continue
-- type checking even in case of errors, use typeOK. If you wish
-- to give up for the current statement, use typeOK'.
typeOK :: Stmt -> TypingM ()
typeOK s = catchError (typeOK' s) tell

-- This function may throw a type error
typeOK' :: Stmt -> TypingM ()

typeOK' Skip = return ()

typeOK' (If e s1 s2) = do
  tye <- typeOf e
  when (tye /= TyBool) $ tellTE "condition of if should be bool"
  typeOK s1
  typeOK s2

typeOK' (Assignment s e) = do
-- The variable s has to be defined
-- The type of the expression e has to be the same as the declared type of s
  tyv <- typeOf (Var s)
  tye <- typeOf e
  when (tye /= tyv) $ 
       throwTE $ "assignment expecting type " ++ 
               show tyv ++ " got type " ++ show tye

typeOK' (While e b) = do
	tye <- typeOf e
	when (tye /= TyBool) $ tellTE "condition of while should be bool"
	typeOK b
--FIXED

typeOK' (VarDecl ty v e) = do 
  env <- get
  when (isVarDefinedInInnermostScope v env) 
           (throwTE $ "variable already declared " ++ v)
  tye <- typeOf e
  when(tye /= ty) $
	throwTE $ "Expression type ("++show tye 
	++")  does not match variable type (" ++ show ty ++")"
-- FIXED make sure that the declared type and the initializer type
-- agree
  modify (newVar v tye)

typeOK' (Block ss) = do
-- Every statement in ss should be well typed
-- Typing environment should be restored to its original 
-- state after ss have been type checked
  enterBlock
  mapM_ typeOK ss 
  exitBlock


typeOK' (FunDecl ty n pars b@(Block ss)) = do
-- FIXED
	env <- get
	if ((lookup n env) /= Nothing) then (throwTE $ "Function "++ n ++ " already declared")
	else do
		checkDuplicates pars
		modify (newVar "$ret" ty)
		modify (newVar n (TyFun ty (map (\(_,typ) -> typ) pars)))
		addPars pars
		typeOK b
		--Somehting
-- n should not yet be defined in the current scope
-- There should be no duplicate parameter names in pars
-- When type checking ss, a binding for n should be in the environment
--   to allow recursive calls.
-- When type checking ss, the parameter name-->paramter type bindings should
--   be in the environment
-- Note that type checking a return statement in ss will require some
--   way of knowing what the return type "ty" is. One way to deal with this
--   is to add a special binding like ("$ret", ty) into the typing environment

typeOK' (Return expr) = do
-- FIXED
	env <- get
	case lookup "$ret" env of
		Nothing -> error $ "ICE: type error in return checking: " ++ show expr
		Just tyret -> do
			tye <- typeOf expr
			when (tye /= tyret) $ throwTE 
				$ "Type error in return expresion: " ++ show expr ++ ". Expected type: " ++ show tyret
	
-- Look for the special $ret variable in the environment. 
-- Let its type be 'tyret'.
-- Make sure that expr has type 'tyret'.


checkArgumentList:: [Expr]-> [Ty] -> TypingM ()
checkArgumentList [] [] = return ()
checkArgumentList (e:elist) (t:tlist) = do
	tye <- typeOf e
	when (tye /= t) $ tellTE $ "Type error in argument list: " ++ show e
	checkArgumentList elist tlist
checkArgumentList _ _ = tellTE $ "Number of arguments does not match"

checkDuplicates:: [(String,Ty)] -> TypingM()
checkDuplicates [] = return ()
checkDuplicates ((name,_):rest) = if (checkOneDuplicate name rest) then (throwTE $ "Argument "++ name ++ " repeated")
				else checkDuplicates rest

checkOneDuplicate:: String -> [(String,Ty)] -> Bool
checkOneDuplicate _ [] = False
checkOneDuplicate name ((n,_):rest) = if (name==n) then True
	else checkOneDuplicate name rest

addPars :: [(String,Ty)] -> TypingM ()
addPars [] = return ()
addPars ((n,t):pars) = modify(newVar n t)

enterBlock :: TypingM ()
enterBlock = modify addMarker 

exitBlock :: TypingM ()
exitBlock = modify removeMarker


isArithmeticOp OpPlus = True
isArithmeticOp OpMinus = True
isArithmeticOp OpMultiplies = True
isArithmeticOp OpDivides = True
isArithmeticOp _ = False

isEqOp OpEqual = True
isEqOp OpNotEqual = True
isEqOp _ = False

isOrdOp OpLess = True
isOrdOp OpLessE = True
isOrdOp OpGreater = True
isOrdOp OpGreaterE = True
isOrdOp _ = False

-- The primitive functions of our language
--primTypeOf OpPlus TyInt TyInt = return TyInt
--primTypeOf OpMinus TyInt TyInt = return TyInt
--primTypeOf OpMultiplies TyInt TyInt = return TyInt
--primTypeOf OpDivides TyInt TyInt = return TyInt
primTypeOf op TyInt TyInt 
	| isArithmeticOp op = return TyInt 
	| otherwise = return TyBool
primTypeOf op TyBool TyBool
	| isArithmeticOp op = throwTE $ show op ++ " requires int parameters."
	| otherwise = return TyBool
primTypeOf op TyInt TyBool = throwTE $ "Incorrect parameters in operation " ++ show op
primTypeOf op TyBool TyInt = throwTE $ "Incorrect parameters in operation " ++ show op

--primTypeOf op TyInt TyInt | isEqOp op = return TyBool
--primTypeOf op TyInt TyInt | isOrdOp op = return TyBool
--primTypeOf op TyInt TyInt = return TyBool
--primTypeOf op TyBool TyBool = return TyBool
--primTypeOf OpEqual TyInt TyInt = return TyBool
--primTypeOf op TyBool TyBool = return TyBool
-- FIXED 
-- Equality/inequality/comparison operators are not checked properly
primTypeOf x l r = error $ 
                 "ICE: type error in operation: " ++ show x ++ " " ++ show l ++ " " ++ show r

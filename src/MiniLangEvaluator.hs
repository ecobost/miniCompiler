

module MiniLangEvaluator where

import WSE
import MiniLangSyntax
import Environment

type EvalM a = WSE String (Env VarName Expr) String a
-- The "evaluation monad"
-- W: the log is a list of Strings 
--   tell function adds to the log
-- S: the state is an association list mapping variable names to expressions
--   get, put, and modify functions access and modify the environment
-- E: Error type is String
--   throwError and catchError deal with errors 
--   (none are thrown in the evaluator)

-- Expression evaluator
eval :: Expr -> EvalM Expr

eval (IntLit i) = return $ IntLit i

eval (BoolLit b) = return $ BoolLit b

eval (Var n) = get >>= \env -> return $ lookupVar n env

eval (BinOp op a b) = do
  a' <- eval a 
  b' <- eval b
  return $ primEval op a' b'

eval (FunCall n args) = do
-- lookup the function n from the environment 
-- Evaluate all the args, and add paramter-->arg bindings to the environment
-- Execute the statement in the body of the function, until a return statement
-- You can assume that the return statement has put a special ("$ret", value)
--   binding on the top of the environment. That will be the return value.
-- Remove the activation record (all bindings introduced by this function,
--   including the special ret binding)
  env <- get
  let (FunDef pars (Block body)) = lookupVar n env 
  enterBlock
  eargs <- mapM eval args
  modify (zip pars eargs ++)
  execUntilReturn body
  env <- get
  let r = snd $ head env -- get the return value stored into the env
  exitBlock
  return r

eval Undefined = return Undefined

execUntilReturn [] = return ()
execUntilReturn (s:ss) = do
  exec s 
-- return statement is detected by observing the $ret binding on top 
-- of the stack
  ((r,_):_) <- get
  if r == "$ret" then return () else execUntilReturn ss

-- Execute statement
-- Statments can affect the environment, hence return a new
-- environment
exec :: Stmt -> EvalM ()

exec Skip = return ()

exec (If c t e) = do
    (BoolLit b) <- eval c
    if b then exec (wrapBlock t) else exec (wrapBlock e)

exec (Assignment v e) = do
  e' <- eval e
  assignVar v e'

exec (Block ss) = do 
  enterBlock
  execUntilReturn ss
  env <- get -- remember the possible ret value
  exitBlock
  -- restore the possible ret value on top of the stack
  case env of 
    (("$ret", e):_) -> modify ( newVar "$ret" e)
    _ -> return ()                       
    

exec w@(While e s) = do
  (BoolLit b) <- eval e
  if b
  then exec (wrapBlock s) >> exec w
  else return ()

exec (VarDecl _ v e) = do
  e' <- eval e
  declVar v e'

exec (FunDecl ty n pars (Block ss)) = 
-- add the binding of n --> function value to the environment
  modify (newVar n (FunDef (map fst pars) (Block ss)))

exec (Return e) = do
-- eval e and put the special "$ret" binding to the environment
    e' <- eval e
    modify (newVar "$ret" e')

wrapBlock :: Stmt -> Stmt
wrapBlock s@(Block b) = s
wrapBlock s = Block [s]

enterBlock :: EvalM ()
enterBlock = modify addMarker

exitBlock :: EvalM ()
exitBlock = modify removeMarker

declVar :: VarName -> Expr -> EvalM ()
declVar v e = do
  env <- get
  if isVarDefinedInInnermostScope v env
  then error $ "Variable already declared " ++ v
  else put $ newVar v e env
        
-- Assign to a variable.
-- If variable exists, replace its value. If it doesn't, add
-- the variable binding. Note, this implementaiton is very 
-- inefficient
assignVar :: VarName -> Expr -> EvalM ()
assignVar n e = do
  env <- get
  if isVarDefined n env then
      put $ map (\(n', e') -> if n == n' then (n, e) else (n', e')) env
  else
      error $ "Tried to assign to an undeclared variable " ++ n


-- Running the program means executing each of its statements.
-- The frist statement is executed in the empty environment,
-- the next statement in tne environment resulting from the first
-- statement, and so on
-- The expression of the program is evaluated in the environment
-- produced by the last statement

run :: Program -> IO Expr
run (Program statements) = do
  let (res, s, o) =
          runWSE (mapM_ exec statements >> eval (FunCall "main" [])) emptyEnv
  case res of
    (Right v) -> return v
    _ -> fail "ICE"



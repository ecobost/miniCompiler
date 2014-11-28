

module Main where

import System.Environment (getArgs)
import Control.Monad (when)

import MiniLangSyntax
import MiniLangParser
import MiniLangTyping
import MiniLangEvaluator

-- parse the program
parseProg  :: String -> IO Program 
parseProg s = do
    case parse start s of                
      [(prog, ls)] | null ls -> return prog
                   | otherwise -> 
                       fail $ "Parse error: unused input, starts with " 
                                ++ take 100 ls 
      _ -> fail "Unknown parse error"           

-- read the file and run it
-- This can be called from the interpreter as readRun True|False "myfile"
parseRun :: Bool -> String -> IO (Ty, Expr)
parseRun verbose s = do 
  p@(Program ss) <- parseProg s
  when verbose
    ( putStrLn "------ Program ------" >>
      putStrLn (pp 0 p) >>
      putStrLn "------ Result  ------"
    )  
  typ <- typeProg p
  pp <- run p
  return (typ, pp)

main = do 
  -- Minimum one and maximum two command line parameters are required
  -- If the first argument is "-v", more verbose output is provided
  -- Otherwise the first argument must be the filename
  args <- getArgs
  let verbose = args!!0 == "-v"
  let fn = if verbose then args!!1 else args!!0
  s <- readFile fn
  (typ, res) <-  parseRun verbose s
  putStrLn $ show typ
  putStrLn $ ": " ++ show res



module Environment where

-- Env is an association list from variable names to expressions
-- Expressions in the environment are assumed to always be values 

type VarName = String 
type Env key val = [(key, val)]

emptyEnv = []


-- Check if a variable exists in an environment
isVarDefined :: Eq k => k -> Env k v -> Bool
isVarDefined n env = n `elem` map fst env

isVarDefinedInInnermostScope :: VarName -> Env VarName v -> Bool
isVarDefinedInInnermostScope v env = v `elem` (map fst (currentScope env)) 

-- Look up the value of a variable from the environment
lookupVar :: (Eq k, Show k) => k -> Env k v -> v
lookupVar m env = case lookup m env of
                    Just v -> v
                    Nothing -> error $ "Unknown variable " ++ show m

-- Add a new variable to the environment
newVar :: k -> v -> Env k v -> Env k v
newVar n e env = (n, e):env

marker = "$"
isMarker (m, _) | m == marker = True
isMarker _ = False

currentScope :: Env String v -> Env String v
currentScope env = takeWhile (not . isMarker) env

addMarker :: Env VarName v -> Env VarName v
addMarker env = (marker, undefined) : env

removeMarker :: Env VarName v -> Env VarName v
removeMarker ((k, _):env) | k == marker = env
removeMarker (_:env) = removeMarker env
removeMarker _ = error $ "ICE: marker not found in environment"



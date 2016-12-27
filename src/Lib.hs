{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Lib
    ( runInterpreter
    ) where

-- many things learned from http://catamorph.de/documents/Transformers.pdf

-- I want my own definition of lookup
import Prelude hiding (lookup)

-- some helper functions live in here
import qualified Data.Map as Map
import qualified Data.List as List

import Data.Maybe

-- for exiting interpreter
import System.Exit

-- for readMaybe
import qualified Text.Read as Read

-- I want to get at the standard "print" function using the name System.print
import qualified System.IO as System

-- colour prompt
import System.Console.ANSI

-- I plan to use these monads to construct the parts of my interpreter
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- The pure expression language
type Name = String          -- variable names
data Val = I Int | B Bool   -- values
  deriving (Eq, Show, Read)
-- mapping from var names to values, vals in a list to make unioning easier later
type Env = [Map.Map Name [Val]]

data Expr = Const Val       -- expressions
           | Add Expr Expr
           | Sub Expr Expr
           | Mul Expr Expr
           | Div Expr Expr
           | And Expr Expr
           | Or Expr Expr
           | Not Expr
           | Eq Expr Expr
           | Gt Expr Expr
           | Lt Expr Expr
           | Var String
           deriving (Eq, Show, Read)

-- current value lookup, only return vars from top of state stack
lookup k t = case Map.lookup k t of
                Just [x] -> return x
                Nothing -> fail ("Unknown variable "++k)

-- Monadic style expression evaluator,
-- with error handling and Reader monad instance to carry dictionary
type Eval a = ReaderT Env (ExceptT String Identity) a

runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

-- Integer typed expressions
evali op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
    (I i0, I i1) -> return $ I (i0 `op` i1)
    _            -> fail "type error in arithmetic expression"

-- Boolean typed expressions
evalb op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
    (B i0, B i1) -> return $ B (i0 `op` i1)
    _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans
evalib op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
    (I i0, I i1) -> return $ B (i0 `op` i1)
    _            -> fail "type error in int->int->bool arithmetic expression"

-- Evaluate an expression
eval :: Expr -> Eval Val
eval (Const v)   = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1
eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1)  = do evalb (||) e0 e1
eval (Not e0  )  = do evalb (const not) e0 (Const (B True))
eval (Eq e0 e1)  = do evalib (==) e0 e1
eval (Gt e0 e1)  = do evalib (>) e0 e1
eval (Lt e0 e1)  = do evalib (<) e0 e1
eval (Var s)     = do
  env <- ask
  lookup s (head env)

-- The statement language
data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               deriving (Eq, Show, Read)

type Run a = StateT Env (ExceptT String IO) a
runRun p =  runExceptT ( runStateT p [Map.empty])

-- every time we change state we want to push the new state onto the top
-- of the stack
set :: (Name, Val) -> Run ()
set (s,i) = state $ (\(current:tail) -> ((), ((Map.insert s [i] current):current:tail)))

exec :: Statement -> Run ()
exec (Seq s0 s1) = do execRetain s0 >> execRetain s1

exec (Assign s v) = do
  st <- get
  Right val <- return $ runEval st (eval v)
  set (s,val)

exec (Print e) = do
  st <- get
  Right val <- return $ runEval st (eval e)
  liftIO $ System.print val
  return ()

exec (If cond s0 s1) = do
  st <- get
  Right (B val) <- return $ runEval st (eval cond)
  if val then do exec s0 else do exec s1

exec (While cond s) = do
  st <- get
  Right (B val) <- return $ runEval st (eval cond)
  if val then do exec s >> exec (While cond s) else return ()

exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)

-- start executing statements with empty map as state
run :: Statement -> IO ()
run stat = do
  result <- runExceptT $ (runStateT $ exec stat) [Map.empty]
  case result of
    Right ( (), env ) -> return ()
    Left exn -> System.print ("Uncaught exception: "++exn)

-- print statement in green
printNextStat :: Statement -> IO ()
printNextStat s = setSGR [SetColor Foreground Vivid Green] *> putStr "Next Stat: " *> putStr (show s) *> setSGR [] *> putStrLn ""

-- "pretty" coloured prompt to go before user commands
prompt :: IO String
prompt = setSGR [SetColor Foreground Vivid White, SetColor Background Vivid Magenta] *> putStr "λ>" *> setSGR [] *> putStr " " *> getLine


-- print next statements in a sequencing statement and wait
execRetain :: Statement -> Run ()
execRetain (Seq s1 s2) = do
  awaitCommand (Seq s1 s2)
execRetain s = do
  liftIO $ printNextStat s
  awaitCommand s

data Command = S    --step
             | SB   --step backwards
             | IC   --inspect current state of variables
             | IH   --inspect history of state of variables
             | Q    --quit
             | U    --unknown command (v likely given you can't backspace)
             deriving (Read, Eq)

parseCommand :: String -> Command
parseCommand s = case (Read.readMaybe s :: Maybe Command) of
                   Just c -> c
                   Nothing -> U   --parse failed, unknown command


-- pause execution, wait for user input
-- if this is just a plain sequencing statement
-- just expand it without asking the user
awaitCommand :: Statement -> Run ()
awaitCommand (Seq s1 s2) = do
  exec (Seq s1 s2)
awaitCommand stat = do
  line <- liftIO $ prompt
  handleCommand stat (parseCommand line)

-- take the user command and do it
handleCommand :: Statement -> Command -> Run ()
handleCommand s S = do
  exec s

handleCommand s SB = do
  liftIO $ putStrLn "This isn't implemented yet :'(" *> printNextStat s
  awaitCommand s

handleCommand s IC = do
  st <- get
  liftIO $ putStrLn (inspectCurrent st)
  awaitCommand s

handleCommand s IH = do
  st <- get
  liftIO $ putStrLn (inspectHistory st)
  awaitCommand s

handleCommand s Q = do
  liftIO $ exitSuccess

handleCommand s U = do
  liftIO $ putStrLn "Unknown Command" *> printNextStat s
  awaitCommand s

-- unfortunately this `foldrWithKey` calls toAscList, which throws away any
-- information the env had regarding the assignment order of varaibles
-- if this is necessary we could go back to the plain old `Map.showTree`
showMap :: Env -> String
showMap e = Map.foldrWithKey f "" (head e)
  where f key val result = result ++ (show key) ++ " : " ++ (show val) ++ "\n"

-- given our "stack" of states,
-- "peek" at the top to see current state
inspectCurrent :: Env -> String
inspectCurrent env = showMap env

-- union all past values for all the variables in the program,
-- then remove adjacent duplicates, we don't want to be shown that
-- a given vairable stayed the same from statement to statement
-- we only care when it changes
inspectHistory :: Env -> String
inspectHistory env = showMap [(Map.map remAdjDups $ Map.unionsWith (++) env)]

-- remove adjacent duplicates in list
remAdjDups :: (Eq a) => [a] -> [a]
remAdjDups [] = []
remAdjDups [x] = [x]
remAdjDups (x:y:xs) = if x == y
                      then remAdjDups(x:xs)
                      else x:remAdjDups(y:xs)

-- recursively check for variable use in expressions
allUsedByExpr :: Expr -> [Name]
allUsedByExpr (Var n) = [n]
allUsedByExpr (Not e) = allUsedByExpr e
allUsedByExpr (Mul e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (Div e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (Add e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (Sub e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (And e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (Or e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (Eq e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (Gt e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (Lt e0 e1) = allUsedByExpr e0 ++ allUsedByExpr e1
allUsedByExpr (Const _) = []

-- recurse down through the statements building a list of all used variables
allUsedVars :: Statement -> [Name]
allUsedVars (Assign _ e) = allUsedByExpr e
allUsedVars (Print e) = allUsedByExpr e
allUsedVars (If cond s0 s1) = allUsedByExpr cond ++ allUsedVars s0 ++ allUsedVars s1
allUsedVars (While cond s) = allUsedByExpr cond ++ allUsedVars s
allUsedVars (Seq s0 s1) = allUsedVars s0 ++ allUsedVars s1
allUsedVars (Try s0 s1) = allUsedVars s0 ++ allUsedVars s1

-- recurse down through the statements finding all assigned variables
allAssignedVars :: Statement -> [Name]
allAssignedVars (Assign n _) = [n]
allAssignedVars (While _ s) = allAssignedVars s
allAssignedVars (Seq s0 s1) = allAssignedVars s0 ++ allAssignedVars s1
allAssignedVars (If _ s0 s1) = allAssignedVars s0 ++ allAssignedVars s1
allAssignedVars (Try s0 s1) = allAssignedVars s0 ++ allAssignedVars s1
allAssignedVars (Print _) = []

-- static analysis runs before interpretation starts
unusedVars :: Statement -> [Name]
unusedVars s = (List.nub $ allAssignedVars s) List.\\ (List.nub $ allUsedVars s)

-- load the file from disk, parse out one big statement.
-- give completely no useful information to the user when the
-- read fails, don't tell them what they did wrong ¯\_(ツ)_/¯
-- print a list of all unused variables before the program starts
runInterpreter filename = do
  str <- readFile filename
  putStrLn "Unused variables:"
  putStrLn $ List.intercalate "\n" (unusedVars (read str :: Statement))
  run $ (read str :: Statement)

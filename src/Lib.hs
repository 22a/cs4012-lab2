{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Lib
    ( someFunc
    ) where

-- many things learned from http://catamorph.de/documents/Transformers.pdf

-- I want my own definition of lookup and I want to write my own function
-- named "print".
import Prelude hiding (lookup)

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

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
data Val = I Int | B Bool   -- values TODO: keep stack of values
  deriving (Eq, Show, Read)
type Env = [Map.Map Name [Val]] -- mapping from names to values

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

run :: Statement -> IO ()
run stat = do
  result <- runExceptT $ (runStateT $ exec stat) [Map.empty]
  case result of
    Right ( (), env ) -> return ()
    Left exn -> System.print ("Uncaught exception: "++exn)

someFunc = do
  str <- readFile "input.pm"
  run $ (read str :: Statement)

printNextStat :: Statement -> IO ()
printNextStat s = setSGR [SetColor Foreground Vivid Green] *> putStr "Next Stat: " *> putStr (show s) *> setSGR [] *> putStrLn ""

prompt :: IO String
prompt = setSGR [SetColor Foreground Vivid White, SetColor Background Vivid Magenta] *> putStr "λ>" *> setSGR [] *> putStr " " *> getLine

execRetain :: Statement -> Run ()
execRetain (Seq s1 s2) = do
  awaitCommand (Seq s1 s2)
execRetain s = do
  liftIO $ printNextStat s
  awaitCommand s

data Command = S
             | SB
             | IC
             | IH
             deriving (Read, Eq)


awaitCommand :: Statement -> Run ()
awaitCommand (Seq s1 s2) = do
  exec (Seq s1 s2)
awaitCommand stat = do
  line <- liftIO $ prompt
  handleCommand stat (read line :: Command)

handleCommand :: Statement -> Command -> Run ()
handleCommand s S = do
  exec s

handleCommand s SB = do
  exec s

handleCommand s IC = do
  st <- get
  liftIO $ putStrLn (inspectCurrent st)
  awaitCommand s

handleCommand s IH = do
  st <- get
  liftIO $ putStrLn (inspectHistory st)
  awaitCommand s

inspectCurrent :: Env -> String
inspectCurrent env = Map.showTree (head env)

inspectHistory :: Env -> String
inspectHistory env = Map.showTree $ Map.unionsWith List.union env

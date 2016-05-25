---
title:  COMBONAD
tags: monads, tutorials, haskell
date: 2009-04-09
author: haskellx
description: Demonstration of combining different permutations of State, Error, and IO monads
comments: true
toc: true
---
[original article](http://fpmatters.blogspot.am/2009/04/beware-monads.html)


{-
A demonstration of combining different permutations of State, Error, and IO monads.
Shows the results of such combinations.
-}
``` haskell
module Main where

import System.IO

import Control.Monad.State
import Control.Monad.Error

-- runErrorT :: (m (Either e a))
-- runStateT :: (s -> m (a, s))

-- The state for state monad examples

data TestState = TestState { value :: Int }
deriving (Show)

data TestState2 = TestState2 { state2 :: String }
deriving (Show)

-- Must use this form of signature rather than "State TestState ()"
increment :: (MonadState TestState m) => m ()
increment = modify $ \ s -> TestState (1 + (value s))

increment2 :: (MonadState TestState2 m) => m ()
increment2 = modify $ \ s -> TestState2 ('x' : (state2 s))

-- The error type for error monad examples

data TestError
= TestError String
deriving (Show)

instance Error TestError where
noMsg = TestError "<unknown error>"
strMsg s = TestError s

```
## Main
``` haskell
-- Main

main :: IO ()
main =
do
testStateError
testErrorState
testStateState
testState
testStateIO
testErrorIO
testStateErrorIO
testErrorStateIO

```
## Lifted IO
``` haskell
-- Lifted IO

cout :: (MonadIO m) => String -> m ()
cout = liftIO . hPutStr stdout

coutLn :: (MonadIO m) => String -> m ()
coutLn = liftIO . hPutStrLn stdout
```

## State
``` haskell
-- MONAD: State

runState' :: State TestState ()
runState' = do
increment
increment

testState :: IO ()
testState =
do
putStrLn "-- State"
r <- return $ runState runState' $ TestState 0
putStrLn $ show r
```
## StateState
``` haskell
-- COMBONAD: StateState

type StateState a = StateT TestState (State TestState2) a

runStateState :: StateState ()
runStateState =
do
increment
--increment2

testStateState :: IO ()
testStateState =
do
putStrLn "-- StateState"
r <- return $ runStateT (runState runStateState (TestState 0)) (TestState2 "")
putStrLn $ show r
```
## StateError
``` haskell
-- COMBONAD: StateError

type StateError a = StateT TestState (Either TestError) a

runStateError1 :: StateError ()
runStateError1 =
do
increment
throwError $ noMsg
increment

runStateError2 :: StateError ()
runStateError2 =
do
increment
increment

testStateError :: IO ()
testStateError =
do
putStrLn "-- StateError"
r <- return $ runStateT runStateError1 $ TestState 0
putStrLn $ show r
r <- return $ runStateT runStateError2 $ TestState 0
putStrLn $ show r
```
## ErrorState
``` haskell
-- COMBONAD: ErrorState

type ErrorState a = ErrorT TestError (State TestState) a

runErrorState1 :: ErrorState ()
runErrorState1 =
do
increment
throwError $ TestError "biffed"
increment

runErrorState2 :: ErrorState ()
runErrorState2 =
do
increment
increment

testErrorState :: IO ()
testErrorState =
do
putStrLn "-- ErrorState"
r <- return $ runState (runErrorT runErrorState1) $ TestState 0
putStrLn $ show r
r <- return $ runState (runErrorT runErrorState2) $ TestState 0
putStrLn $ show r
```
## StateIO
``` haskell
-- COMBONAD: StateIO

type StateIO a = StateT TestState IO a

runStateIO :: StateIO ()
runStateIO =
do
coutLn $ "hola, amigos"
increment

testStateIO :: IO ()
testStateIO =
do
putStrLn "-- StateIO"
r <- runStateT runStateIO $ TestState 0
putStrLn $ show r
```
## ErrorIO
``` haskell
-- COMBONAD: ErrorIO

type ErrorIO a = ErrorT TestError IO a

runErrorIO1 :: ErrorIO ()
runErrorIO1 =
do
coutLn $ "step 1"
coutLn $ "step 2"

runErrorIO2 :: ErrorIO ()
runErrorIO2 =
do
coutLn $ "step 1"
throwError $ strMsg "biffed"
coutLn $ "step 2"

testErrorIO :: IO ()
testErrorIO =
do
putStrLn "-- ErrorIO"
r <- runErrorT runErrorIO1
putStrLn $ show r
r <- runErrorT runErrorIO2
putStrLn $ show r
```
## StateErrorIO
``` haskell
-- COMBONAD: StateErrorIO

type StateErrorIO a = StateT TestState (ErrorT TestError IO) a

runStateErrorIO1 :: StateErrorIO ()
runStateErrorIO1 =
do
coutLn $ "with error"
increment
throwError $ TestError "biffed"
increment

runStateErrorIO2 :: StateErrorIO ()
runStateErrorIO2 =
do
coutLn $ "without error"
increment
increment

testStateErrorIO :: IO ()
testStateErrorIO =
do
putStrLn "-- StateErrorIO"
r <- runErrorT $ runStateT runStateErrorIO1 $ TestState 0
putStrLn $ show r
r <- runErrorT $ runStateT runStateErrorIO2 $ TestState 0
putStrLn $ show r
```
## ErrorStateIO
``` haskell
-- COMBONAD: ErrorStateIO

type ErrorStateIO a = ErrorT TestError (StateT TestState IO) a

runErrorStateIO1 :: ErrorStateIO ()
runErrorStateIO1 =
do
coutLn $ "with error"
increment
throwError $ TestError "biffed"
increment

runErrorStateIO2 :: ErrorStateIO ()
runErrorStateIO2 =
do
coutLn $ "without error"
increment
increment

testErrorStateIO :: IO ()
testErrorStateIO =
do
putStrLn "-- ErrorStateIO"
r <- runStateT (runErrorT runErrorStateIO1) $ TestState 0
putStrLn $ show r
r <- runStateT (runErrorT runErrorStateIO2) $ TestState 0
putStrLn $ show r
```
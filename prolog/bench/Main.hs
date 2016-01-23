{-# LANGUAGE OverloadedStrings, QuasiQuotes, StandaloneDeriving #-}
module Main (main) where

import Prolog (Program, consult, parseQuery ,resolve, VariableName(..), Term(..),var,withTrace)
import Quote (ts)

import System.Environment (getArgs)
import Control.DeepSeq (deepseq, NFData(rnf))

instance NFData Term where
   rnf (Struct a ts) = rnf a `seq` rnf ts
   rnf (Var v)       = rnf v
   rnf (Cut n)       = rnf n
   rnf Wildcard      = ()
instance NFData VariableName where
   rnf (VariableName i s) = rnf i `seq` rnf s



run program_file goal = do
  Right p <- consult program_file
  case parseQuery goal of
    Left  err -> putStrLn (show err)
    Right gs  -> do
      qs <- resolve p gs
      putStrLn $ show qs
      putStrLn $ "Number of solutions: " ++ show (length qs)


main = do
   args <- getArgs
   let n = case args of { [] -> 6; (x:_) -> read x }
   putStrLn "Starting benchmark..."
--   qs <- resolve p [ts|queens($n,Qs)|]
   -- run  "../bench/queens.pl" "queens(5,Qs)"
   run  "../bench/fig02_12.pl" "[a,b,c] = .(a,.(b,.(c,[])))"

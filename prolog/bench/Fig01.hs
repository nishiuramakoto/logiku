{-# LANGUAGE OverloadedStrings, QuasiQuotes, StandaloneDeriving #-}
module Fig01_08 where

import Prolog (consult, resolve, VariableName(..), Term(..))
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



bench prog goals = do
   Right p <- consult prog
   qs <- resolve p goals
   print $ "unifier=" ++  show qs
   putStrLn $ qs `deepseq` "Number of solutions: " ++ show (length qs)

test_12  = bench "fig01_12.pl"
           [ts|solution(L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16)|]

test_13  = bench "fig01_13.pl"
           [ts|colors(IT,SI,HR,CH,AT,HU,DE,SK,CZ,PL,SEA)|]

test_14  = bench "fig01_14.pl"
           [ts|schedule(Ta,A1,A2,Tb,B1,B2,Td,D1,D2)|]

test_15  = bench "fig01_14.pl"
           [ts|schedule(T,A1,A2,T,B1,B2,T,D1,D2)|]

{-# LANGUAGE FlexibleInstances #-}

-- A benchmark of shift/reset: Filinski's representing non-determinism monads
--
--  The benchmark is taken from Sec 6.1 of
--    Martin Gasbichler, Michael Sperber: Final Shift for Call/cc: Direct
--    Implementation of Shift and Reset, ICFP'02, pp. 271-282. 
--    http://www-pu.informatik.uni-tuebingen.de/users/sperber/papers/shift-reset-direct.pdf
-- This code is a straightforward translation of bench_nondet.ml
--
-- This is a micro-benchmark: it is very non-determinism-intensive. It is
-- *not* representative: the benchmark does nothing else but
-- concatenates lists. The List monad does this directly; whereas
-- continuation monads do the concatenation with more overhead (e.g.,
-- building the closures representing continuations). Therefore,
-- the List monad here outperforms all other implementations of 
-- non-determinism.
-- It should be stressed that the delimited control is optimized
-- for the case where control operations are infrequent, so we pay
-- as we go. The use of the delimited control operators is more
-- expensive, but the code that does not use delimited control does not
-- have to pay anything for delimited control. 
-- Again, in the present micro-benchmark, there is hardly any code that
-- does not use non-determinism, so the overhead of delimited control
-- is very noticeable. That is why this benchmark is good at estimating
-- the overhead of different implementations of delimited control.

-- To compile this code
-- ghc --make -O2 -main-is Bench_nondet.main_list5 Bench_nondet.hs
-- To run this code
-- GHCRTS="-tstderr" /usr/bin/time ./Bench_nondet

module Bench_nondet where

-- import Control.Monad.CC.CCExc
import Control.Monad.CC.CCCxe
-- import Control.Monad.CC.CCRef

import Data.List (sort)
import Control.Monad.Identity
import Control.Monad (MonadPlus(..), liftM2, msum)
-- import System.CPUTime

-- Small language with non-determinism: just like the one in our DSL-WC paper

int :: MonadPlus repr => Int -> repr Int
int x = return x

add :: MonadPlus repr => repr Int -> repr Int -> repr Int
add xs ys = liftM2 (+) xs ys

lam :: MonadPlus repr => (repr a -> repr b) -> repr (a -> repr b)
lam f = return $ f . return

app :: MonadPlus repr => repr (a -> repr b) -> (repr a -> repr b)
app xs ys = do {x <- xs; y <- ys; x y}

amb :: MonadPlus repr => [repr Int] -> repr Int
amb = msum

-- Benchmark cases

test_ww :: MonadPlus repr => repr Int
test_ww = 
 let f = lam (\x ->
	      add (add x (amb [int 6, int 4, int 2, int 8])) 
	                 (amb [int 2, int 4, int 5, int 4, int 1]))
 in f `app` amb [int 0, int 2, int 3, int 4, int 5, int 32]

ww_answer = 
 sort [8, 10, 11, 10, 7, 6, 8, 9, 8, 5, 4, 6, 7, 6, 3, 10, 12, 13,
       12, 9, 10, 12, 13, 12, 9, 8, 10, 11, 10, 7, 6, 8, 9, 8, 5, 12, 14, 15,
       14, 11, 11, 13, 14, 13, 10, 9, 11, 12, 11, 8, 7, 9, 10, 9, 6, 13, 15,
       16, 15, 12, 12, 14, 15, 14, 11, 10, 12, 13, 12, 9, 8, 10, 11, 10, 7,
       14, 16, 17, 16, 13, 13, 15, 16, 15, 12, 11, 13, 14, 13, 10, 9, 11, 12,
       11, 8, 15, 17, 18, 17, 14, 40, 42, 43, 42, 39, 38, 40, 41, 40, 37, 36,
       38, 39, 38, 35, 42, 44, 45, 44, 41]

-- Real benchmark cases

test_www :: MonadPlus repr => repr Int
test_www = 
 let f = lam (\x ->
	      add (add x (amb [int 6, int 4, int 2, int 8])) 
	                 (amb [int 2, int 4, int 5, int 4, int 1]))
 in f `app` (f `app` amb [int 0, int 2, int 3, int 4, int 5, int 32])

test_wwww :: MonadPlus repr => repr Int
test_wwww = 
 let f = lam (\x ->
	      add (add x (amb [int 6, int 4, int 2, int 8])) 
	                 (amb [int 2, int 4, int 5, int 4, int 1]))
 in f `app` (f `app` (f `app` amb [int 0, int 2, int 3, int 4, int 5, int 32]))

test_w5 :: MonadPlus repr => repr Int
test_w5 = 
 let f = lam (\x ->
	      add (add x (amb [int 6, int 4, int 2, int 8])) 
	                 (amb [int 2, int 4, int 5, int 4, int 1]))
 in f `app` (f `app` 
     (f `app` (f `app` amb [int 0, int 2, int 3, int 4, int 5, int 32])))


-- Different implementations of our language (MonadPlus)

-- The List monad: Non-determinism monad as a list of successes

run_list :: [Int] -> [Int]
run_list = id

testl1 = (==) [101, 201, 102, 202] . run_list $
	 add (amb [int 1, int 2]) (amb [int 100, int 200])

testl2 = ww_answer == sort (run_list test_ww)


-- CPS-monad, implemented by hand; it must be quite efficient therefore
newtype CPS a = CPS{unCPS:: (a -> [Int]) -> [Int]}

instance Monad CPS where
    return x = CPS $ \k -> k x
    m >>= f  = CPS $ \k -> unCPS m (\a -> unCPS (f a) k)

instance MonadPlus CPS where
    mzero = CPS $ \_ -> []
    mplus m1 m2 = CPS $ \k -> unCPS m1 k ++ unCPS m2 k

run_cps :: CPS Int -> [Int]
run_cps m = unCPS m (\x -> [x])


testc1 = (==) [101, 201, 102, 202] . run_cps $
	 add (amb [int 1, int 2]) (amb [int 100, int 200])

testc2 = ww_answer == sort (run_cps test_ww)

-- CCEx monad

-- Not a very optimal implementation of mplus (a tree would be better)
-- But is suffices as a benchmark of different implementations of CC
instance Monad m => MonadPlus (CC (PS [Int]) m) where
    mzero = abortP ps (return [])
    mplus m1 m2 = takeSubCont ps (\k ->
		     liftM2 (++)
		       (pushPrompt ps (pushSubCont k m1))
		       (pushPrompt ps (pushSubCont k m2)))

run_dir :: CC (PS [Int]) Identity Int -> [Int]
run_dir m = runIdentity . runCC $
	    pushPrompt ps (m >>= return . (:[]))


testd1 = (==) [101, 201, 102, 202] . run_dir $
	 add (amb [int 1, int 2]) (amb [int 100, int 200])

testd2 = ww_answer == sort (run_dir test_ww)

{-
-- CCRef monad

-- Need a reader-monad layer to propagate the prompt
newtype CCR m a = CCR{unCCR :: Prompt m [Int] -> CC m a}

instance Monad m => Monad (CCR m) where
    return x = CCR $ \_ -> return x
    m >>= f  = CCR $ \p -> unCCR m p >>= \v -> unCCR (f v) p


-- Not a very optimal implementation of mplus (a tree would be better)
-- But is suffices as a benchmark of different implementations of CC
instance (Monad m, Mutation m) => MonadPlus (CCR m) where
    mzero = CCR $ \p -> abortP p (return [])
    mplus m1 m2 = CCR $ \p ->
		   takeSubCont p (\k ->
		     liftM2 (++)
		       (pushDelimSubCont k (unCCR m1 p))
		       (pushDelimSubCont k (unCCR m2 p)))

run_ref :: CCR IO Int -> IO [Int]
run_ref m = runCC $ do
	    p <- newPrompt
	    pushPrompt p (unCCR m p >>= return . (:[]))

testr1 = ((return . ((==) [101, 201, 102, 202])) =<<) . run_ref $
	 add (amb [int 1, int 2]) (amb [int 100, int 200])

testr2 = do
	 r <- run_ref test_ww
	 return $ ww_answer == sort r

main_ref5io = do
	      l <- run_ref test_w5
	      print $ length l == 960000
-}


-- Benchmarks themselves

main_list3 = print $ 2400   == (length . run_list $ test_www)
main_list4 = print $ 48000  == (length . run_list $ test_wwww)
main_list5 = print $ 960000 == (length . run_list $ test_w5)

main_cps3 = print $ 2400   == (length . run_cps $ test_www)
main_cps4 = print $ 48000  == (length . run_cps $ test_wwww)
main_cps5 = print $ 960000 == (length . run_cps $ test_w5)

-- We expect the direct implementation to be slower since CC is the transformer,
-- whereas CPS is not. The latter is hand-written for a specific answer-type.
main_dir3 = print $ 2400   == (length . run_dir $ test_www)
main_dir4 = print $ 48000  == (length . run_dir $ test_wwww)
main_dir5 = print $ 960000 == (length . run_dir $ test_w5)

-- Instantiate CC to the IO as the base monad, attempting to quantify the
-- effect of the Identity transformer
main_dir5io = do
	      l <- runCC $ pushPrompt ps (test_w5 >>= return . (:[]))
	      print $ length l == 960000

{- Median of 5 runs

main_list5
<<ghc: 186526764 bytes, 356 GCs, 619182/1156760 avg/max bytes residency (3 samples), 4M in use, 0.00 INIT (0.00 elapsed), 0.25 MUT (0.25 elapsed), 0.06 GC (0.06 elapsed) :ghc>>
        0.30 real         0.30 user         0.00 sys

main_cps5
<<ghc: 231580040 bytes, 442 GCs, 4017/4104 avg/max bytes residency (24 samples), 2M in use, 0.00 INIT (0.00 elapsed), 0.28 MUT (0.28 elapsed), 0.31 GC (0.33 elapsed) :ghc>>
        0.60 real         0.58 user         0.01 sys

main_dir5 (CCExc implementation)
<<ghc: 780415108 bytes, 1489 GCs, 10459973/39033060 avg/max bytes residency (14 samples), 110M in use, 0.00 INIT (0.00 elapsed), 1.30 MUT (1.32 elapsed), 2.92 GC (3.14 elapsed) :ghc>>
        4.48 real         4.22 user         0.24 sys

main_dir5io (CCExc implementation)
<<ghc: 1148031880 bytes, 2190 GCs, 10339954/38941944 avg/max bytes residency (14 samples), 108M in use, 0.00 INIT (0.00 elapsed), 2.15 MUT (2.20 elapsed), 3.04 GC (3.24 elapsed) :ghc>>
        5.45 real         5.18 user         0.21 sys


main_dir5 (CCCxe implementation)
./Bench_nondet +RTS -tstderr 
True
<<ghc: 991065016 bytes, 1891 GCs, 10473968/38790660 avg/max bytes residency (14 samples), 110M in use, 0.00 INIT (0.00 elapsed), 1.45 MUT (1.49 elapsed), 2.99 GC (3.20 elapsed) :ghc>>
        4.70 real         4.44 user         0.23 sys

main_dir5io (CCCxe implementation)
./Bench_nondet +RTS -tstderr 
True
<<ghc: 991065412 bytes, 1891 GCs, 10364029/37920012 avg/max bytes residency (14 samples), 109M in use, 0.00 INIT (0.00 elapsed), 1.46 MUT (1.50 elapsed), 2.99 GC (3.20 elapsed) :ghc>>
        4.72 real         4.44 user         0.23 sys

main_ref5io (without pushDelimSubCont)
./Bench_nondet +RTS -tstderr 
True
<<ghc: 19050261764 bytes, 36337 GCs, 10620542/49328200 avg/max bytes residency (16 samples), 123M in use, 0.00 INIT (0.00 elapsed), 61.45 MUT (62.70 elapsed), 6.06 GC (6.21 elapsed) :ghc>>
       68.94 real        67.51 user         1.03 sys


main_ref5io (with pushDelimSubCont)
./Bench_nondet +RTS -tstderr 
True
<<ghc: 5666546308 bytes, 10809 GCs, 10538302/46414760 avg/max bytes residency (14 samples), 114M in use, 0.00 INIT (0.00 elapsed), 16.27 MUT (16.68 elapsed), 3.65 GC (3.80 elapsed) :ghc>>
       20.50 real        19.92 user         0.46 sys

-}

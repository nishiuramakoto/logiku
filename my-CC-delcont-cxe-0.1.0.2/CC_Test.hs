{-# LANGUAGE DeriveDataTypeable #-}

-- Tests of the CC Transformer operations: CCExe

module CC_testsT where

import Control.Monad.CC.CCCxe
-- import Control.Monad.CC.CCExc
import Control.Monad.Trans
import Data.Typeable

expect ve vp = if ve == vp then putStrLn $ "expected answer " ++ (show ve)
	          else error $ "expected " ++ (show ve) ++
		               ", computed " ++ (show vp)

test1 = runCC (return 1 >>= (return . (+ 4))) >>= expect 5
-- 5

doall = sequence_ [test1, test2, test3, test3', test3'', 
		   test4, test5, test41, test5''1, test5''21, test5''22,
		   test5''3, test54,
		   test6, test7, test7', test7'',
		   testls, testls0, testls01, testlc, testlc', testlc1
		  ]
-- test3''' should raise an error

incr :: Monad m => Int -> m Int -> m Int
incr n m = m >>= return . (n +)

test2 = (expect 9 =<<) . runCC $
  incr 4 . pushPrompt ps $ pushPrompt ps (return 5)
-- 9

test3 = (expect 9 =<<) . runCC $
  incr 4 . pushPrompt ps $ (incr 6 $ abortP ps (return 5))

test3' = (expect 9 =<<) . runCC $
  incr 4 . pushPrompt ps . pushPrompt ps $ (incr 6 $ abortP ps (return 5))

test3'' = (expect 27 =<<) . runCC $
  incr 20 . pushPrompt ps $ 
	 do
	 v1 <- pushPrompt ps (incr 6 $ abortP ps (return 5))
	 v2 <- abortP ps (return 7)
	 return $ v1 + v2 + 10

test3''' = (print =<<) . runCC $ do
	       v <- pushPrompt ps $ 
		 do
		 v1 <- pushPrompt ps (incr 6 $ abortP ps (return 5))
		 v2 <- abortP ps (return 7)
		 return $ v1 + v2 + 10
	       v <- abortP ps (return 9)
	       return $ v + 20
-- error

test4 = (expect 35 =<<) . runCC $
  incr 20 . pushPrompt ps $
	 incr 10 . takeSubCont ps $ \sk -> 
	                 pushPrompt ps (pushSubCont sk (return 5))

test41 = (expect 35 =<<) . runCC $ 
  incr 20 . pushPrompt ps $ 
    incr 10 . takeSubCont ps $ \sk -> 
	pushSubCont sk (pushPrompt ps (pushSubCont sk (abortP ps (return 5))))


-- Danvy/Filinski's test
--(display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
--; --> 117

test5 = (expect 117 =<<) . runCC $
  incr 10 . pushPrompt ps $
     incr 2 . shiftP ps $ \sk -> incr 100 $ sk =<< (sk 3)
-- 117

-- multi-prompt tests

-- Testing prompt flavor P2

test5''1 = (expect 115 =<<) . runCC $
  incr 10 . pushPrompt p2L $ 
     incr 2 . (id =<<) . shiftP p2L $ \sk -> 
		incr 100 $ (sk (pushPrompt p2R
				  (sk (sk (abortP p2R (return 3))))))

-- Testing prompt flavor PP

-- Here, p1 and p0 have the same type, and so p0 is actually the same as p1
test5''21 = (expect 117 =<<) . runCC $
  incr 10 . pushPrompt p0 $ 
     incr 2 . (id =<<) . shiftP p0 $ \sk -> 
		incr 100 $ (sk (pushPrompt p1
				  (sk (sk (abortP p1 (return 3))))))
 where p0 = pp `as_prompt_type` (0::Int)
       p1 = pp

-- Now, p1 and p0 have different types
newtype NInt = NInt{unNInt :: Int} deriving Typeable
test5''22 = (expect 115 =<<) . runCC $
  incr 10 . pushPrompt p0 $ 
     incr 2 . (id =<<) . shiftP p0 $ \sk -> 
		incr 100 $ (sk (lunNInt (pushPrompt p1
				 (lNInt 
				  (sk (sk (abortP p1 (return (NInt 3)))))))))
 where p0 = pp `as_prompt_type` (0::Int)
       p1 = pp
       lunNInt m = m >>= return . unNInt
       lNInt   m = m >>= return . NInt

-- Testing prompt flavor PD
-- p0 and p1 have the same type, but are different
test5''3 = (expect 115 =<<) . runCC $
  incr 10 . pushPrompt p0 $ 
     incr 2 . (id =<<) . shiftP p0 $ \sk -> 
		incr 100 $ (sk (pushPrompt p1
				  (sk (sk (abortP p1 (return 3))))))
 where p0 = newPrompt 0 `as_prompt_type` (0::Int)
       p1 = newPrompt 1

test54 = (expect 117 =<<) . runCC $
  incr 10 . pushPrompt p0 $ 
     incr 2 . (id =<<) . shiftP p0 $ \sk -> 
		incr 100 $ (sk (pushPrompt p1
				  (sk (sk (abortP p0 (return 3))))))
 where p0 = newPrompt 0 `as_prompt_type` (0::Int)
       p1 = newPrompt 1

test6 = (expect 15 =<<) . runCC $
  let pushtwice sk = pushSubCont sk (pushSubCont sk (return 3)) in
  incr 10 . pushPrompt p1 $ 
     incr 1 . pushPrompt p2 $ takeSubCont p1 pushtwice
 where p1 = newPrompt 1 `as_prompt_type` (0::Int)
       p2 = newPrompt 2


-- The most difficult test. The difference between the prompts really matters
-- now
test7 = (expect 135 =<<) . runCC $
  let pushtwice sk = pushSubCont sk (pushSubCont sk 
					      (takeSubCont p2
					       (\sk2 -> pushSubCont sk2
						(pushSubCont sk2 (return 3)))))
  in
  incr 100 . pushPrompt p1 $
    incr 1 . pushPrompt p2 $
     incr 10 . pushPrompt p3 $ (takeSubCont p1 pushtwice)
 where p1 = newPrompt 1 `as_prompt_type` (0::Int)
       p2 = newPrompt 2
       p3 = newPrompt 3
-- 135

test7' = (expect 135 =<<) . runCC $
  let pushtwice f = f (f (shiftP p2 (\f2 -> f2 =<< (f2 3))))
  in
  incr 100 . pushPrompt p1 $
    incr 1 . pushPrompt p2 $
     incr 10 . pushPrompt p3 $ (shiftP p1 pushtwice >>= id)
 where p1 = newPrompt 1 `as_prompt_type` (0::Int)
       p2 = newPrompt 2
       p3 = newPrompt 3
-- 135

test7'' = (expect 135 =<<) . runCC $
  let pushtwice f = f (f (shift0P p2 (\f2 -> f2 =<< (f2 3))))
  in
  incr 100 . pushPrompt p1 $
    incr 1 . pushPrompt p2 $
     incr 10 . pushPrompt p3 $ (shift0P p1 pushtwice >>= id)
 where p1 = newPrompt 1 `as_prompt_type` (0::Int)
       p2 = newPrompt 2
       p3 = newPrompt 3


-- Checking shift, shift0, control 

testls = (expect ["a"] =<<) . runCC $
    pushPrompt ps (
		  do
		  let x = shiftP ps (\f -> f [] >>= (return . ("a":)))
		  xv <- x
		  shiftP ps (\_ -> return xv))


-- (display (prompt0 (cons 'a (prompt0 (shift0 f (shift0 g '()))))))
testls0 = (expect [] =<<) . runCC $
    pushPrompt ps (
       (return . ("a":)) =<< 
          (pushPrompt ps (shift0P ps (\_ -> (shift0P ps (\_ -> return []))))))
  
testls01 = (expect ["a"] =<<) . runCC $
    pushPrompt ps (
       (return . ("a":)) =<< 
          (pushPrompt ps 
	   (shift0P ps (\f -> f (shift0P ps (\_ -> return []))) >>= id)))
  

testlc = (expect [] =<<) . runCC $
    pushPrompt ps (
		  do
		  let x = controlP ps (\f -> f [] >>= (return . ("a":)))
		  xv <- x
		  controlP ps (\_ -> return xv))
  

testlc' = (expect ["a"] =<<) . runCC $
    pushPrompt ps (
		  do
		  let x = controlP ps (\f -> f [] >>= (return . ("a":)))
		  xv <- x
		  controlP ps (\g -> g xv))
-- ["a"]

testlc1 = (expect 2 =<<) . runCC $
    pushPrompt ps (do
		  takeSubCont ps (\sk -> 
				pushPrompt ps (pushSubCont sk (return 1)))
		  takeSubCont ps (\sk -> pushSubCont sk (return 2)))


-- traversing puzzle by Olivier Danvy

type DelimControl m a b = 
    Prompt (PS b) m b -> 
    ((a -> CC (PS b) m b) -> CC (PS b) m b) -> CC (PS b) m a

traverse :: Show a => DelimControl IO [a] [a] -> [a] -> IO ()
traverse op lst = (print =<<) . runCC $
  let visit [] = return []
      visit (h:t) = do
	            v <- op ps (\f -> f t >>= (return . (h:)))
	            visit v
  in pushPrompt ps (visit lst)


-- *CC_Refn> traverse shiftP [1,2,3,4,5]
-- [1,2,3,4,5]
-- *CC_Refn> traverse controlP [1,2,3,4,5]
-- [5,4,3,2,1]

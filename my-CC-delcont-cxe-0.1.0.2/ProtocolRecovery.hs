-- Example of CCCxe

-- Communicating with a server according to a fixed protocol;
-- reporting wrong responses to the supervisor;
-- permitting the recovery from protocol errors if the supervisor
-- decides that the error can be recovered by re-reading.
--
-- The problem was posed by magicloud.magiclouds in the message
-- http://www.haskell.org/pipermail/haskell-cafe/2011-January/088600.html


-- This code is interactive, letting the user act as a good or a bad
-- server
--
-- Sample interactions (after invoking 'main')
--   1,2        normal sequence
--   1,3,3,3,2  the same with debugging
--   3,1,3,2    the same with debugging
--   4          disconnect
--   1,4        disconnect
--   1,1        bad, bad server

module ProtocolRecovery where

-- Import (one of the) CC libraries
-- http://okmij.org/ftp/continuations/implementations.html#CC-monads

import Control.Monad.CC.CCCxe
import Control.Monad.Trans

-- Requests and responses
data Req = Req_hello | Req_who_are_you deriving Show
data Res = Res_hello | Res_name String | Res_debug | Res_disconnect
	 deriving (Eq, Show)


-- The error codes
data Err = Err_bad_resp Res		-- more alternatives could be added
	 deriving Show

-- The answer of a CC computation
-- The answer is either normal or an out-of-band message to the
-- supervisor, with an error code and the resumption.
-- We shall use the prompt flavor PS for the single answer-type CCAns

data CCAns = Done 			-- OK, finished
	   | Exc Err 			  -- exception with the code
	         (OurM CCAns)             -- resumption
		 -- cleanup procedure may be added

type OurM a = CC (PS CCAns) IO a	-- our monad

-- The main exchange with the server
exchange :: OurM ()
exchange = do
 send Req_hello
 readMsg >>= expect (== Res_hello)
 send Req_who_are_you
 readMsg >>= expect (\x -> case x of Res_name _ -> True; _ -> False)
 return ()


-- Check the response to see it matches our expectations
-- We report an unexpected response to the parent; 
-- if the parent tells us to continue, we re-read from the server
expect :: (Res -> Bool) -> Res -> OurM ()

expect pred msg | pred msg = return ()

expect pred msg = do
  shiftP ps (\k -> return $ Exc (Err_bad_resp msg) (k ()))
  readMsg >>= expect pred 		-- re-read, re-process



main = do
       connect
       runCC $ loop $ pushPrompt ps ( exchange >> return Done )
       disconnect
 where
 loop m = m >>= check

 check :: CCAns -> OurM ()
 check Done = return ()
 check (Exc err resum) = do
			 out ["Exception:", show err]
			 decide err resum

 -- decide what to do on error
 decide (Err_bad_resp Res_disconnect) _ = do
             out ["Aborting"]
	     return ()
 decide (Err_bad_resp Res_debug) resum = do
             out ["DEBUG"]
	     loop resum 		-- resuming
 decide _ _ = do
	      out ["Really bad response!"]
	      return () 		-- quitting


-- stubs

connect    = out ["Client Connected"]
disconnect = out ["Client Dis-connected"]

send :: MonadIO m => Req -> m ()
send req = out ["sending:",show req]

readMsg :: MonadIO m => m Res
readMsg = do
 out ["Enter response, as a number 1..4"]
 resp_code <- liftIO getLine >>= return . read
 case resp_code of
  1 -> return $ Res_hello
  2 -> return $ Res_name "dummy"
  3 -> return $ Res_debug
  4 -> return $ Res_disconnect
  _ -> out ["Bad code. Try again"] >> readMsg


out :: MonadIO m => [String] -> m ()
out = liftIO . putStrLn . unwords


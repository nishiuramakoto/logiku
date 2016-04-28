{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE UndecidableInstances  #-}
module User
       ( UserStorage(..)
       , UserStorageMap
       , YesodUserStorage(..)
       , readUserStorage
       , modifyUserStorage_
       , modifyUserStorage
--       , lookupUserStorage
  ) where

import Import.NoFoundation
-- import Yesod.Auth
-- import Data.Map(Map)
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Graph.Inductive.Graph
import           System.IO(hSetBuffering,BufferMode(..))

import CCGraph

data UserStorage master = UserStorage { usCCGraph ::  CCGraph master
                                      , usCCRoot :: CCNode
                                      , usCCNodeQueue :: Seq.Seq (LocalTime,CCNode)
                                      }

deriving instance (Show (CCGraph master)) => Show (UserStorage master)

type UserStorageMap master = Map (Maybe (AuthId master)) (UserStorage master)


class YesodUserStorage master where
  getUserStorage :: master -> MVar (UserStorageMap master)

  newUserStorage :: IO (MVar (UserStorageMap master))
  newUserStorage = newMVar Map.empty

instance Default (UserStorage master) where
--  def = UserStorage (insNode (0, CCNodeLabel dt Nothing) CCGraph.empty) 0 Seq.empty
--        where dt = ut1ToLocalTime 0 (ModJulianDate 0)
  def = UserStorage  CCGraph.empty 0 Seq.empty

readUserStorage :: (YesodAuth master, Ord (AuthId master) , YesodUserStorage master)
                   => HandlerT master IO (UserStorage master)
readUserStorage = do
  master <- getYesod
  -- maid   <- maybeAuthId
  maid <- return Nothing

  let mv = getUserStorage master

  map' <- liftIO $ readMVar mv
  let mmv' = Map.lookup maid map'
  case mmv' of
    Just mv' -> return mv'
    Nothing  -> do
      let mv'   = def
      let map'' = Map.insert maid mv' map'
      _ <- liftIO $ swapMVar mv map''
      return def

modifyUserStorage_ :: (YesodAuth master, Ord (AuthId master), YesodUserStorage master)
                    => (Maybe (UserStorage master) -> HandlerT master IO (Maybe (UserStorage master)))
                    ->  HandlerT master IO ()
modifyUserStorage_  f =  modifyUserStorage f'
  where
    f' a = do mus <- f a
              return (mus,())

modifyUserStorage :: (YesodAuth master, Ord (AuthId master), YesodUserStorage master)
                   => (Maybe (UserStorage master) -> HandlerT master IO (Maybe (UserStorage master), b))
                   -> HandlerT master IO b
modifyUserStorage  f = do
--  readUserStorage >>= putStrLn . T.pack . show
  -- liftIO $ hSetBuffering stdout NoBuffering
  -- liftIO $ hSetBuffering stderr NoBuffering
  -- putStrLn $ "-----------modifyUserStorage-------------"
  --x <- fst <$> f Nothing
  --putStrLn . T.pack $ "Nothing -> " ++ show  x

  master <- getYesod
  -- maid   <- maybeAuthId
  maid <- return Nothing
  let mv = getUserStorage master
  map' <- liftIO $ takeMVar mv
  let mmv' = Map.lookup maid map'
  (mmv,b) <- f' mmv'
  let map'' = case mmv of
        Just mv' -> Map.insert maid mv' $ Map.delete maid map'
        Nothing  -> Map.delete maid map'
--        Map.update (const mmv) maid map'

  putMVar mv $! map''

  -- liftIO $ maybe (putStrLn "Nothing") dumpMVar mmv'
  -- putStrLn $ "-->"
  -- liftIO $ maybe (putStrLn "Nothing") dumpMVar (Map.lookup maid map'')
  -- liftIO $ maybe (putStrLn "Nothing") dumpMVar mmv

--  readUserStorage >>= putStrLn . T.pack .  show
  return b

  where
    f' (Just mv) = do
      let us = mv
      (mus,b) <- f (Just us)
      case mus of
        Just us' -> do let mv = us'
                       return (Just mv, b)
        Nothing  -> return (Nothing, b)

    f' Nothing = do
      (mus,b) <- f Nothing
      case mus of
        Just us' -> do let mv =  us'
                       return (Just mv, b)
        Nothing  -> return (Nothing, b)

-- lookupUserStorage :: (YesodAuth master, Ord (AuthId master), YesodUserStorage master) => HandlerT master IO (UserStorage master)
-- lookupUserStorage = do
--   map_ <-  readUserStorage
--   maid <- maybeAuthId
--   let mus =  Map.lookup maid map_
--   case mus of
--     Just us -> return us
--     Nothing -> return def

dumpMVar :: Show a => MVar a -> IO ()
dumpMVar mv = do a <- readMVar mv
                 putStrLn $ T.pack $ show a

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

import             Data.Time.LocalTime
import             Data.Graph.Inductive.Graph

import CCGraph

data UserStorage master = UserStorage { usCCGraph ::  CCGraph master
                                      , usCCRoot :: CCNode
                                      , usCCNodeQueue :: Seq.Seq (LocalTime,CCNode)
                                      }

type UserStorageMap master = Map (Maybe (AuthId master)) (MVar (UserStorage master))


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
  maid   <- maybeAuthId
  let mv = getUserStorage master

  map' <- liftIO $ readMVar mv
  let mmv' = Map.lookup maid map'
  case mmv' of
    Just mv' -> liftIO $ readMVar mv'
    Nothing  -> do
      mv' <- newMVar def
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
  master <- getYesod
  maid   <- maybeAuthId
  let mv = getUserStorage master
  map' <- liftIO $ takeMVar mv

  (mmv,b) <- f' $  Map.lookup maid map'
  let map'' = Map.update (const mmv) maid map'

  putMVar mv $! map''
  return b

  where
    f' (Just mv) = do
      us <- takeMVar mv
      (mus,b) <- f (Just us)
      case mus of
        Just us' -> putMVar mv us' >> return (Just mv, b)
        Nothing  -> return (Nothing, b)

    f' Nothing = do
      (mus,b) <- f Nothing
      case mus of
        Just us' -> do mv <- newMVar us'
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

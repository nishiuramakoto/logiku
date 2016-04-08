module User
       ( UserStorage(..)
       , UserStorageMap(..)
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
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import CCGraph

data UserStorage    master = UserStorage (MVar (UserStorageContent master))

data UserStorageContent master = UserStorageContent { uscCCGraph ::  CCGraph master
                                                    , uscCCRoot :: CCNode
                                                    , uscCCNodeQueue :: Seq.Seq (LocalTime,CCNode)
                                                    }
type UserStorageMap master = Map (Maybe (AuthId master)) (UserStorage master)


class YesodUserStorage master where
  getUserStorage :: master -> MVar (UserStorageMap master)

  newUserStorage :: IO (MVar (UserStorageMap master))
  newUserStorage = newMVar Map.empty


readUserStorage :: YesodUserStorage master => HandlerT master IO (UserStorageMap master)
readUserStorage = do
  master <- getYesod
  let mv = getUserStorage master
  liftIO $ readMVar mv

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

  map_ <- liftIO $ takeMVar mv
  let mus =  Map.lookup maid map_
  (mus',b) <- f mus
  let map' = Map.alter (const mus') maid map_
  putMVar mv $! map'
  return b

-- lookupUserStorage :: (YesodAuth master, Ord (AuthId master), YesodUserStorage master) => HandlerT master IO (UserStorage master)
-- lookupUserStorage = do
--   map_ <-  readUserStorage
--   maid <- maybeAuthId
--   let mus =  Map.lookup maid map_
--   case mus of
--     Just us -> return us
--     Nothing -> return def

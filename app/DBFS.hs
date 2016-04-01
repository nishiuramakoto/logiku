{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables,
             GADTs,
             TypeFamilies

  #-}

module DBFS
       ( DbfsError(..)
       , Perm(..)
       , UserModOption(..)
       , ChownOption(..)
       , ChmodOption(..)
       , mkdir
       , touch , touchAt
       , fopen, opendir
--       , mkdir' , touch'
       , isPrivileged
       , isGroupOwnerOf
       , belongs
       , userExistsBy
       , getUserDisplayName
       , getDirectoryUserId
       , getDirectoryGroups
       , getUser

       , chown
--       , chownDirectory
--       , chownFile
       , chmodDirectory
       , chmodFile

       , su , suGuest
       , useradd
       , usermod
       , userdel

       , groupadd
         -- , groupmod
       , groupdel

       , isDirectoryOwnerReadableBy
       , isDirectoryOwnerWritableBy
       , isDirectoryOwnerExecutableBy

       , isDirectoryGroupReadableBy
       , isDirectoryGroupWritableBy
       , isDirectoryGroupExecutableBy

       , isDirectoryEveryoneReadableBy
       , isDirectoryEveryoneWritableBy
       , isDirectoryEveryoneExecutableBy

       , isDirectoryReadableBy
       , isDirectoryWritableBy
       , isDirectoryExecutableBy


       , isFileOwnerReadableBy
       , isFileOwnerWritableBy
       , isFileOwnerExecutableBy

       , isFileGroupReadableBy
       , isFileGroupWritableBy
       , isFileGroupExecutableBy

       , isFileEveryoneReadableBy
       , isFileEveryoneWritableBy
       , isFileEveryoneExecutableBy

       , isFileReadableBy
       , isFileWritableBy
       , isFileExecutableBy

       , lsDirectory
       , lsFile
--       , llFile
       , findDirectory
       , findFile , findFile'
       , findExecutableFile

       , readDirectory
       , readFile

       , writeDirectory
       , writeFile

       , rmdir
       , rm_rf
       , unlink,rm

         -- , programTagsAdd
         -- , programTagsDel
         -- , programTagsFind

         -- , goalTagsAdd
         -- , goalTagsDel
         -- , goalTagsFind

         -- TODO User accounting functions

  ) where


import             Import.NoFoundation   hiding ((==.), (>=.) ,(||.)
                                                , on , Value , update , (=.) , forM_ , delete
                                                , readFile, writeFile , FileInfo
                                                , isNothing
                                                )
import  qualified  Import.NoFoundation as I
import             Database.Esqueleto hiding(insert)
import  qualified  Database.Persist as P
import  qualified  Data.Text as T
import             Data.Foldable hiding(null, mapM_)
import             Control.Monad.Trans.Either
import             Constructors
import             Data.List (nubBy)
import  qualified  Data.Function as F

-- Rule: Should never throw in any way from the functions in this module.
--       Always test the validity of insert/update/delete etc. and return 'DbfsError' appropriately.


data DbfsError = DirectoryDoesNotExist Text
               | FileDoesNotExist Text
               | UserDoesNotExist   Text
               | UserAlreadyExists Text
               | GroupAlreadyExists Text
               | GroupDoesNotExist  Text
               | AlreadyGroupMember Text
               | NotAGroupMember  Text
               | DirectoryAlreadyExists Text
               | FileAlreadyExists Text
               | DirectoryNonEmpty Text
               | PermissionError  Text
               | DuplicateDirectoryGroups Text
               | DuplicateFileGroups Text
               | AlreadyOwner Text
               | NotAnOwner Text
               | EntryAlreadyExists Text
               | EntryDoesNotExist Text
               | EntityDoesNotExist Text
               | EntityCouldNotBeOpened Text
               | DuplicateOwnerGroups Text
               | SchemaError Text -- Code smell (should be handled earlier, not here)
               deriving (Eq,Show)

type Result a = Either DbfsError a

data UserModOption  = AddToGroup GroupId
                    | DelFromGroup GroupId
                    | SetDisplayName Text
                    | SetUmask UMask
                    deriving (Eq,Show)

data ChownOption  = ChownOwner    UserAccountId
                  | ChownAddGroup GroupId
                  | ChownDelGroup GroupId
                  deriving (Eq,Show)

data ChmodOption = ChmodOwner Perm
                 | ChmodGroup GroupId Perm
                 | ChmodEveryone Perm
                 deriving (Eq,Show)

----------------------  PsuedoDirectoryEntity ------------------------
-- Abstracts file like objects not necessarily backed by databases (but their children are)
class (PersistFileEntity (PseudoEntry record), Show (PseudoKey record))
      => PseudoDirectoryEntity record where
  data PseudoKey   record
  type PseudoEntry record :: *

  getPseudoEntity      :: MonadIO m
                          => PseudoKey record -> SqlPersistT m (Maybe record)
  isPseudoExecutableBy :: MonadIO m => PseudoKey record -> UserAccountId -> SqlPersistT m Bool

  makePseudoEntry      :: UserAccountId -> PseudoKey record -> Text -> UTCTime -> UTCTime -> UTCTime -> UMask
                          -> PseudoEntry record

  getPseudoEntry    :: MonadIO m
                       => PseudoKey record -> Key (PseudoEntry record)
                       -> SqlPersistT m (Maybe (PseudoEntry record))

  getPseudoEntryBy    :: MonadIO m
                         => UserAccountId -> PseudoKey record -> Text
                         -> SqlPersistT m (Maybe (Entity (PseudoEntry record)))

  touchPseudoEntry ::  MonadIO m => PseudoKey record -> Key (PseudoEntry record) -> SqlPersistT m (Result ())

  entryAlreadyExistsError :: PseudoKey record -> Text -> DbfsError
  entryAlreadyExistsError key name = EntryAlreadyExists $ T.concat [ T.pack (show key), name]

  pseudoEntityDoesNotExistError :: PseudoKey record -> DbfsError
  pseudoEntityDoesNotExistError key = EntityDoesNotExist (T.pack $ show key)


data RootDirectory = RootDirectory deriving (Show)

instance PseudoDirectoryEntity RootDirectory where
  data PseudoKey   RootDirectory = PseudoRootKey RootDirectory deriving (Show)
  type PseudoEntry RootDirectory = Directory

  getPseudoEntity _  = return $ Just RootDirectory

  isPseudoExecutableBy _ _ = return True
  makePseudoEntry he _ name time umask = makeDirectory he name time umask
  getPseudoEntry _ key = get key
  getPseudoEntryBy he _ name = getBy (UniqueDirectory he name)

  touchPseudoEntry _ k = touchDirectory k

instance PseudoDirectoryEntity Directory where
  data PseudoKey   Directory = PseudoDirKey (Key Directory) deriving (Show)
  type PseudoEntry Directory = File

  getPseudoEntity (PseudoDirKey dirkey)  = get dirkey

  isPseudoExecutableBy (PseudoDirKey dirkey) uid = dirkey `isDirectoryExecutableBy` uid
  makePseudoEntry  he (PseudoDirKey dirkey) name time umask = makeFile he dirkey name time umask
  getPseudoEntry _ key = get key
  getPseudoEntryBy he (PseudoDirKey dirkey) name = getBy (UniqueFile he dirkey name)
  entryAlreadyExistsError (PseudoDirKey key) name = FileAlreadyExists $ T.concat [T.pack $ show key, name]
  touchPseudoEntry (PseudoDirKey _)  k = touchFile k


------------------------  PersistFileEntity --------------------------
-- Abstracts File like entities
class ( PersistEntity record, PersistEntity (OwnerGroup record)
      , PersistEntityBackend record ~ SqlBackend, PersistEntityBackend (OwnerGroup record) ~ SqlBackend)
      => PersistFileEntity record where
  type OwnerGroup   record :: *

  makeOwnerGroup   :: Key record -> GroupId -> UMask -> OwnerGroup record
  uniqueOwnerGroup :: Key record -> GroupId -> Unique (OwnerGroup record)

  doesNotExistError    :: Key record -> DbfsError
  doesNotExistError key = EntityDoesNotExist (T.pack $ show key)

  duplicateGroupsError :: Key record -> DbfsError
  duplicateGroupsError key = DuplicateOwnerGroups (T.pack $ show key)

  getFileEntity    :: (MonadIO m) => Key record  -> SqlPersistT m (Maybe record)
  touchFileEntity  :: (MonadIO m) => Key record  -> SqlPersistT m (Result  ())

  ownerField :: Key record -> EntityField record (Key UserAccount)
  ownerId    :: record -> (Key UserAccount)
  getOwnerId :: MonadIO m => Key record -> SqlPersistT m (Result (Key UserAccount))
  getOwnerId k =  do mu <- getFileEntity k
                     case mu of
                       Just u  -> Right <$> return (ownerId u)
                       Nothing -> Left  <$> return (doesNotExistError k)


instance PersistFileEntity Directory where
  type OwnerGroup       Directory = DirectoryGroup

  makeOwnerGroup = makeDirectoryGroup
  uniqueOwnerGroup = UniqueDirectoryGroup

  doesNotExistError  key = DirectoryDoesNotExist  (T.pack $ show key)
--  alreadyExistsError key = DirectoryAlreadyExists (T.pack $ show key)
  getFileEntity = get
  touchFileEntity  = touchDirectory

  ownerField _ = DirectoryUserId
  ownerId = directoryUserId

instance PersistFileEntity File where
  type OwnerGroup       File = FileGroup

  makeOwnerGroup = makeFileGroup
  uniqueOwnerGroup = UniqueFileGroup

  doesNotExistError  key = FileDoesNotExist  (T.pack $ show key)
--  alreadyExistsError key = FileAlreadyExists (T.pack $ show key)
  getFileEntity = get
  touchFileEntity = touchFile

  ownerField _ = FileUserId
  ownerId = fileUserId


-------------------------- Helper functions --------------------------

insertDbfs :: (MonadIO m, PersistEntity val , PersistUnique (PersistEntityBackend val) )
              => DbfsError -> val -> ReaderT (PersistEntityBackend val) m (Result (Key val))
insertDbfs err v = do mkey <- insertUnique v
                      case mkey of
                        Just key -> return $ Right key
                        Nothing  -> return $ Left  err

deleteByDbfs :: (MonadIO m, PersistEntity val, PersistUnique (PersistEntityBackend val) )
                => DbfsError -> Unique val -> ReaderT (PersistEntityBackend val) m (Result ())
deleteByDbfs err v = do mval <- getBy v
                        case mval of
                          Just _   -> do deleteBy v
                                         return $ Right ()
                          Nothing  -> return $ Left  err

_deleteDbfs :: (MonadIO m, PersistEntity val, PersistUnique (PersistEntityBackend val) )
              => DbfsError -> Key val -> ReaderT (PersistEntityBackend val) m (Result ())
_deleteDbfs err v = do mval <- get v
                       case mval of
                        Just _   -> do P.delete v
                                       return $ Right ()
                        Nothing  -> return $ Left  err


updateDbfs :: (MonadIO m , PersistEntity val , PersistStore (PersistEntityBackend val) )
              => DbfsError -> Key val -> [Update val] -> ReaderT (PersistEntityBackend val) m (Result ())
updateDbfs err key updates = do
  mval <- get key
  case mval of
    Just _   -> Right <$> P.update key updates
    Nothing  -> Left  <$> return err

-- | Returns schema error if there is more than 1 uniqueness constraint (does not throw)
upsertDbfs :: (MonadIO m ,MonadBaseControl IO m , PersistEntity val, PersistUnique (PersistEntityBackend val))
              => val -> [Update val]
              -> ReaderT (PersistEntityBackend val) m (Result (Entity val))
upsertDbfs v updates = do
  (Right <$> upsert v updates)
    `catch` (\e -> return $ Left $ SchemaError (T.pack $ show (e :: IOException)))


-- | Replace if there is a conflicting key with this value; returns 'Left' otherwise.
_replaceDbfs :: (MonadIO m, PersistEntity val, PersistUnique (PersistEntityBackend val) )
              => DbfsError ->  val -> ReaderT (PersistEntityBackend val) m (Result (Entity val))
_replaceDbfs err v = do mukey <- checkUnique v
                        case mukey of
                         Just _   -> Right <$> upsert v []
                         Nothing  -> Left  <$> return err



getUserDisplayName :: MonadIO m => UserAccountId -> SqlPersistT m (Result (Maybe Text))
getUserDisplayName uid =
  do mu <- get uid
     case mu of
       Just u  -> Right <$> return (userAccountDisplayName u)
       Nothing -> Left  <$> return (UserDoesNotExist $ T.pack $ show uid)


getUserUmask :: MonadIO m => UserAccountId -> SqlPersistT m (Result UMask)
getUserUmask uid =
  do mu <- get uid
     case mu of
       Just u  -> Right <$> return (umaskFromUserAccount u)
       Nothing -> Left  <$> return (UserDoesNotExist $ T.pack $ show uid)


getUser :: MonadIO m => Text -> SqlPersistT m (Result UserAccountId)
getUser ident = do
  muser <- getBy (UniqueUserAccount ident)
  case muser of
    Just (Entity uid _) -> return $ Right uid
    Nothing   -> return $ Left  (UserDoesNotExist ident)


isPrivileged :: MonadIO m => UserAccountId -> SqlPersistT m Bool
isPrivileged he = maybe False userAccountPrivileged <$> get he

userExistsBy :: MonadIO m => Text -> SqlPersistT m Bool
userExistsBy ident =  do existing <- P.getBy $ UniqueUserAccount ident
                         return $ maybe False (const True) existing

userExists :: MonadIO m => UserAccountId -> SqlPersistT m Bool
userExists uid =  do existing <- P.get $ uid
                     return $ maybe False (const True) existing


getDirectoryUserId :: MonadIO m => DirectoryId -> SqlPersistT m (Result UserAccountId)
getDirectoryUserId dir =
  do mu <- get dir
     case mu of
       Just u  -> Right <$> return (directoryUserId u)
       Nothing -> Left  <$> return (DirectoryDoesNotExist $ T.pack $ show dir)

getDirectoryGroups :: MonadIO m => DirectoryId -> SqlPersistT m (Result [GroupId])
getDirectoryGroups dir =
  do mu <- get dir
     case mu of
       Just _ -> do gs <- select $
                            from $ \directoryGroups -> do
                              where_ (directoryGroups^.DirectoryGroupDirectoryId ==. val dir)
                              return $ directoryGroups
                    return $ Right (map (directoryGroupGroupId . entityVal) gs)
       Nothing -> Left  <$> return (DirectoryDoesNotExist $ T.pack $ show dir)


---------------------- Modification/Access time ----------------------
touchUserAccount :: MonadIO m => UserAccountId -> SqlPersistT m (Result ())
touchUserAccount him = do
  time <- liftIO $ getCurrentTime
  updateDbfs (UserDoesNotExist $ T.pack $ show him)
    him
    [ (I.=.) UserAccountModified time
    , (I.=.) UserAccountAccessed time
    ]

_touchGroup :: MonadIO m => GroupId -> SqlPersistT m (Result ())
_touchGroup gid = do
  time <- liftIO $ getCurrentTime
  updateDbfs (GroupDoesNotExist $ T.pack $ show gid)
    gid
    [ (I.=.) GroupModified time
    , (I.=.) GroupAccessed time
    ]

touchFile :: MonadIO m => FileId -> SqlPersistT m (Result ())
touchFile file = do
  time <- liftIO $ getCurrentTime
  updateDbfs (FileDoesNotExist $ T.pack $ show file)
    file
    [ (I.=.) FileModified time
    , (I.=.) FileAccessed time
    ]

touchDirectory :: MonadIO m => DirectoryId -> SqlPersistT m (Result ())
touchDirectory dir = do
  time <- liftIO $ getCurrentTime
  updateDbfs (DirectoryDoesNotExist $ T.pack $ show dir)
    dir
    [ (I.=.) DirectoryModified time
    , (I.=.) DirectoryAccessed time
    ]

_accessUserAccount :: MonadIO m => UserAccountId -> SqlPersistT m (Result ())
_accessUserAccount him = do
  time <- liftIO $ getCurrentTime
  updateDbfs (UserDoesNotExist $ T.pack $ show him)
    him
    [ (I.=.) UserAccountAccessed  time
    ]

_accessGroup :: MonadIO m => GroupId -> SqlPersistT m (Result ())
_accessGroup gid = do
  time <- liftIO $ getCurrentTime
  updateDbfs (GroupDoesNotExist $ T.pack $ show gid)
    gid
    [ (I.=.) GroupAccessed  time
    ]

accessFile :: MonadIO m => FileId -> SqlPersistT m (Result ())
accessFile file = do
  time <- liftIO $ getCurrentTime
  updateDbfs (FileDoesNotExist $ T.pack $ show file)
    file
    [ (I.=.) FileAccessed   time
    ]

accessDirectory :: MonadIO m => DirectoryId -> SqlPersistT m (Result ())
accessDirectory dir = do
  time <- liftIO $ getCurrentTime
  updateDbfs (DirectoryDoesNotExist $ T.pack $ show dir)
    dir
    [ (I.=.) DirectoryAccessed   time
    ]



------------------------------ User ------------------------------

su :: MonadIO m => SqlPersistT m  UserAccountId
su = do roots <- select $
                 from $ \userAccount -> do
                   where_ (userAccount^.UserAccountPrivileged ==. val True)
                   limit 1
                   return userAccount

        case roots of
          (Entity root _:_) -> return  root
          []       -> makeRoot
  where
    makeRoot :: MonadIO m => SqlPersistT m  UserAccountId
    makeRoot = do time <- liftIO getCurrentTime
                  root <- insert $ (makeUserAccount  "root" time time time) { userAccountPrivileged = True }
                  return   root


suGuest :: MonadIO m => SqlPersistT m UserAccountId
suGuest = do eguest <- getUser "guest"
             case eguest of
               Right guest -> do unprivilege guest
                                 return  guest
               Left  _     -> makeGuest
  where
    makeGuest :: MonadIO m => SqlPersistT m  UserAccountId
    makeGuest = do time <- liftIO getCurrentTime
                   guest <- insert $ (makeUserAccount  "guest" time time time)
                   return $ guest

unprivilege :: MonadIO m => UserAccountId -> SqlPersistT m ()
unprivilege him = do
  _ <- updateDbfs (UserDoesNotExist $ T.pack $ show him)  him  [ (I.=.) UserAccountId him ]
  delete $
    from $ \groupMember -> do
      where_ (groupMember^.GroupMemberMember ==. val him)


useradd :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result UserAccountId)
useradd he ident = do
  prv <- isPrivileged he
  time <- liftIO $ getCurrentTime
  if prv
    then  insertDbfs (UserAlreadyExists ident) $ makeUserAccount ident time time time
    else  return $ Left  $ PermissionError "needs to be root to create a user"

userdel :: MonadIO m => UserAccountId -> UserAccountId -> SqlPersistT m (Result ())
userdel he him = do
  prv <- isPrivileged he
  uidExists <- userExists him

  if (prv || he == him) && uidExists
    then do deleteCascade him
            return $ Right ()
    else if not uidExists
         then return $ Left $ UserDoesNotExist (T.pack $ show him)
         else return $ Left $ PermissionError (T.pack (show he ++ " needs to be root to delete " ++ show him))

  -- if (prv || he == him) && uidExists
  --   then do ownedGroups <- select $
  --                          from $ \grp -> do
  --                            where_ (grp^.GroupOwner ==. val him)
  --                            return grp

  --           forM_ (map entityKey ownedGroups) (he `groupdel`)
  --           delete $
  --             from $ \directoryVote -> do
  --               where_ (directoryVote^.DirectoryVoteUserId ==. val him)

  --           delete $
  --             from $ \fileVote -> do
  --               where_ (fileVote^.FileVoteUserId ==. val him)

  --           delete $
  --             from $ \groupMembers -> do
  --               where_ (groupMembers^.GroupMemberMember ==. val him)

  --           delete $
  --             from $ \directory -> do
  --               where_ (directory^.DirectoryUserId ==. val him)

  --           delete $
  --             from $ \file -> do
  --               where_ (file^.FileUserId ==. val him)

  --           delete $
  --             from $ \userAccount -> do
  --               where_ (userAccount^.UserAccountId ==. val him)

  --           return $ Right he




usermod :: MonadIO m
           => UserAccountId -> UserAccountId -> [UserModOption] -> SqlPersistT m (Result UserAccountId)
usermod he him opts =  foldlM (>>>) (Right him) opts
  where
    (Left  a) >>> _  = return $ Left a

    (Right _) >>> (AddToGroup gid) = do
      prv <- isPrivileged he
      own <- he `isGroupOwnerOf`  gid
      if (prv || own)
        then do _ <- touchUserAccount him
                fmap (const him) <$>
                  insertDbfs (AlreadyGroupMember $ T.pack $ show him ) (makeGroupMember gid him)

        else return $ Left $ PermissionError $
                  T.concat [ T.pack $  show he , "is neither root nor the group owner" ]

    Right _ >>> DelFromGroup gid = do
      prv <- isPrivileged he
      own <- he `isGroupOwnerOf`  gid
      if (prv || own)
        then do _ <- touchUserAccount him
                fmap (const him) <$>
                  (deleteByDbfs (NotAGroupMember (T.pack $ show (gid,him))) $ UniqueGroupMember gid him)

        else return $ Left $ PermissionError $
             T.concat [ T.pack $  show he , " is neither root nor the group owner" ]

    Right _ >>> SetDisplayName newName = do
      prv <- isPrivileged he
      let himself = he == him

      if (prv || himself)
        then do _ <- touchUserAccount him
                update $ \user -> do
                  set user [ UserAccountDisplayName =. val (Just newName) ]
                  where_   (user^.UserAccountId ==. val him)
                return $ Right him
        else do return $ Left $ PermissionError $
                  T.concat [ T.pack $ show he
                           , "is neither root nor the person he is trying to change the display name" ]

    Right _ >>> SetUmask newUmask = do
      prv <- isPrivileged he

      if prv || (he == him)
        then do _ <- touchUserAccount him
                fmap (fmap (const him)) $ updateDbfs (UserDoesNotExist $ T.pack $ show he)
                  him
                  [ (I.=.) UserAccountUmaskOwnerR  (umaskOwnerR newUmask)
                  , (I.=.) UserAccountUmaskOwnerW  (umaskOwnerW newUmask)
                  , (I.=.) UserAccountUmaskOwnerX (umaskOwnerX newUmask)
                  , (I.=.) UserAccountUmaskGroupR  (umaskGroupR newUmask)
                  , (I.=.) UserAccountUmaskGroupW  (umaskGroupW newUmask)
                  , (I.=.) UserAccountUmaskGroupX  (umaskGroupX newUmask)
                  , (I.=.) UserAccountUmaskEveryoneR (umaskEveryoneR newUmask)
                  , (I.=.) UserAccountUmaskEveryoneW (umaskEveryoneW newUmask)
                  , (I.=.) UserAccountUmaskEveryoneX (umaskEveryoneX newUmask)
                  ]

        else do return $ Left $ PermissionError $
                  T.concat [ T.pack $ show he , "is neither root nor the person he is trying to change " ]




isGroupOwnerOf :: MonadIO m => UserAccountId -> GroupId -> SqlPersistT m Bool
isGroupOwnerOf he gid =  do
  mgroup <- get gid
  case mgroup of
    Just grp  -> return (groupOwner grp == he)
    Nothing     -> return False


--------------------------------  Group --------------------------------
groupadd :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result GroupId)
groupadd he groupName = do
  -- Anyone can make a group (TODO:should this be changed?)
  time <- liftIO $ getCurrentTime
  insertDbfs (GroupAlreadyExists groupName) (makeGroup groupName he time time time)

   -- case mgid of
   --   Just gid ->
   --   Nothing  -> return $ Left  $ GroupAlreadyExists $ T.concat ["group " , show groupName , "already exists" ]

-- groupmod :: UserId -> GroupId -> Text -> Maybe Text -> SqlPersistT m (Result GroupId)
-- groupmod uid gid groupName explanation = do
--   prv   <- privileged uid
--   owner <- groupOwner gid
--   if (prv || owner == uid)
--     then do update $ \p -> do
--             set [ p^.GroupGroup =. groupName
--                 , p^.GroupExplanation =. explanation
--                 ]
--             return $ Right gid
--     else
--     return $ Left $ PermissionError "only root or the owner can modify the group"

groupdel :: MonadIO m => UserAccountId -> GroupId -> SqlPersistT m (Result UserAccountId)
groupdel he gid = do
  prv   <- isPrivileged he
  own   <- he `isGroupOwnerOf` gid
  if  prv || own
    then do deleteCascade gid
            return $ Right he
    -- then do delete $
    --           from $ \directoryGroup -> do
    --             where_ (directoryGroup^.DirectoryGroupGroupId ==. val gid)

    --         delete $
    --           from $ \fileGroup -> do
    --             where_ (fileGroup^.FileGroupGroupId ==. val gid)

    --         delete $
    --           from $ \groupMember -> do
    --             where_ (groupMember^.GroupMemberGroupId  ==. val gid)

    --         delete $
    --           from $ \grp -> do
    --             where_ (grp^.GroupId ==. val gid)

    --         return $ Right he

    else
    return $ Left $ PermissionError
    (T.pack $ show he ++ " needs to be root or the group owner to delete the group")




belongs :: MonadIO m => UserAccountId -> SqlPersistT m (Result [GroupId])
belongs he = do
  muser <- get he
  case muser of
    Just _ -> do  groups  <- select $
                                from $ \groupMember -> do
                                  where_ (groupMember^.GroupMemberMember ==. val he)
                                  return groupMember
                  return $ Right $ map (groupMemberGroupId . entityVal)  groups

    Nothing -> return $ Left $ UserDoesNotExist $ T.pack $ show he



-------------------------- Entity creation  --------------------------
-- In our system, everyone is allowed to create a directory (which is really a prolog program)
-- _mkdir' :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result DirectoryId)
-- _mkdir' he name = do
--   muser <- get he

--   case muser of
--     Just user -> do
--       let umask = umaskFromUserAccount user
--       edir <- insertDbfs (DirectoryAlreadyExists name) (makeDirectory he name umask)
--       case edir of
--         Left err  -> return $ Left err
--         Right dir -> do
--           groups <- belongs he -- should not throw

--           case groups of
--             Right gs -> do
--               forM_  gs $ \g -> do
--                 insertDbfs (DuplicateDirectoryGroups (T.pack $ show dir)) $ makeDirectoryGroup dir g umask
--               return $ Right dir

--             Left err -> return $ Left err

--     Nothing   -> return $ Left $ UserDoesNotExist $ T.pack $  "user does not exist:" ++ show he



-- _touch' :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result FileId)
-- _touch' he dir name = do
--   muser <- get he
--   mdir  <- get dir
-- -- Unlike a unix filesystem, creating files is allowed iff the directory is executable, not writable
--   x     <- dir `isDirectoryExecutableBy` he
--   case (muser, mdir, x) of
--     (Just user, Just _ , True) ->  do
--       let umask =  umaskFromUserAccount user
--       efile <- insertDbfs (FileAlreadyExists name) (makeFile he dir name umask)
--       case efile of
--         Left  err -> return $ Left err
--         Right file -> do
--           groups <- belongs he -- should not throw

--           case groups of
--             Right gs -> do
--               forM_  gs $  \g ->
--                 insertDbfs (DuplicateFileGroups (T.pack $ show file)) (makeFileGroup file g umask)
--               return $ Right file
--             Left err -> return $ Left err

--     (Nothing, _      , _  )  -> return $ Left $ UserDoesNotExist      $ T.pack (show he)
--     (_      , Nothing , _ )  -> return $ Left $ DirectoryDoesNotExist $ T.pack (show dir)
--     (_ , _ ,  _ ) -> return $ Left $ PermissionError $ T.pack "directory not executable(but MAY be writable)"

fopen :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result (Entity File))
fopen he dir name = open he (PseudoDirKey dir) name

touch :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result (Key File))
touch he dir name = runEitherT $ do Entity key _ <- EitherT $ open he (PseudoDirKey dir) name
                                    return key

touchAt :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result (Key File))
touchAt = touch

mkdir :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result (Key Directory))
mkdir he name = runEitherT $ do Entity key _ <- EitherT $ open he (PseudoRootKey RootDirectory) name
                                return key

opendir :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result (Entity Directory))
opendir he name = open he (PseudoRootKey RootDirectory) name

open :: (MonadIO m
        , PseudoDirectoryEntity dir , PersistEntityBackend (OwnerGroup (PseudoEntry dir)) ~ SqlBackend)
        =>  UserAccountId ->  PseudoKey dir -> Text -> SqlPersistT m (Result (Entity (PseudoEntry dir)))
open he dirkey name = do
  muser <- get he
  mdir  <- getPseudoEntity dirkey
  x <- dirkey `isPseudoExecutableBy` he
  case (muser, mdir, x) of

    (Just user, Just _dir, True) ->  do
      let umask =  umaskFromUserAccount user
      created  <- liftIO $ getCurrentTime
      efile <- insertDbfs (entryAlreadyExistsError dirkey name)
                 (makePseudoEntry he dirkey name created created created umask)
      case efile of
        Left  (EntryAlreadyExists _)     -> getPseudoEntryBy he dirkey name >>= touch'
        Left  (FileAlreadyExists  _)     -> getPseudoEntryBy he dirkey name >>= touch'
        Left  (DirectoryAlreadyExists _) -> getPseudoEntryBy he dirkey name >>= touch'
        Left  err                        -> return $ Left err
        Right file                       -> do insertGroups file umask
                                               getEntry file


    (Nothing, _      , _  )  -> return $ Left $ UserDoesNotExist      $ T.pack (show he)
    (_      , Nothing , _ )  -> return $ Left $ pseudoEntityDoesNotExistError dirkey
    (_ , _ ,  _ )            -> return $ Left $ PermissionError $
                                           T.pack "parent is not executable(but may be writable)"

  where

    touch' (Just ent@(Entity key _value)) = touchPseudoEntry dirkey key >> return (Right ent)
    touch' Nothing                   = return $ Left (EntryDoesNotExist (T.pack "file does not exist"))

    insertGroups file  umask = do
      groups <- belongs he -- should not throw
      case groups of
        Right gs -> do
          forM_  gs $  \g ->
            insertDbfs (duplicateGroupsError file) (makeOwnerGroup file g umask)
        Left _  -> return ()

    getEntry file = do mvalue <- getPseudoEntry dirkey file
                       case mvalue of
                         Just value -> return $ Right (Entity file value)
                         Nothing     -> return $ Left  (EntityCouldNotBeOpened
                                                        $ T.pack $ show file)



-- lsRoot :: UserId -> Int -> Int -> Handler [ Entity Directory ]
-- lsRoot uid n m
--   | n < m     = findP uid [] [OffsetBy n, LimitTo (m-n) ]
--   | otherwise = findP uid [] []

-- findP :: UserId -> [ Filter Directory ] -> [ SelectOpt Directory ] -> Handler [ Entity Directory ]
-- findP uid filter select = undefined

-- _selectGroupPrograms :: UserId -> SqlPersistT Handler [ (Entity Directory
--                                                        , Entity DirectoryGroupSecurity
--                                                        , Entity GroupMember) ]
-- _selectGroupPrograms uid   =
--   select $
--   from $ \(prog `InnerJoin` flag `InnerJoin` grp) -> do
--      on (grp ^. GroupMemberGroupId ==. flag ^. DirectoryGroupSecurityGroupId)
--      on (flag ^. DirectoryGroupSecurityDirectoryId ==. prog ^. DirectoryId)
--      where_ ( grp ^. GroupMemberMember ==. val uid )
--      return ( prog, flag, grp)


---------------------- Access function template ----------------------
isDirectoryOwnerAccessibleBy:: MonadIO m
                       =>  DirectoryId -> UserAccountId -> EntityField Directory Bool -> SqlPersistT m Bool
isDirectoryOwnerAccessibleBy dir him field = do
  ds <- select $
        from $ \directory -> do
          where_( directory^.DirectoryId          ==. val dir
                  &&. directory^.DirectoryUserId  ==. val him
                  &&. directory^.field            ==. val True
                )
          return directory
  return $ not $ null ds

isDirectoryEveryoneAccessibleBy:: MonadIO m
                       =>  DirectoryId -> UserAccountId -> EntityField Directory Bool -> SqlPersistT m Bool
isDirectoryEveryoneAccessibleBy dir _him field = do
  ds <- select $
        from $ \directory -> do
          where_( directory^.DirectoryId          ==. val dir
                  &&. directory^.field            ==. val True
                )
          return directory
  return $ not $ null ds

isDirectoryGroupAccessibleBy :: MonadIO m
                                => DirectoryId -> UserAccountId -> EntityField DirectoryGroup Bool
                             -> SqlPersistT m Bool
isDirectoryGroupAccessibleBy dir him field = do
  ds <- select $
        from $ \(directory `InnerJoin` directoryGroup `InnerJoin` groupMember) -> do
          on (groupMember^.GroupMemberGroupId ==. directoryGroup^.DirectoryGroupGroupId)
          on (directoryGroup^.DirectoryGroupDirectoryId ==. directory^.DirectoryId)
          where_ ( directory^.DirectoryId ==. val dir
                   &&. directoryGroup^.field ==. val True
                   &&. groupMember^.GroupMemberMember ==. val him )
          return directory
  return $ not $ null ds


isFileOwnerAccessibleBy:: MonadIO m
                       =>  FileId -> UserAccountId -> EntityField File Bool -> SqlPersistT m Bool
isFileOwnerAccessibleBy f him field = do
  ds <- select $
        from $ \file -> do
          where_( file^.FileId          ==. val f
                  &&. file^.FileUserId  ==. val him
                  &&. file^.field       ==. val True
                )
          return file
  return $ not $ null ds

isFileEveryoneAccessibleBy:: MonadIO m
                       =>  FileId -> UserAccountId -> EntityField File Bool -> SqlPersistT m Bool
isFileEveryoneAccessibleBy f _him field = do
  ds <- select $
        from $ \file -> do
          where_( file^.FileId       ==. val f
                  &&. file^.field    ==. val True
                )
          return file
  return $ not $ null ds

isFileGroupAccessibleBy :: MonadIO m
                                => FileId -> UserAccountId -> EntityField FileGroup Bool
                             -> SqlPersistT m Bool
isFileGroupAccessibleBy f him field = do
  ds <- select $
        from $ \(file `InnerJoin` fileGroup `InnerJoin` groupMember) -> do
          on (groupMember^.GroupMemberGroupId ==. fileGroup^.FileGroupGroupId)
          on (fileGroup^.FileGroupFileId ==. file^.FileId)
          where_ ( file^.FileId ==. val f
                   &&. fileGroup^.field ==. val True
                   &&. groupMember^.GroupMemberMember ==. val him )
          return file
  return $ not $ null ds



-------------------------- Permission API   ------------------------
isDirectoryOwnerReadableBy :: MonadIO m
                              =>  DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryOwnerReadableBy dir him = dir `isDirectoryOwnerAccessibleBy` him $ DirectoryOwnerR

isDirectoryOwnerWritableBy :: MonadIO m
                              => DirectoryId -> UserAccountId ->  SqlPersistT m Bool
isDirectoryOwnerWritableBy dir him = dir `isDirectoryOwnerAccessibleBy` him $ DirectoryOwnerW

isDirectoryOwnerExecutableBy :: MonadIO m
                                => DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryOwnerExecutableBy dir him = dir `isDirectoryOwnerAccessibleBy` him $  DirectoryOwnerX


isDirectoryGroupReadableBy :: MonadIO m
                              =>  DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryGroupReadableBy dir him = dir `isDirectoryGroupAccessibleBy` him $ DirectoryGroupGroupR

isDirectoryGroupWritableBy :: MonadIO m
                              => DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryGroupWritableBy dir him = dir `isDirectoryGroupAccessibleBy` him $ DirectoryGroupGroupW

isDirectoryGroupExecutableBy :: MonadIO m
                                => DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryGroupExecutableBy dir him = dir `isDirectoryGroupAccessibleBy` him $  DirectoryGroupGroupX


isDirectoryEveryoneReadableBy :: MonadIO m
                              =>  DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryEveryoneReadableBy dir him = dir `isDirectoryEveryoneAccessibleBy` him $ DirectoryEveryoneR

isDirectoryEveryoneWritableBy :: MonadIO m
                              => DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryEveryoneWritableBy dir him = dir `isDirectoryEveryoneAccessibleBy` him $ DirectoryEveryoneW

isDirectoryEveryoneExecutableBy :: MonadIO m
                                => DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryEveryoneExecutableBy dir him = dir `isDirectoryEveryoneAccessibleBy` him $  DirectoryEveryoneX




isDirectoryReadableBy :: MonadIO m =>  DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryReadableBy dir him = orM [ dir `isDirectoryOwnerReadableBy`  him
                                    , dir `isDirectoryEveryoneReadableBy` him
                                    , dir `isDirectoryGroupReadableBy`  him
                                    ]

isDirectoryWritableBy :: MonadIO m =>  DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryWritableBy dir him = orM [ dir `isDirectoryOwnerWritableBy`  him
                                    , dir `isDirectoryEveryoneWritableBy` him
                                    , dir `isDirectoryGroupWritableBy`  him
                                    ]

isDirectoryExecutableBy :: MonadIO m => DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryExecutableBy dir him = orM [ dir `isDirectoryOwnerExecutableBy`  him
                                      , dir `isDirectoryEveryoneExecutableBy` him
                                      , dir `isDirectoryGroupExecutableBy`  him
                                      ]





isFileOwnerReadableBy :: MonadIO m
                              =>  FileId -> UserAccountId -> SqlPersistT m Bool
isFileOwnerReadableBy dir him = dir `isFileOwnerAccessibleBy` him $ FileOwnerR

isFileOwnerWritableBy :: MonadIO m
                              => FileId -> UserAccountId ->  SqlPersistT m Bool
isFileOwnerWritableBy dir him = dir `isFileOwnerAccessibleBy` him $ FileOwnerW

isFileOwnerExecutableBy :: MonadIO m
                                => FileId -> UserAccountId -> SqlPersistT m Bool
isFileOwnerExecutableBy dir him = dir `isFileOwnerAccessibleBy` him $  FileOwnerX


isFileGroupReadableBy :: MonadIO m
                              =>  FileId -> UserAccountId -> SqlPersistT m Bool
isFileGroupReadableBy dir him = dir `isFileGroupAccessibleBy` him $ FileGroupGroupR

isFileGroupWritableBy :: MonadIO m
                              => FileId -> UserAccountId -> SqlPersistT m Bool
isFileGroupWritableBy dir him = dir `isFileGroupAccessibleBy` him $ FileGroupGroupW

isFileGroupExecutableBy :: MonadIO m
                                => FileId -> UserAccountId -> SqlPersistT m Bool
isFileGroupExecutableBy dir him = dir `isFileGroupAccessibleBy` him $  FileGroupGroupX


isFileEveryoneReadableBy :: MonadIO m
                              =>  FileId -> UserAccountId -> SqlPersistT m Bool
isFileEveryoneReadableBy dir him = dir `isFileEveryoneAccessibleBy` him $ FileEveryoneR

isFileEveryoneWritableBy :: MonadIO m
                              => FileId -> UserAccountId -> SqlPersistT m Bool
isFileEveryoneWritableBy dir him = dir `isFileEveryoneAccessibleBy` him $ FileEveryoneW

isFileEveryoneExecutableBy :: MonadIO m
                                => FileId -> UserAccountId -> SqlPersistT m Bool
isFileEveryoneExecutableBy dir him = dir `isFileEveryoneAccessibleBy` him $  FileEveryoneX




isFileReadableBy :: MonadIO m =>  FileId -> UserAccountId -> SqlPersistT m Bool
isFileReadableBy dir him = orM [ dir `isFileOwnerReadableBy`  him
                                    , dir `isFileEveryoneReadableBy` him
                                    , dir `isFileGroupReadableBy`  him
                                    ]

isFileWritableBy :: MonadIO m =>  FileId -> UserAccountId -> SqlPersistT m Bool
isFileWritableBy dir him = orM [ dir `isFileOwnerWritableBy`  him
                                    , dir `isFileEveryoneWritableBy` him
                                    , dir `isFileGroupWritableBy`  him
                                    ]

isFileExecutableBy :: MonadIO m => FileId -> UserAccountId -> SqlPersistT m Bool
isFileExecutableBy dir him = orM [ dir `isFileOwnerExecutableBy`  him
                                      , dir `isFileEveryoneExecutableBy` him
                                      , dir `isFileGroupExecutableBy`  him
                                      ]





--------------------------------  ls  --------------------------------
lsDirectory :: (MonadIO m )
      => UserAccountId -> Int64 -> Int64 -> SqlPersistT m (Result [DirectoryId])

lsDirectory uid offs lim = do
  prv <- isPrivileged uid
  results <- select $
             from $ \(directory
                      `LeftOuterJoin`
                      (directoryGroup `InnerJoin` groupMember )) ->
                    distinctOn [don (directory^.DirectoryId) ] $ do
                      on (groupMember^.GroupMemberGroupId ==. directoryGroup^.DirectoryGroupGroupId)
                      on (directoryGroup^.DirectoryGroupDirectoryId ==. directory^.DirectoryId)
                      where_ ( directoryOwnerReadable uid directory
                               ||. directoryGroupReadable uid directoryGroup groupMember
                               ||. directoryEveryoneReadable uid directory
                               ||. val prv ==. val True
                             )
                      offset offs
                      limit lim
                      return (directory^.DirectoryId)
  return (Right $ map unValue results)
  where
    directoryOwnerReadable uid' directory = directory^.DirectoryUserId ==. val uid'
                                           &&. directory^.DirectoryOwnerR ==. val True

    directoryGroupReadable uid' directoryGroup groupMember =
      groupMember^.GroupMemberMember ==. val uid'
      &&. directoryGroup^.DirectoryGroupGroupR ==. val True

    directoryEveryoneReadable _uid' directory = directory^.DirectoryEveryoneR ==. val True


isDirectoryEmpty :: MonadIO m
                    => DirectoryId -> SqlPersistT m Bool
isDirectoryEmpty dir = do
  files <- select $
           from $ \file -> do
             where_ (file^.FileDirectoryId ==. val dir)
  return $ null files


lsFile :: (MonadIO m )
      => UserAccountId -> DirectoryId -> Int64 -> Int64 -> SqlPersistT m (Result [FileId])

lsFile uid dir offs lim = do
  prv <- isPrivileged uid
  results <- select $
             from $ \(file
                      `LeftOuterJoin`
                      (fileGroup `InnerJoin` groupMember )) ->
                    distinctOn [don (file^.FileId) ] $ do
                      on (groupMember^.GroupMemberGroupId ==. fileGroup^.FileGroupGroupId)
                      on (fileGroup^.FileGroupFileId ==. file^.FileId)
                      where_ ( (fileOwnerReadable uid file
                                ||. fileGroupReadable uid fileGroup groupMember
                                ||. fileEveryoneReadable uid file
                                ||. val prv ==. val True
                               ) &&. file^.FileDirectoryId ==. val dir
                             )
                      offset offs
                      limit lim
                      return (file^.FileId)
  _ <- accessDirectory dir
  return (Right $ map unValue results)
  where
    fileOwnerReadable uid' file = file^.FileUserId ==. val uid'
                                 &&. file^.FileOwnerR ==. val True

    fileGroupReadable uid' fileGroup groupMember =
      groupMember^.GroupMemberMember ==. val uid'
      &&. fileGroup^.FileGroupGroupR ==. val True

    fileEveryoneReadable _uid' file = file^.FileEveryoneR ==. val True


-- llFile :: MonadIO m
--           => [ FileId ] -> SqlPersistT m (Result [ FileInfo ])
-- llFile files =  foldrM (>>>) (Right []) files
--   where
--     _    >>> (Left err) = return (Left err)
--     file >>> (Right fs) = do mf <- getFileInfo file
--                              case mf of
--                                Just f  -> return $ Right (f:fs)
--                                Nothing -> return $ Left (FileDoesNotExist (T.pack $ show file))


-- getPublicFile :: MonadIO m
--                  => FileId -> SqlPersistT m (Maybe PublicFile)
-- getPublicFile file = do mf <- get file
--                         case mf of
--                           Just f  -> return $ Just $ makePublicFile f
--                           Nothing -> return Nothing

-------------------------------- Find --------------------------------

findDirectory :: (MonadIO m )
      => UserAccountId -> Int64 -> Int64 -> SqlPersistT m (Result [DirectoryInfo])

findDirectory uid offs lim = do
  prv <- isPrivileged uid
  results <- select $
             from $ \(directory
                      `LeftOuterJoin`
                      (directoryGroup `InnerJoin` groupMember )) ->
                    distinctOn [don (directory^.DirectoryId) ] $ do
                      on (groupMember?.GroupMemberGroupId ==. directoryGroup?.DirectoryGroupGroupId)
                      on (directoryGroup?.DirectoryGroupDirectoryId ==. just (directory^.DirectoryId))
                      where_ ( directoryReadable        directory directoryGroup groupMember
                               ||. directoryWritable    directory directoryGroup groupMember
                               ||. directoryExecutable  directory directoryGroup groupMember
                               ||. val prv ==. val True
                             )
                      offset offs
                      limit lim

                      return ((directory^.DirectoryId)
                             ,(directory^.DirectoryUserId)
                             ,(directory^.DirectoryName)
                             ,(directory^.DirectoryCreated)
                             ,(directory^.DirectoryModified)
                             ,(directory^.DirectoryAccessed)
                             ,(directoryReadable    directory directoryGroup groupMember)
                             ,(directoryWritable    directory directoryGroup groupMember)
                             ,(directoryExecutable  directory directoryGroup groupMember)
                             )

  return (Right $ map (\(Value directory
                        ,Value uid
                        ,Value name
                        ,Value created
                        ,Value modified
                        ,Value accessed
                        ,Value r
                        ,Value w
                        ,Value x
                        ) -> makeDirectoryInfo directory uid name created modified accessed r w x)
          results)
  --return (Right [])
  where

    directoryReadable directory directoryGroup groupMember = directoryOwnerReadable directory
                                              ||. directoryGroupReadable directoryGroup groupMember
                                              ||. directoryEveryoneReadable  directory

    directoryWritable directory directoryGroup groupMember =  directoryOwnerWritable  directory
                                               ||. directoryGroupWritable  directoryGroup groupMember
                                               ||. directoryEveryoneWritable  directory

    directoryExecutable directory directoryGroup groupMember = directoryOwnerExecutable  directory
                                                ||. directoryGroupExecutable directoryGroup groupMember
                                                ||. directoryEveryoneExecutable  directory


    directoryOwnerReadable   directory = directory^.DirectoryUserId ==. val uid &&. directory^.DirectoryOwnerR ==. val True
    directoryOwnerWritable   directory = directory^.DirectoryUserId ==. val uid &&. directory^.DirectoryOwnerW ==. val True
    directoryOwnerExecutable directory = directory^.DirectoryUserId ==. val uid &&. directory^.DirectoryOwnerX ==. val True

    directoryGroupReadable   directoryGroup groupMember =  not_ (isNothing (groupMember?.GroupMemberMember))
                                                 &&. not_ (isNothing (directoryGroup?.DirectoryGroupGroupR))
                                                 &&. groupMember?.GroupMemberMember ==. just (val uid)
                                                 &&. directoryGroup?.DirectoryGroupGroupR ==. just (val True)
    directoryGroupWritable   directoryGroup groupMember =  not_ (isNothing (groupMember?.GroupMemberMember))
                                                 &&. not_ (isNothing (directoryGroup?.DirectoryGroupGroupW))
                                                 &&. groupMember?.GroupMemberMember ==. just (val uid)
                                                 &&. directoryGroup?.DirectoryGroupGroupW ==. just (val True)
    directoryGroupExecutable directoryGroup groupMember =  not_ (isNothing (groupMember?.GroupMemberMember))
                                                 &&. not_ (isNothing (directoryGroup?.DirectoryGroupGroupX))
                                                 &&. groupMember?.GroupMemberMember ==. just (val uid)
                                                 &&. directoryGroup?.DirectoryGroupGroupX ==. just (val True)

    directoryEveryoneReadable   directory = directory^.DirectoryEveryoneR ==. val True
    directoryEveryoneWritable   directory = directory^.DirectoryEveryoneW ==. val True
    directoryEveryoneExecutable directory = directory^.DirectoryEveryoneX ==. val True


findFile :: (MonadIO m )
      => UserAccountId -> Int64 -> Int64 -> SqlPersistT m (Result [FileInfo])

findFile uid offs lim = do
  prv <- isPrivileged uid
  results <- select $
             from $ \(file
                      `LeftOuterJoin`
                      (fileGroup `InnerJoin` groupMember )) ->
                    distinctOn [don (file^.FileId) ] $ do
                      on (groupMember?.GroupMemberGroupId ==. fileGroup?.FileGroupGroupId)
                      on (fileGroup?.FileGroupFileId ==. just (file^.FileId))
                      where_ ( fileReadable        file fileGroup groupMember
                               ||. fileWritable    file fileGroup groupMember
                               ||. fileExecutable  file fileGroup groupMember
                               ||. val prv ==. val True
                             )
                      offset offs
                      limit lim

                      return ((file^.FileId)
                             ,(file^.FileUserId)
                             ,(file^.FileDirectoryId)
                             ,(file^.FileName)
                             ,(file^.FileCreated)
                             ,(file^.FileModified)
                             ,(file^.FileAccessed)
                             ,(fileReadable    file fileGroup groupMember)
                             ,(fileWritable    file fileGroup groupMember)
                             ,(fileExecutable  file fileGroup groupMember)
                             )

  return (Right $ map (\(Value file
                        ,Value uid
                        ,Value dir
                        ,Value name
                        ,Value created
                        ,Value modified
                        ,Value accessed
                        ,Value r
                        ,Value w
                        ,Value x
                        ) -> makeFileInfo file uid dir name created modified accessed r w x)
          results)
  --return (Right [])
  where

    fileReadable file fileGroup groupMember = fileOwnerReadable file
                                              ||. fileGroupReadable fileGroup groupMember
                                              ||. fileEveryoneReadable  file

    fileWritable file fileGroup groupMember =  fileOwnerWritable  file
                                               ||. fileGroupWritable  fileGroup groupMember
                                               ||. fileEveryoneWritable  file

    fileExecutable file fileGroup groupMember = fileOwnerExecutable  file
                                                ||. fileGroupExecutable fileGroup groupMember
                                                ||. fileEveryoneExecutable  file


    fileOwnerReadable   file = file^.FileUserId ==. val uid &&. file^.FileOwnerR ==. val True
    fileOwnerWritable   file = file^.FileUserId ==. val uid &&. file^.FileOwnerW ==. val True
    fileOwnerExecutable file = file^.FileUserId ==. val uid &&. file^.FileOwnerX ==. val True

    fileGroupReadable   fileGroup groupMember =  not_ (isNothing (groupMember?.GroupMemberMember))
                                                 &&. not_ (isNothing (fileGroup?.FileGroupGroupR))
                                                 &&. groupMember?.GroupMemberMember ==. just (val uid)
                                                 &&. fileGroup?.FileGroupGroupR ==. just (val True)
    fileGroupWritable   fileGroup groupMember =  not_ (isNothing (groupMember?.GroupMemberMember))
                                                 &&. not_ (isNothing (fileGroup?.FileGroupGroupW))
                                                 &&. groupMember?.GroupMemberMember ==. just (val uid)
                                                 &&. fileGroup?.FileGroupGroupW ==. just (val True)
    fileGroupExecutable fileGroup groupMember =  not_ (isNothing (groupMember?.GroupMemberMember))
                                                 &&. not_ (isNothing (fileGroup?.FileGroupGroupX))
                                                 &&. groupMember?.GroupMemberMember ==. just (val uid)
                                                 &&. fileGroup?.FileGroupGroupX ==. just (val True)

    fileEveryoneReadable   file = file^.FileEveryoneR ==. val True
    fileEveryoneWritable   file = file^.FileEveryoneW ==. val True
    fileEveryoneExecutable file = file^.FileEveryoneX ==. val True

findFile' :: (MonadIO m )
      => UserAccountId -> Int64 -> Int64 -> SqlPersistT m (Result [FileInfo])

findFile' uid offs lim = do
  prv <- isPrivileged uid
  results <- select $
             from $ \(file
                      `LeftOuterJoin`
                      (fileGroup `InnerJoin` groupMember )) -> do
                      on (groupMember?.GroupMemberGroupId ==. fileGroup?.FileGroupGroupId)
                      on (fileGroup?.FileGroupFileId ==. just (file^.FileId))
                      offset offs
                      limit lim
                      return (file, fileGroup, groupMember)

  return $ Right $ distinctOnFileId $ filter (accessible prv) $ flip map results $
    \(Entity fid file , fileGroup , groupMember) ->
    let uid  = fileUserId file
        dir  = fileDirectoryId file
        name = fileName file
        created = fileCreated file
        modified = fileModified file
        accessed = fileAccessed file
        r = fileReadable   file fileGroup groupMember
        w = fileWritable   file fileGroup groupMember
        x = fileExecutable file fileGroup groupMember
    in makeFileInfo fid uid dir name created modified accessed r w x

  where
    fileReadable file fileGroup groupMember = fileOwnerReadable file
                                              || fileGroupReadable fileGroup groupMember
                                              || fileEveryoneReadable file
    fileWritable file fileGroup groupMember = fileOwnerWritable file
                                              || fileGroupWritable fileGroup groupMember
                                              || fileEveryoneWritable file
    fileExecutable file fileGroup groupMember = fileOwnerExecutable file
                                              || fileGroupExecutable fileGroup groupMember
                                              || fileEveryoneExecutable file

    fileOwnerReadable file   = fileUserId file == uid && fileOwnerR file
    fileOwnerWritable file   = fileUserId file == uid && fileOwnerW file
    fileOwnerExecutable file = fileUserId file == uid && fileOwnerX file

    fileGroupReadable (Just (Entity _ fileGroup)) (Just (Entity _ groupMember)) =
      groupMemberMember groupMember == uid  && fileGroupGroupR fileGroup
    fileGroupReadable _ _ = False

    fileGroupWritable (Just (Entity _ fileGroup)) (Just (Entity _ groupMember)) =
      groupMemberMember groupMember == uid  && fileGroupGroupW fileGroup
    fileGroupWritable _ _ = False

    fileGroupExecutable (Just (Entity _ fileGroup)) (Just (Entity _ groupMember)) =
      groupMemberMember groupMember == uid  && fileGroupGroupX fileGroup
    fileGroupExecutable _ _ = False


    fileEveryoneReadable   file = fileEveryoneR file
    fileEveryoneWritable   file = fileEveryoneW file
    fileEveryoneExecutable file = fileEveryoneX file

    accessible :: Bool -> FileInfo -> Bool
    accessible prv info = prv || fileInfoR info || fileInfoW info || fileInfoX info

    distinctOnFileId :: [FileInfo] -> [FileInfo]
    distinctOnFileId = nubBy f
      where
        f  :: FileInfo -> FileInfo -> Bool
        f = (==) `F.on` fileInfoFileId





findExecutableFile :: MonadIO m
                      => UserAccountId -> Int64 -> Int64 -> SqlPersistT m (Result [FileId])
findExecutableFile uid offs lim = do
  prv <- isPrivileged uid
  results <- select $
             from $ \(file
                      `LeftOuterJoin`
                      (fileGroup `InnerJoin` groupMember)) ->
                    distinctOn [don (file^.FileId) ] $ do
                      on (groupMember^.GroupMemberGroupId ==. fileGroup^.FileGroupGroupId)
                      on (fileGroup^.FileGroupFileId ==. file^. FileId)
                      where_ ( fileOwnerExecutable uid file
                               ||. fileGroupExecutable uid fileGroup groupMember
                               ||. fileEveryoneExecutable uid file
                               ||. val prv ==. val True
                             )
                      offset offs
                      limit  lim
                      return (file^.FileId)
  return (Right $ map unValue results)
  where
    fileOwnerExecutable uid' file = file^.FileUserId ==. val uid'
                                    &&. file^.FileOwnerX ==. val True
    fileGroupExecutable uid' fileGroup groupMember =
      groupMember^.GroupMemberMember ==. val uid'
      &&. fileGroup^.FileGroupGroupX ==. val True

    fileEveryoneExecutable _uid file = file^.FileEveryoneX ==. val True




-------------------------------- Read --------------------------------
readDirectory :: MonadIO m => UserAccountId -> DirectoryId -> SqlPersistT m (Result  Directory)
readDirectory he dir = do
  readable <- dir `isDirectoryReadableBy` he
  if readable
    then do ps <- get dir
            _  <- accessDirectory dir
            case ps of
              Just d  -> return $ Right d
              Nothing -> return $ Left $ DirectoryDoesNotExist $ T.concat [T.pack $ show dir ]
    else return $ Left $ PermissionError $ T.concat [T.pack $ show (he,dir) ]

readFile :: MonadIO m => UserAccountId -> FileId -> SqlPersistT m (Result  File)
readFile he file = do
  readable <- file `isFileReadableBy` he
  if readable
    then do ps <- get file
            _ <- accessFile file
            case ps of
              Just f  -> return $ Right f
              Nothing -> return $ Left $ FileDoesNotExist $ T.concat [T.pack $ show file ]
    else return $ Left $ PermissionError $ T.concat [T.pack $ show (he,file) ]


------------------------------  Write --------------------------------
writeDirectory :: MonadIO m => UserAccountId -> Entity Directory -> SqlPersistT m (Result (Entity Directory))
writeDirectory he directory@(Entity key v) = do
  writable <- key `isDirectoryWritableBy` he
  if writable
    then do repsert key v
            _<- touchDirectory key
            return (Right directory)

    else return $ Left $ DirectoryDoesNotExist $ T.concat [ T.pack $ show (he,key) ]

writeFile :: MonadIO m => UserAccountId -> Entity File -> SqlPersistT m (Result (Entity File))
writeFile he file@(Entity key v) = do
  writable <- key `isFileWritableBy` he
  if writable
    then do repsert key v
            _<- touchFile key
            return (Right file)
    else return $ Left $ FileDoesNotExist $ T.concat [ T.pack $ show (he,key) ]


-- ---------------------------- rmdir/unlink ----------------------------

rmdir :: MonadIO m =>UserAccountId -> DirectoryId -> SqlPersistT m (Result ())
rmdir he dir = do
  writable <- dir `isDirectoryWritableBy` he
  is_empty    <- isDirectoryEmpty dir
  case (writable , is_empty) of
    (True,True) ->
      do delete $
           from $ \directoryTag -> do
             where_ (directoryTag^.DirectoryTagDirectoryId ==. val dir)

         delete $
           from $ \directoryGroup -> do
             where_ (directoryGroup^.DirectoryGroupDirectoryId ==. val dir)

         delete $
           from $ \directory -> do
             where_ (directory^.DirectoryId ==. val dir)
         return $ Right ()

    (False , _) ->  return $ Left $ PermissionError $ T.concat [T.pack $ show (he,dir) ]
    (_  , False) -> return $ Left $ DirectoryNonEmpty $ T.concat [T.pack $ show (he,dir) ]

rm_rf :: MonadIO m =>UserAccountId -> DirectoryId -> SqlPersistT m (Result ())
rm_rf he dir = do
  writable <- dir `isDirectoryWritableBy` he
  is_empty    <- isDirectoryEmpty dir
  case (writable , is_empty) of
    (True,True) ->  deleteCascade dir >> return ( Right () )
    (False , _) ->  return $ Left $ PermissionError $ T.concat [T.pack $ show (he,dir) ]
    (_  , False) -> return $ Left $ DirectoryNonEmpty $ T.concat [T.pack $ show (he,dir) ]

unlink :: MonadIO m => UserAccountId -> FileId -> SqlPersistT m (Result ())
unlink he file = do
  writable <- file `isFileWritableBy` he

  case (writable) of
    (True) -> deleteCascade file >> return (Right ())
      -- do delete $
      --      from $ \fileTag -> do
      --        where_ (fileTag^.FileTagFileId ==. val file)

      --    delete $
      --      from $ \fileGroup -> do
      --        where_ (fileGroup^.FileGroupFileId ==. val file)

      --    delete $
      --      from $ \file' -> do
      --        where_ (file'^.FileId ==. val file)
      --    return $ Right ()

    (False) ->  return $ Left $ PermissionError $ T.concat [T.pack $ show (he,file) ]

rm :: MonadIO m =>UserAccountId -> FileId -> SqlPersistT m (Result ())
rm = unlink

------------------------------  chown --------------------------------



-- _chownDirectory :: MonadIO m =>
--                   UserAccountId -> DirectoryId -> [ChownOption] -> SqlPersistT m (Result DirectoryId)
-- _chownDirectory he it opts = do
--   mdir <- get it
--   case mdir of
--     Just _  -> foldlM (>>>) (Right it) opts
--     Nothing -> return $ Left $ DirectoryDoesNotExist $ T.pack $ show it

--   where
--     (>>>) :: MonadIO m
--              => Result DirectoryId -> ChownOption -> SqlPersistT m (Result DirectoryId)
--     (Left err) >>> _ = return $ Left err
--     (Right _ ) >>> (ChownOwner uid) =
--       do
--         prv   <- isPrivileged he
--         if prv
--           then do update $ \directory -> do
--                     set directory [ DirectoryUserId =. val uid ]
--                     where_ (directory^.DirectoryId ==. val it)
--                   return $ Right it

--           else return $ Left $ PermissionError (T.concat [ "only root can change the owner"
--                                                          , T.pack $ show he ++ show it ] )

--     (Right _ ) >>> (ChownAddGroup gid) =
--       do
--         prv   <- isPrivileged he
--         eumask <- getUserUmask he
--         eowner <- getDirectoryUserId it
--         created <- liftIO $ getCurrentTime

--         case (eumask,eowner , prv || eowner == Right he) of
--           (Left err, _ , _ ) ->  return $ Left err
--           (_ , Left err , _ ) ->  return $ Left err
--           (_ , _   , False) ->  return $ Left $ PermissionError $
--                                        (T.concat ["only root or the owner can change the group ownership"])
--           (Right umask, Right _ , True ) ->
--             fmap (const it) <$>
--             insertDbfs (AlreadyOwner $ T.pack $ show gid) (makeDirectoryGroup it gid created umask)

--     (Right _ ) >>> (ChownDelGroup gid) =
--       do
--         prv   <- isPrivileged he
--         eumask <- getUserUmask he
--         eowner <- getDirectoryUserId it

--         case (eumask,eowner , prv || eowner == Right he) of
--           (Left err, _ , _ ) ->  return $ Left err
--           (_ , Left err , _ ) ->  return $ Left err
--           (_ , _   , False) ->  return $ Left $ PermissionError $
--                                        (T.concat ["only root or the owner can change the group ownership"])
--           (Right _, Right _ , True ) ->
--             fmap (const it) <$>
--             (deleteByDbfs (NotAnOwner $ T.pack $ show gid) $ UniqueDirectoryGroup it gid)




chown :: (MonadIO m , PersistFileEntity val)
         => UserAccountId -> Key val -> [ChownOption] -> SqlPersistT m (Result (Key val))
chown he key opts = do
  mit <- getFileEntity key

  case mit of
    Just _  -> foldlM (>>>) (Right key) opts
    Nothing -> return $ Left $ doesNotExistError key

   where
--     (>>>) :: (MonadIO m, PersistFileEntity val,  PersistEntityBackend val ~ SqlBackend)
--              => Result (Key val) -> ChownOption -> SqlPersistT m (Result (Key val))
     (Left err) >>> _ = return $ Left err

     (Right k) >>> (ChownOwner uid) =
       do
         prv   <- isPrivileged he
         if prv
           then do e <- updateDbfs (doesNotExistError k)
                        k
                        [ (I.=.) (ownerField k) uid ]
                   _ <- touchFileEntity k
                   return $ fmap (const k) e

           else return $ Left $ PermissionError (T.concat [ "only root can change the owner"
                                                          , T.pack $ show he ++ show k ] )

     (Right k) >>> (ChownAddGroup gid) =
       do
         prv   <- isPrivileged he
         eumask <- getUserUmask he
         eowner <- getOwnerId k
         _ <- touchFileEntity k
--         created <- liftIO $ getCurrentTime

         case (eumask,eowner , prv || eowner == Right he) of
           (Left err, _ , _ )  ->  return $ Left err
           (_ , Left err , _ ) ->  return $ Left err
           (_ , _   , False)   ->  return $ Left $ PermissionError $
                                       (T.concat ["only root or the owner can change the group ownership"])
           (Right umask, Right _ , True ) ->
             fmap (const k) <$>
             insertDbfs (AlreadyOwner $ T.pack $ show gid) (makeOwnerGroup k gid umask)

     (Right k) >>> (ChownDelGroup gid) =
       do
         prv   <- isPrivileged he
         eumask <- getUserUmask he
         eowner <- getOwnerId k
         _ <- touchFileEntity k

         case (eumask,eowner , prv || eowner == Right he) of
           (Left err, _ , _ )  ->  return $ Left err
           (_ , Left err , _ ) ->  return $ Left err
           (_ , _   , False)   ->  return $ Left $ PermissionError $
                                   (T.concat ["only root or the owner can change the group ownership"])
           (Right _, Right _ , True ) ->
             fmap (const k) <$>
             (deleteByDbfs (NotAnOwner $ T.pack $ show gid) $ uniqueOwnerGroup k gid)


------------------------------  chmod --------------------------------

isOwnerOfDirectory :: MonadIO m => UserAccountId -> DirectoryId -> SqlPersistT m Bool
isOwnerOfDirectory he dir = do
  mdir <- get dir
  case mdir of
    Just d -> return $ directoryUserId d == he
    _      -> return False



chmodDirectory :: (MonadIO m, MonadBaseControl IO m)
                  => UserAccountId -> DirectoryId -> [ChmodOption]
                  -> SqlPersistT m (Result DirectoryId)
chmodDirectory he dir opts  = do
  own      <- he `isOwnerOfDirectory` dir
  prv      <- isPrivileged he

  if (prv || own)
    then do e <- foldlM (>>>) (Right ()) opts
            return $ (fmap (const dir) e)
    else return $ Left $ PermissionError $ T.pack $ show he ++ " needs to be the owner or root"

    where
      Left err >>> _ = return $ Left err

      Right _  >>> ChmodOwner (Perm r w x) = do
        _ <- touchDirectory dir
        updateDbfs (DirectoryDoesNotExist $ T.pack $ show dir)
          dir
          [ (I.=.) DirectoryOwnerR  r
          , (I.=.) DirectoryOwnerW  w
          , (I.=.) DirectoryOwnerX  x
          ]

      Right _  >>> ChmodEveryone (Perm r w x) = do
        _ <- touchDirectory dir
        updateDbfs (DirectoryDoesNotExist $ T.pack $ show dir)
          dir
          [ (I.=.) DirectoryEveryoneR  r
          , (I.=.) DirectoryEveryoneW  w
          , (I.=.) DirectoryEveryoneX  x
          ]

      Right _ >>> ChmodGroup gid (Perm r w x) = do
        _ <- touchDirectory dir
        e <- upsertDbfs (makeDirectoryGroupWithPerm dir gid (Perm r w x))
          [ (I.=.) DirectoryGroupGroupR   r
          , (I.=.) DirectoryGroupGroupW   w
          , (I.=.) DirectoryGroupGroupX   x
          ]
        return $ fmap (const () ) e


isOwnerOfFile :: MonadIO m => UserAccountId -> FileId -> SqlPersistT m Bool
isOwnerOfFile he file = do
  mfile <- get file
  case mfile of
    Just f -> return $ fileUserId f == he
    _      -> return False


chmodFile :: (MonadIO m, MonadBaseControl IO m)
             => UserAccountId -> FileId -> [ChmodOption]
             -> SqlPersistT m (Result FileId)
chmodFile he file opts  = do
  own      <- he `isOwnerOfFile` file
  prv      <- isPrivileged he

  if (prv || own)
    then do e <- foldlM (>>>) (Right ()) opts
            return $ fmap (const file) e
    else return $ Left $ PermissionError $ T.pack $ show he ++ " needs to be the owner or root"

    where
      Left err >>> _ = return $ Left err

      Right _  >>> ChmodOwner (Perm r w x) = do
        _ <- touchFile file
        updateDbfs (FileDoesNotExist $ T.pack $ show file)
          file
          [ (I.=.) FileOwnerR  r
          , (I.=.) FileOwnerW  w
          , (I.=.) FileOwnerX  x
          ]

      Right _  >>> ChmodEveryone (Perm r w x) = do
        _ <- touchFile file
        updateDbfs (FileDoesNotExist $ T.pack $ show file)
          file
          [ (I.=.) FileEveryoneR  r
          , (I.=.) FileEveryoneW  w
          , (I.=.) FileEveryoneX  x
          ]

      Right _ >>> ChmodGroup gid (Perm r w x) = do
        _ <- touchFile file
        e <- upsertDbfs (makeFileGroupWithPerm file gid  (Perm r w x))
          [ (I.=.) FileGroupGroupR   r
          , (I.=.) FileGroupGroupW   w
          , (I.=.) FileGroupGroupX   x
          ]
        return $ fmap (const () ) e



-- ------------------------  Database Invariants  ------------------------

-- valid :: SqlPersistT m Bool
-- valid = allM [ mapM (\g -> validPid (prologGoalDirectoryId g)) =<< allGoals
--              ,  mapM (\g -> validGid (groupMembersGroupId g)) =<< allGroupMember
--              ,  mapM (\g -> validUid (groupMembersMember g)) =<< allGroupMember
--              ,  mapM (\g -> validPid (programGroupsDirectoryId g)) =<< allProgramGroups
--              ,  mapM (\g -> validGid (programGroupsGroupId g)) =<<  allProgramGroups
--              ,  mapM (\g -> validGoalId (goalGroupsFileId g)) =<< allGoalGroups
--              ,  mapM (\g -> validGid (goalGroupsGroupId g)) =<< allGoalGroups
--              ,  mapM (\g -> validPid (prologProgramTagsDirectoryId g)) =<< allDirectorysTags
--              ,  mapM (\g -> validTagId (prologProgramTagsTagId g)) =<< allDirectoryTags
--              ,  mapM (\g -> validGoalid (prologGoalTagsFileId g)) =<< allFileTags
--              ,  mapM (\g -> validTagId (prologGoalTagsTagId g)) =<< allFileTags
--              ]


------------------------ Auxillary functions  ------------------------

orM :: Monad m => [m Bool] -> m Bool
orM [] = return False
orM (m:ms) = do x <- m
                if x
                  then return True
                  else orM ms

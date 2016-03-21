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
--       , mkdir' , touch'
       , isPrivileged
       , isGroupOwnerOf
       , belongs
       , userExistsBy
       , getUserDisplayName
       , getDirectoryUserId
       , getDirectoryGroups
       , chown
--       , chownDirectory
--       , chownFile
       , chmodDirectory
       , chmodFile

       , su
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

       , readDirectory
       , readFile

       , writeDirectory
       , writeFile

       , rmdir
       , unlink,rm

         -- , programTagsAdd
         -- , programTagsDel
         -- , programTagsFind

         -- , goalTagsAdd
         -- , goalTagsDel
         -- , goalTagsFind

         -- TODO User accounting functions

  ) where


import             Import hiding ((==.), (>=.) ,(||.)  , on , Value , update , (=.) , forM_ , delete
                                 , readFile, writeFile
                                 )
import  qualified  Import as I
import             Database.Esqueleto hiding(insert)
import  qualified  Database.Persist as P
import  qualified  Data.Text as T
import             Data.Foldable hiding(null, mapM_)


data DbfsError = DirectoryDoesNotExist Text
               | FileDoesNotExist Text
               | UserDoesNotExist   Text
               | UserAlreadyExists Text
               | GroupAlreadyExists Text
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
               | EntityDoesNotExist Text
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


-- Abstracts file like objects not necessarily backed by databases (but their children are)
class (PersistFileEntity (PseudoEntry record), Show (PseudoKey record))
      => PseudoDirectoryEntity record where
  data PseudoKey   record
  type PseudoEntry record :: *

  getPseudoEntity      :: MonadIO m => PseudoKey record -> SqlPersistT m (Maybe record)
  isPseudoExecutableBy :: MonadIO m => PseudoKey record -> UserAccountId -> SqlPersistT m Bool
  makePseudoEntry      :: UserAccountId -> PseudoKey record -> Text -> UMask -> PseudoEntry record
  entryAlreadyExistsError :: PseudoKey record -> Text -> DbfsError
  entryAlreadyExistsError key name = EntryAlreadyExists $ T.concat [ T.pack (show key), name]
  pseudoEntityDoesNotExistError :: PseudoKey record -> DbfsError
  pseudoEntityDoesNotExistError key = EntityDoesNotExist (T.pack $ show key)

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

  getFile    :: (MonadIO m) => Key record -> SqlPersistT m (Maybe record)
  ownerField :: Key record -> EntityField record (Key UserAccount)
  ownerId    :: record -> (Key UserAccount)
  getOwnerId :: MonadIO m => Key record -> SqlPersistT m (Result (Key UserAccount))
  getOwnerId k =  do mu <- getFile k
                     case mu of
                       Just u  -> Right <$> return (ownerId u)
                       Nothing -> Left  <$> return (doesNotExistError k)


data RootDirectory = RootDirectory deriving (Show)

instance PseudoDirectoryEntity RootDirectory where
  data PseudoKey RootDirectory = PseudoRootKey RootDirectory deriving (Show)
  type PseudoEntry RootDirectory = Directory

  getPseudoEntity _  = return $ Just RootDirectory
  isPseudoExecutableBy _ _ = return True
  makePseudoEntry he _ name umask = makeDirectory he name umask

instance PseudoDirectoryEntity Directory where
  data PseudoKey Directory = PseudoDirKey (Key Directory) deriving (Show)
  type PseudoEntry Directory = File

  getPseudoEntity (PseudoDirKey dirkey)  = get dirkey
  isPseudoExecutableBy (PseudoDirKey dirkey) uid = dirkey `isDirectoryExecutableBy` uid
  makePseudoEntry he (PseudoDirKey dirkey) name umask = makeFile he dirkey name umask
  entryAlreadyExistsError (PseudoDirKey key) name = FileAlreadyExists $ T.concat [T.pack $ show key, name]

instance PersistFileEntity Directory where
  type OwnerGroup Directory = DirectoryGroup
  makeOwnerGroup = makeDirectoryGroup
  uniqueOwnerGroup = UniqueDirectoryGroup

  doesNotExistError  key = DirectoryDoesNotExist  (T.pack $ show key)
--  alreadyExistsError key = DirectoryAlreadyExists (T.pack $ show key)
  getFile = get
  ownerField _ = DirectoryUserId
  ownerId = directoryUserId

instance PersistFileEntity File where
  type OwnerGroup File = FileGroup
  makeOwnerGroup = makeFileGroup
  uniqueOwnerGroup = UniqueFileGroup

  doesNotExistError  key = FileDoesNotExist  (T.pack $ show key)
--  alreadyExistsError key = FileAlreadyExists (T.pack $ show key)
  getFile = get
  ownerField _ = FileUserId
  ownerId = fileUserId


-- Rule: SHOULD NEVER THROW IN ANY WAY FROM THE FUNCTIONS IN THIS MODULE.
--       ALWAYS test the validity of insert/update/delete etc.


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
------------------------------ User ------------------------------

su :: MonadIO m => SqlPersistT m (Result UserAccountId)
su = do roots <- select $
                 from $ \userAccount -> do
                   where_ (userAccount^.UserAccountPrivileged ==. val True)
                   limit 1
                   return userAccount

        case roots of
          (Entity root _:_) -> return $ Right root
          []       -> makeRoot
  where
    makeRoot :: MonadIO m => SqlPersistT m (Result UserAccountId)
    makeRoot = do root <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
                  return $ Right root

useradd :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result UserAccountId)
useradd he ident = do
  prv <- isPrivileged he
  if prv
    then  insertDbfs (UserAlreadyExists ident) $ makeUserAccount ident
    else  return $ Left  $ PermissionError "needs to be root to create a user"

userdel :: MonadIO m => UserAccountId -> UserAccountId -> SqlPersistT m (Result UserAccountId)
userdel he him = do
  prv <- isPrivileged he
  uidExists <- userExists him

  if (prv || he == him) && uidExists
    then do ownedGroups <- select $
                           from $ \grp -> do
                             where_ (grp^.GroupOwner ==. val him)
                             return grp

            forM_ (map entityKey ownedGroups) (he `groupdel`)

            delete $
              from $ \groupMembers -> do
                where_ (groupMembers^.GroupMemberMember ==. val him)

            delete $
              from $ \directory -> do
                where_ (directory^.DirectoryUserId ==. val him)

            delete $
              from $ \file -> do
                where_ (file^.FileUserId ==. val him)

            delete $
              from $ \userAccount -> do
                where_ (userAccount^.UserAccountId ==. val him)

            return $ Right he

    else if not uidExists
         then return $ Left $ UserDoesNotExist (T.pack $ show him)
         else return $ Left $ PermissionError (T.pack (show he ++ " needs to be root to delete " ++ show him))


usermod :: MonadIO m
           => UserAccountId -> UserAccountId -> [UserModOption] -> SqlPersistT m (Result UserAccountId)
usermod he him opts =  foldlM (>>>) (Right him) opts
  where
    (Left  a) >>> _  = return $ Left a

    (Right _) >>> (AddToGroup gid) = do
      prv <- isPrivileged he
      own <- he `isGroupOwnerOf`  gid
      if (prv || own)
        then do fmap (const him) <$>
                  insertDbfs (AlreadyGroupMember $ T.pack $ show him ) (makeGroupMember gid him)

        else return $ Left $ PermissionError $
                  T.concat [ T.pack $  show he , "is neither root nor the group owner" ]

    Right _ >>> DelFromGroup gid = do
      prv <- isPrivileged he
      own <- he `isGroupOwnerOf`  gid
      if (prv || own)
        then fmap (const him) <$>
             (deleteByDbfs (NotAGroupMember (T.pack $ show (gid,him))) $ UniqueGroupMember gid him)

        else return $ Left $ PermissionError $
             T.concat [ T.pack $  show he , " is neither root nor the group owner" ]

    Right _ >>> SetDisplayName newName = do
      prv <- isPrivileged he
      let himself = he == him

      if (prv || himself)
        then do update $ \user -> do
                  set user [ UserAccountDisplayName =. val (Just newName) ]
                  where_   (user^.UserAccountId ==. val him)
                return $ Right him
        else do return $ Left $ PermissionError $
                  T.concat [ T.pack $ show he
                           , "is neither root nor the person he is trying to change the display name" ]

    Right _ >>> SetUmask newUmask = do
      prv <- isPrivileged he

      if prv || (he == him)
        then do fmap (fmap (const him)) $ updateDbfs (UserDoesNotExist $ T.pack $ show he)
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
  insertDbfs (GroupAlreadyExists groupName) (makeGroup groupName he)


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
    then do delete $
              from $ \directoryGroup -> do
                where_ (directoryGroup^.DirectoryGroupGroupId ==. val gid)

            delete $
              from $ \fileGroup -> do
                where_ (fileGroup^.FileGroupGroupId ==. val gid)

            delete $
              from $ \groupMember -> do
                where_ (groupMember^.GroupMemberGroupId  ==. val gid)

            delete $
              from $ \grp -> do
                where_ (grp^.GroupId ==. val gid)

            return $ Right he

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
_mkdir' :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result DirectoryId)
_mkdir' he name = do
  muser <- get he

  case muser of
    Just user -> do
      let umask = umaskFromUserAccount user
      edir <- insertDbfs (DirectoryAlreadyExists name) (makeDirectory he name umask)
      case edir of
        Left err  -> return $ Left err
        Right dir -> do
          groups <- belongs he -- should not throw

          case groups of
            Right gs -> do
              forM_  gs $ \g -> do
                insertDbfs (DuplicateDirectoryGroups (T.pack $ show dir)) $ makeDirectoryGroup dir g umask
              return $ Right dir

            Left err -> return $ Left err

    Nothing   -> return $ Left $ UserDoesNotExist $ T.pack $  "user does not exist:" ++ show he



_touch' :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result FileId)
_touch' he dir name = do
  muser <- get he
  mdir  <- get dir
-- Unlike a unix filesystem, creating files is allowed iff the directory is executable, not writable
  x     <- dir `isDirectoryExecutableBy` he
  case (muser, mdir, x) of
    (Just user, Just _ , True) ->  do
      let umask =  umaskFromUserAccount user
      efile <- insertDbfs (FileAlreadyExists name) (makeFile he dir name umask)
      case efile of
        Left  err -> return $ Left err
        Right file -> do
          groups <- belongs he -- should not throw

          case groups of
            Right gs -> do
              forM_  gs $  \g ->
                insertDbfs (DuplicateFileGroups (T.pack $ show file)) (makeFileGroup file g umask)
              return $ Right file
            Left err -> return $ Left err

    (Nothing, _      , _  )  -> return $ Left $ UserDoesNotExist      $ T.pack (show he)
    (_      , Nothing , _ )  -> return $ Left $ DirectoryDoesNotExist $ T.pack (show dir)
    (_ , _ ,  _ ) -> return $ Left $ PermissionError $ T.pack "directory not executable(but MAY be writable)"


touch :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result FileId)
touch he dir name = open he (PseudoDirKey dir) name

touchAt :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result FileId)
touchAt = touch

mkdir :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result DirectoryId)
mkdir he name = open he (PseudoRootKey RootDirectory) name


open :: (MonadIO m
        , PseudoDirectoryEntity dir , PersistEntityBackend (OwnerGroup (PseudoEntry dir)) ~ SqlBackend)
        =>  UserAccountId ->  PseudoKey dir -> Text -> SqlPersistT m (Result (Key (PseudoEntry dir)))
open he dirkey name = do
  muser    <- get  he
  mdir  <- getPseudoEntity dirkey
  x <- dirkey `isPseudoExecutableBy` he
  case (muser, mdir, x) of

    (Just user, Just _dir, True) ->  do
      let umask =  umaskFromUserAccount user
      efile <- insertDbfs (entryAlreadyExistsError dirkey name) (makePseudoEntry he dirkey name umask)
      case efile of
        Left  err -> return $ Left err
        Right file -> do
          groups <- belongs he -- should not throw

          case groups of
            Right gs -> do
              forM_  gs $  \g ->
                insertDbfs (duplicateGroupsError file) (makeOwnerGroup file g umask)
              return $ Right file
            Left err -> return $ Left err

    (Nothing, _      , _  )  -> return $ Left $ UserDoesNotExist      $ T.pack (show he)
    (_      , Nothing , _ )  -> return $ Left $ pseudoEntityDoesNotExistError dirkey
    (_ , _ ,  _ ) -> return $ Left $ PermissionError $ T.pack "parent is not executable(but may be writable)"



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
      => UserAccountId -> Int64 -> Int64 -> SqlPersistT m (Result [FileId])

lsFile uid offs lim = do
  prv <- isPrivileged uid
  results <- select $
             from $ \(file
                      `LeftOuterJoin`
                      (fileGroup `InnerJoin` groupMember )) ->
                    distinctOn [don (file^.FileId) ] $ do
                      on (groupMember^.GroupMemberGroupId ==. fileGroup^.FileGroupGroupId)
                      on (fileGroup^.FileGroupFileId ==. file^.FileId)
                      where_ ( fileOwnerReadable uid file
                               ||. fileGroupReadable uid fileGroup groupMember
                               ||. fileEveryoneReadable uid file
                               ||. val prv ==. val True
                             )
                      offset offs
                      limit lim
                      return (file^.FileId)
  return (Right $ map unValue results)
  where
    fileOwnerReadable uid' file = file^.FileUserId ==. val uid'
                                 &&. file^.FileOwnerR ==. val True

    fileGroupReadable uid' fileGroup groupMember =
      groupMember^.GroupMemberMember ==. val uid'
      &&. fileGroup^.FileGroupGroupR ==. val True

    fileEveryoneReadable _uid' file = file^.FileEveryoneR ==. val True

------------------------------ ReadDirectory  ----------------------------
readDirectory :: MonadIO m => UserAccountId -> DirectoryId -> SqlPersistT m (Result  Directory)
readDirectory he dir = do
  readable <- dir `isDirectoryReadableBy` he
  if readable
    then do ps <- get dir
            case ps of
              Just d  -> return $ Right d
              Nothing -> return $ Left $ DirectoryDoesNotExist $ T.concat [T.pack $ show dir ]
    else return $ Left $ PermissionError $ T.concat [T.pack $ show (he,dir) ]

readFile :: MonadIO m => UserAccountId -> FileId -> SqlPersistT m (Result  File)
readFile he file = do
  readable <- file `isFileReadableBy` he
  if readable
    then do ps <- get file
            case ps of
              Just f  -> return $ Right f
              Nothing -> return $ Left $ FileDoesNotExist $ T.concat [T.pack $ show file ]
    else return $ Left $ PermissionError $ T.concat [T.pack $ show (he,file) ]


--------------------------  WriteDirectory  --------------------------
writeDirectory :: MonadIO m => UserAccountId -> Entity Directory -> SqlPersistT m (Result (Entity Directory))
writeDirectory he directory@(Entity key v) = do
  writable <- key `isDirectoryWritableBy` he
  if writable
    then repsert key v >> return (Right directory)
    else return $ Left $ DirectoryDoesNotExist $ T.concat [ T.pack $ show (he,key) ]

writeFile :: MonadIO m => UserAccountId -> Entity File -> SqlPersistT m (Result (Entity File))
writeFile he file@(Entity key v) = do
  writable <- key `isFileWritableBy` he
  if writable
    then repsert key v >> return (Right file)
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

unlink :: MonadIO m =>UserAccountId -> FileId -> SqlPersistT m (Result ())
unlink he file = do
  writable <- file `isFileWritableBy` he

  case (writable) of
    (True) ->
      do delete $
           from $ \fileTag -> do
             where_ (fileTag^.FileTagFileId ==. val file)

         delete $
           from $ \fileGroup -> do
             where_ (fileGroup^.FileGroupFileId ==. val file)

         delete $
           from $ \file' -> do
             where_ (file'^.FileId ==. val file)
         return $ Right ()

    (False) ->  return $ Left $ PermissionError $ T.concat [T.pack $ show (he,file) ]

rm :: MonadIO m =>UserAccountId -> FileId -> SqlPersistT m (Result ())
rm = unlink

------------------------------  chown --------------------------------



_chownDirectory :: MonadIO m =>
                  UserAccountId -> DirectoryId -> [ChownOption] -> SqlPersistT m (Result DirectoryId)
_chownDirectory he it opts = do
  mdir <- get it
  case mdir of
    Just _  -> foldlM (>>>) (Right it) opts
    Nothing -> return $ Left $ DirectoryDoesNotExist $ T.pack $ show it

  where
    (>>>) :: MonadIO m
             => Result DirectoryId -> ChownOption -> SqlPersistT m (Result DirectoryId)
    (Left err) >>> _ = return $ Left err
    (Right _ ) >>> (ChownOwner uid) =
      do
        prv   <- isPrivileged he
        if prv
          then do update $ \directory -> do
                    set directory [ DirectoryUserId =. val uid ]
                    where_ (directory^.DirectoryId ==. val it)
                  return $ Right it

          else return $ Left $ PermissionError (T.concat [ "only root can change the owner"
                                                         , T.pack $ show he ++ show it ] )

    (Right _ ) >>> (ChownAddGroup gid) =
      do
        prv   <- isPrivileged he
        eumask <- getUserUmask he
        eowner <- getDirectoryUserId it

        case (eumask,eowner , prv || eowner == Right he) of
          (Left err, _ , _ ) ->  return $ Left err
          (_ , Left err , _ ) ->  return $ Left err
          (_ , _   , False) ->  return $ Left $ PermissionError $
                                       (T.concat ["only root or the owner can change the group ownership"])
          (Right umask, Right _ , True ) ->
            fmap (const it) <$>
            insertDbfs (AlreadyOwner $ T.pack $ show gid) (makeDirectoryGroup it gid umask)

    (Right _ ) >>> (ChownDelGroup gid) =
      do
        prv   <- isPrivileged he
        eumask <- getUserUmask he
        eowner <- getDirectoryUserId it

        case (eumask,eowner , prv || eowner == Right he) of
          (Left err, _ , _ ) ->  return $ Left err
          (_ , Left err , _ ) ->  return $ Left err
          (_ , _   , False) ->  return $ Left $ PermissionError $
                                       (T.concat ["only root or the owner can change the group ownership"])
          (Right _, Right _ , True ) ->
            fmap (const it) <$>
            (deleteByDbfs (NotAnOwner $ T.pack $ show gid) $ UniqueDirectoryGroup it gid)




chown :: (MonadIO m , PersistFileEntity val)
         => UserAccountId -> Key val -> [ChownOption] -> SqlPersistT m (Result (Key val))
chown he key opts = do
  mit <- getFile key

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
                   return $ fmap (const k) e

           else return $ Left $ PermissionError (T.concat [ "only root can change the owner"
                                                          , T.pack $ show he ++ show k ] )

     (Right k) >>> (ChownAddGroup gid) =
       do
         prv   <- isPrivileged he
         eumask <- getUserUmask he
         eowner <- getOwnerId k

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

      Right _  >>> ChmodOwner (Perm r w x) =
        updateDbfs (DirectoryDoesNotExist $ T.pack $ show dir)
        dir
        [ (I.=.) DirectoryOwnerR  r
        , (I.=.) DirectoryOwnerW  w
        , (I.=.) DirectoryOwnerX  x
        ]

      Right _  >>> ChmodEveryone (Perm r w x) =
        updateDbfs (DirectoryDoesNotExist $ T.pack $ show dir)
        dir
        [ (I.=.) DirectoryEveryoneR  r
        , (I.=.) DirectoryEveryoneW  w
        , (I.=.) DirectoryEveryoneX  x
        ]

      Right _ >>> ChmodGroup gid (Perm r w x) = do
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

      Right _  >>> ChmodOwner (Perm r w x) =
        updateDbfs (FileDoesNotExist $ T.pack $ show file)
        file
        [ (I.=.) FileOwnerR  r
        , (I.=.) FileOwnerW  w
        , (I.=.) FileOwnerX  x
        ]

      Right _  >>> ChmodEveryone (Perm r w x) =
        updateDbfs (FileDoesNotExist $ T.pack $ show file)
        file
        [ (I.=.) FileEveryoneR  r
        , (I.=.) FileEveryoneW  w
        , (I.=.) FileEveryoneX  x
        ]

      Right _ >>> ChmodGroup gid (Perm r w x) = do
        e <- upsertDbfs (makeFileGroupWithPerm file gid (Perm r w x))
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

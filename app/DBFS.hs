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
       , isPrivileged
       , isGroupOwnerOf
       , belongs
       , userExistsBy
       , getUserDisplayName
       , getDirectoryUserId
       , getDirectoryGroups
       , chown
       , chownDirectory
--       , chownFile
       , chmodDirectory
       , chmodFile

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

         -- , lsPrograms
         -- , lsGoals

         -- , findPrograms
         -- , findGoals

         -- , readProgram
         -- , readGoal

         -- , writeProgram
         -- , writeGoal

         -- , createProgram
         -- , createGoal

         -- , rmProgram
         -- , rmGoal


         -- , programTagsAdd
         -- , programTagsDel
         -- , programTagsFind

         -- , goalTagsAdd
         -- , goalTagsDel
         -- , goalTagsFind

         -- TODO User accounting functions

  ) where


import             Import hiding ((==.), (>=.) ,(||.)  , on , Value , update , (=.) , forM_ , delete)
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
               | PermissionError  Text
               | DuplicateDirectoryGroups Text
               | DuplicateFileGroups Text
               | AlreadyOwner Text
               | NotAnOwner Text
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

deleteDbfs :: (MonadIO m, PersistEntity val, PersistUnique (PersistEntityBackend val) )
              => DbfsError -> Key val -> ReaderT (PersistEntityBackend val) m (Result ())
deleteDbfs err v = do mval <- get v
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
replaceDbfs :: (MonadIO m, PersistEntity val, PersistUnique (PersistEntityBackend val) )
              => DbfsError ->  val -> ReaderT (PersistEntityBackend val) m (Result (Entity val))
replaceDbfs err v = do mukey <- checkUnique v
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
                              where_ (directoryGroups^.DirectoryGroupsDirectoryId ==. val dir)
                              return $ directoryGroups
                    return $ Right (map (directoryGroupsGroupId . entityVal) gs)
       Nothing -> Left  <$> return (DirectoryDoesNotExist $ T.pack $ show dir)
------------------------------ User ------------------------------



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
                where_ (groupMembers^.GroupMembersMember ==. val him)

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
                  insertDbfs (AlreadyGroupMember $ T.pack $ show him ) (makeGroupMembers gid him)

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
              from $ \directoryGroups -> do
                where_ (directoryGroups^.DirectoryGroupsGroupId ==. val gid)

            delete $
              from $ \fileGroups -> do
                where_ (fileGroups^.FileGroupsGroupId ==. val gid)

            delete $
              from $ \groupMembers -> do
                where_ (groupMembers^.GroupMembersGroupId  ==. val gid)

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
                                from $ \groupMembers -> do
                                  where_ (groupMembers^.GroupMembersMember ==. val he)
                                  return groupMembers
                  return $ Right $ map (groupMembersGroupId . entityVal)  groups

    Nothing -> return $ Left $ UserDoesNotExist $ T.pack $ show he



-------------------------- Entity creation  --------------------------
-- In our system, everyone is allowed to create a directory (which is really a prolog program)
mkdir :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result DirectoryId)
mkdir he name = do
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
                insertDbfs (DuplicateDirectoryGroups (T.pack $ show dir)) $ makeDirectoryGroups dir g umask
              return $ Right dir

            Left err -> return $ Left err

    Nothing   -> return $ Left $ UserDoesNotExist $ T.pack $  "user does not exist:" ++ show he



touch :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result FileId)
touch he dir name = do
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
                insertDbfs (DuplicateFileGroups (T.pack $ show file)) (makeFileGroups file g umask)
              return $ Right file
            Left err -> return $ Left err

    (Nothing, _      , _  )  -> return $ Left $ UserDoesNotExist      $ T.pack (show he)
    (_      , Nothing , _ )  -> return $ Left $ DirectoryDoesNotExist $ T.pack (show dir)
    (_ , _ ,  _ ) -> return $ Left $ PermissionError $ T.pack "directory not executable(but MAY be writable)"


touchAt :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result FileId)
touchAt = touch


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
                                => DirectoryId -> UserAccountId -> EntityField DirectoryGroups Bool
                             -> SqlPersistT m Bool
isDirectoryGroupAccessibleBy dir him field = do
  ds <- select $
        from $ \(directory `InnerJoin` directoryGroups `InnerJoin` groupMembers) -> do
          on (groupMembers^.GroupMembersGroupId ==. directoryGroups^.DirectoryGroupsGroupId)
          on (directoryGroups^.DirectoryGroupsDirectoryId ==. directory^.DirectoryId)
          where_ ( directory^.DirectoryId ==. val dir
                   &&. directoryGroups^.field ==. val True
                   &&. groupMembers^.GroupMembersMember ==. val him )
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
                                => FileId -> UserAccountId -> EntityField FileGroups Bool
                             -> SqlPersistT m Bool
isFileGroupAccessibleBy f him field = do
  ds <- select $
        from $ \(file `InnerJoin` fileGroups `InnerJoin` groupMembers) -> do
          on (groupMembers^.GroupMembersGroupId ==. fileGroups^.FileGroupsGroupId)
          on (fileGroups^.FileGroupsFileId ==. file^.FileId)
          where_ ( file^.FileId ==. val f
                   &&. fileGroups^.field ==. val True
                   &&. groupMembers^.GroupMembersMember ==. val him )
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
isDirectoryGroupReadableBy dir him = dir `isDirectoryGroupAccessibleBy` him $ DirectoryGroupsGroupR

isDirectoryGroupWritableBy :: MonadIO m
                              => DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryGroupWritableBy dir him = dir `isDirectoryGroupAccessibleBy` him $ DirectoryGroupsGroupW

isDirectoryGroupExecutableBy :: MonadIO m
                                => DirectoryId -> UserAccountId -> SqlPersistT m Bool
isDirectoryGroupExecutableBy dir him = dir `isDirectoryGroupAccessibleBy` him $  DirectoryGroupsGroupX


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
isFileGroupReadableBy dir him = dir `isFileGroupAccessibleBy` him $ FileGroupsGroupR

isFileGroupWritableBy :: MonadIO m
                              => FileId -> UserAccountId -> SqlPersistT m Bool
isFileGroupWritableBy dir him = dir `isFileGroupAccessibleBy` him $ FileGroupsGroupW

isFileGroupExecutableBy :: MonadIO m
                                => FileId -> UserAccountId -> SqlPersistT m Bool
isFileGroupExecutableBy dir him = dir `isFileGroupAccessibleBy` him $  FileGroupsGroupX


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





-- --------------------  lsHeadPrograms  --------------------

-- lsHeadPrograms :: Int64 -> Int64 -> UserId ->  SqlPersistT m [Entity Directory]
-- lsHeadPrograms lim offs uid  =
--       select $
--       from $ \(program
--                `FullOuterJoin` ownerFlags
--                `FullOuterJoin` everyoneFlags
--                `FullOuterJoin` groupFlags
--                `FullOuterJoin` userGroups )  -> do
--                on (groupFlags^.DirectoryGroupSecurityGroupId ==. userGroups^.GroupMemberGroupId)
--                on (program^.DirectoryId ==. groupFlags^.DirectoryGroupSecurityDirectoryId)
--                on (program^.DirectoryId ==. everyoneFlags^.DirectoryEveryoneSecurityDirectoryId)
--                on (program^.DirectoryId ==. ownerFlags^.DirectoryOwnerSecurityDirectoryId)

--                where_ ( programOwnerReadableCond uid program ownerFlags
--                         ||. programGroupReadableCond uid groupFlags userGroups
--                         ||. programEveryoneReadableCond everyoneFlags )
--                limit lim
--                offset offs
--                return program


-- programOwnerReadableCond :: UserId
--                             -> SqlExpr (Entity Directory)
--                             -> SqlExpr (Entity DirectoryOwnerSecurity)
--                             -> SqlExpr (Value Bool)
-- programOwnerReadableCond uid prog ownerFlags = prog^.DirectoryUserId ==. val uid
--                                             &&. ownerFlags^.DirectoryOwnerSecurityR ==. val True

-- programGroupReadableCond :: UserId
--                             -> SqlExpr (Entity DirectoryGroupSecurity)
--                             -> SqlExpr (Entity GroupMember)
--                             -> SqlExpr (Value Bool)
-- programGroupReadableCond uid groupFlags userGroups = userGroups^.GroupMemberMember ==. val uid
--                                                      &&. groupFlags^.DirectoryGroupSecurityR ==. val True

-- programEveryoneReadableCond :: SqlExpr (Entity DirectoryEveryoneSecurity) -> SqlExpr (Value Bool)
-- programEveryoneReadableCond everyOneFlags = everyOneFlags^.DirectoryEveryoneSecurityR ==. val True


-- -------------------------------- lsHeadGoals ---------------------------------

-- lsHeadGoals :: Int64 -> Int64 -> UserId -> DirectoryId -> SqlPersistT m [Entity File]
-- lsHeadGoals lim offs uid pid = do
--   x <- programExecutable uid pid
--   if x then callSelect else throwM (GoalAccessError "Program goal not listable")

--   where
--     callSelect =
--       select $
--       from $ \(goal
--                `FullOuterJoin` ownerFlags
--                `FullOuterJoin` everyoneFlags
--                `FullOuterJoin` groupFlags
--                `FullOuterJoin` userGroups )  -> do
--                on (groupFlags^.FileGroupSecurityGroupId ==. userGroups^.GroupMemberGroupId)
--                on (goal^.FileId ==. groupFlags^.FileGroupSecurityFileId)
--                on (goal^.FileId ==. everyoneFlags^.FileEveryoneSecurityFileId)
--                on (goal^.FileId ==. ownerFlags^.FileOwnerSecurityFileId)

--                where_ ( goalOwnerReadableCond uid goal ownerFlags
--                         ||. goalGroupReadableCond uid groupFlags userGroups
--                         ||. goalEveryoneReadableCond everyoneFlags )
--                limit lim
--                offset offs
--                return goal

-- goalOwnerReadableCond :: UserId -> SqlExpr (Entity File) -> SqlExpr (Entity FileOwnerSecurity)
--                          -> SqlExpr (Value Bool)
-- goalOwnerReadableCond uid goal ownerFlags = goal^.FileUserId ==. val uid
--                                             &&. ownerFlags^.FileOwnerSecurityR ==. val True

-- goalGroupReadableCond :: UserId -> SqlExpr (Entity FileGroupSecurity) -> SqlExpr (Entity GroupMember)
--                          -> SqlExpr (Value Bool)
-- goalGroupReadableCond uid groupFlags userGroups = userGroups^.GroupMemberMember ==. val uid
--                                                   &&. groupFlags^.FileGroupSecurityR ==. val True

-- goalEveryoneReadableCond :: SqlExpr (Entity FileEveryoneSecurity)
--                             -> SqlExpr (Value Bool)
-- goalEveryoneReadableCond everyOneFlags = everyOneFlags^.FileEveryoneSecurityR ==. val True


-- ---------------------------- FindPrograms ----------------------------

-- findPrograms :: UserId
--           ->  (SqlExpr (Entity Directory) -> SqlExpr (Value Bool))
--           ->  SqlQuery ()
--           ->  SqlPersistT m [Entity Directory]
-- findPrograms  uid programFilter query =
--   select $
--   from $ \((program
--             `FullOuterJoin` programOwnerFlags
--             `FullOuterJoin` programEveryoneFlags
--             `FullOuterJoin` programGroupFlags
--             `FullOuterJoin` programUserGroups)
--           )  -> do
--            on (programUserGroups^.GroupMemberGroupId ==. programGroupFlags^.DirectoryGroupSecurityGroupId)
--            on (program^.DirectoryId ==. programGroupFlags^.DirectoryGroupSecurityDirectoryId)
--            on (program^.DirectoryId ==. programEveryoneFlags^.DirectoryEveryoneSecurityDirectoryId)
--            on (program^.DirectoryId ==. programOwnerFlags^.DirectoryOwnerSecurityDirectoryId)

--            where_ (( programOwnerReadableCond uid program programOwnerFlags
--                      ||. programGroupReadableCond uid programGroupFlags programUserGroups
--                      ||. programEveryoneReadableCond programEveryoneFlags )
--                    &&. programFilter program )
--            query
--            return (program)

-- ---------------------------- FindGoals ----------------------------


-- findGoals :: UserId
--           ->  (SqlExpr (Entity Directory) -> SqlExpr (Value Bool))
--           ->  (SqlExpr (Entity File)    -> SqlExpr (Value Bool))
--           ->  SqlQuery ()
--           ->  SqlPersistT m [(Entity Directory, Entity File)]
-- findGoals  uid programFilter goalFilter query =
--   select $
--   from $ \((program
--             `FullOuterJoin` programOwnerFlags
--             `FullOuterJoin` programEveryoneFlags
--             `FullOuterJoin` programGroupFlags
--             `FullOuterJoin` programUserGroups)
--            `InnerJoin`
--            (goal
--             `FullOuterJoin` goalOwnerFlags
--             `FullOuterJoin` goalEveryoneFlags
--             `FullOuterJoin` goalGroupFlags
--             `FullOuterJoin` goalUserGroups
--            ))  -> do
--            on (goalUserGroups^.GroupMemberGroupId ==. goalGroupFlags^.FileGroupSecurityGroupId)
--            on (goal^.FileId ==. goalGroupFlags^.FileGroupSecurityFileId)
--            on (goal^.FileId ==. goalEveryoneFlags^.FileEveryoneSecurityFileId)
--            on (goal^.FileId ==. goalOwnerFlags^.FileOwnerSecurityFileId)

--            on (goal^.FileDirectoryId ==. program^.DirectoryId)

--            on (programUserGroups^.GroupMemberGroupId ==. programGroupFlags^.DirectoryGroupSecurityGroupId)
--            on (program^.DirectoryId ==. programGroupFlags^.DirectoryGroupSecurityDirectoryId)
--            on (program^.DirectoryId ==. programEveryoneFlags^.DirectoryEveryoneSecurityDirectoryId)
--            on (program^.DirectoryId ==. programOwnerFlags^.DirectoryOwnerSecurityDirectoryId)


--            where_ (( goalOwnerReadableCond uid goal goalOwnerFlags
--                      ||. goalGroupReadableCond uid goalGroupFlags goalUserGroups
--                      ||. goalEveryoneReadableCond goalEveryoneFlags )
--                    &&.
--                    ( programOwnerReadableCond uid program programOwnerFlags
--                      ||. programGroupReadableCond uid programGroupFlags programUserGroups
--                      ||. programEveryoneReadableCond programEveryoneFlags )
--                    &&. programFilter program &&. goalFilter goal )
--            query
--            return (program,goal)



-- ---------------------------- ReadProgram  ----------------------------

-- readProgram :: UserId -> DirectoryId -> SqlPersistT m (Maybe (Entity Directory))
-- readProgram uid pid = do
--   readable <- programReadable uid pid
--   if readable
--     then do ps <- select $
--                   from $ \program -> do
--                     where_ (program^.DirectoryId ==. val pid)
--                     return program
--             case ps of
--               [p] -> return (Just p)
--               _   -> throwM $ FileSystemError "impossible error"
--     else return Nothing

-- ---------------------------- ReadGoal  ----------------------------

-- readGoal :: UserId -> FileId -> SqlPersistT m (Maybe (Entity File))
-- readGoal uid gid = do
--   readable <- goalReadable uid gid
--   if readable
--     then do gs <- select $
--                   from $ \goal -> do
--                     where_ (goal^.FileId ==. val gid)
--                     return goal
--             case gs of
--               [g] -> return (Just g)
--               _   -> throwM $ FileSystemError "impossible error"
--     else return Nothing

-- ----------------------------  Write ------------------------------

-- writeProgram :: UserId -> Entity Directory -> SqlPersistT m (Maybe (Entity Directory))
-- writeProgram uid prog@(Entity pid (Directory uid' name expl code)) = do
--   writable <- programWritable uid pid
--   if writable
--     then do update $ \p -> do
--               set p [ DirectoryUserId      =. val uid'
--                     , DirectoryName        =. val name
--                     , DirectoryExplanation =. val expl
--                     , DirectoryCode        =. val code
--                     ]
--               where_ (p^.DirectoryId ==. val pid)
--             return (Just prog)
--     else
--     return Nothing



-- writeGoal :: UserId -> Entity File -> SqlPersistT m (Maybe (Entity File))
-- writeGoal uid goal@(Entity gid (File uid' pid name expl code)) = do
--   writable <- goalWritable uid gid
--   if writable
--     then do update $ \p -> do
--               set p [ FileUserId      =. val uid'
--                     , FileDirectoryId   =. val pid
--                     , FileName        =. val name
--                     , FileExplanation =. val expl
--                     , FileCode        =. val code
--                     ]
--               where_ (p^.FileId ==. val gid)
--             return (Just goal)
--     else
--     return Nothing


-- ------------------------------  Create  ------------------------------
-- createProgram ::  UserId -> Text -> Text -> Text
--                   -> SqlPersistT m (Maybe DirectoryId)
-- createProgram  uid name expl code = do
--   exists <- userExists uid
--   if not exists
--     then return Nothing
--     else do
--     umask <- userUmask uid
--     pid <- insert $ Directory uid name expl code
--            (not (umaskOwnerR umask))
--            (not (umaskOwnerW umask))
--            (not (umaskOwnerX umask))
--            (not (umaskEveryoneR umask))
--            (not (umaskEveryoneW umask))
--            (not (umaskEveryoneX umask))
--     return $ Just pid

-- createGoal ::  UserId -> DirectoryId -> Text -> Text -> Text
--            -> SqlPersistT m (Maybe DirectoryId)
-- createGoal  uid pid name expl code = do
--   exists <- userExists uid
--   x      <- programExecutable uid pid

--   if not exists || not x
--     then return Nothing
--     else do
--       umask <- userUmask uid
--       pid <- insert $ File uid pid name expl code
--              (not (umaskOwnerR umask))
--              (not (umaskOwnerW umask))
--              (not (umaskOwnerX umask))
--              (not (umaskEveryoneR umask))
--              (not (umaskEveryoneW umask))
--              (not (umaskEveryoneX umask))
--       return $ Just pid


-- ---------------------------- rmdir/unlink ----------------------------
-- rmdirProgram :: UserId -> DirectoryId -> SqlPersistT m (Maybe DirectoryId)
-- rmdirProgram uid pid = do
--   writable <- programWritable uid pid
--   goals    <- lsGoals uid pid
--   if writable && null goals
--     then rm pid
--     else return Nothing

--   where
--     rm pid = do delete $
--                   from $ \p -> do
--                     where_ (p^.ProgramGroupsDirectoryId ==. val pid)
--                 delete $
--                   from $ \p -> do
--                     where_ (p^.DirectorysTagsDirectoryId ==. val pid)
--                 delete $
--                   from $ \p -> do
--                     where_ (p^.DirectoryId ==. val pid)

-- unlinkGoal :: UserId -> FileId -> SqlPersistT m (Maybe FileId)
-- unlinkGoal uid pid = do
--   writable <- programWritable uid pid
--   if writable
--     then unlink' pid
--     else return  Nothing

--   where
--     unlink' pid = do delete $
--                        from $ \p -> do
--                          where_ (p^.GoalGroupsFileId ==. val pid)

--                      delete $
--                        from $ \p -> do
--                          where_ (p^.FilesTagsFileId ==. val pid)

--                      delete $
--                        from $ \p -> do
--                          where_ (p^.FileId ==. val pid)



------------------------------  chown --------------------------------



chownDirectory :: MonadIO m =>
                  UserAccountId -> DirectoryId -> [ChownOption] -> SqlPersistT m (Result DirectoryId)
chownDirectory he it opts = do
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
            insertDbfs (AlreadyOwner $ T.pack $ show gid) (makeDirectoryGroups it gid umask)

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
            (deleteByDbfs (NotAnOwner $ T.pack $ show gid) $ UniqueDirectoryGroups it gid)



class (PersistEntity record, PersistEntity (OwnerGroups record))  => PersistFileEntity record where
  type OwnerGroups record :: *
  makeOwnerGroups   :: Key record -> GroupId -> UMask -> OwnerGroups record
  uniqueOwnerGroups :: Key record -> GroupId -> Unique (OwnerGroups record)

  doesNotExistError  :: Key record -> DbfsError
  alreadyExistsError :: Key record -> DbfsError
  getFile :: (MonadIO m) => Key record -> SqlPersistT m (Maybe record)
  ownerField :: Key record -> EntityField record (Key UserAccount)
  ownerId      :: record -> (Key UserAccount)
  getOwnerId :: MonadIO m => Key record -> SqlPersistT m (Result (Key UserAccount))
  getOwnerId k =  do mu <- getFile k
                     case mu of
                       Just u  -> Right <$> return (ownerId u)
                       Nothing -> Left  <$> return (doesNotExistError k)


instance PersistFileEntity Directory where
  type OwnerGroups Directory = DirectoryGroups
  makeOwnerGroups = makeDirectoryGroups
  uniqueOwnerGroups = UniqueDirectoryGroups

  doesNotExistError  key = DirectoryDoesNotExist  (T.pack $ show key)
  alreadyExistsError key = DirectoryAlreadyExists (T.pack $ show key)
  getFile = get
  ownerField _ = DirectoryUserId
  ownerId = directoryUserId

instance PersistFileEntity File where
  type OwnerGroups File = FileGroups
  makeOwnerGroups = makeFileGroups
  uniqueOwnerGroups = UniqueFileGroups

  doesNotExistError  key = FileDoesNotExist  (T.pack $ show key)
  alreadyExistsError key = FileAlreadyExists (T.pack $ show key)
  getFile = get
  ownerField _ = FileUserId
  ownerId = fileUserId

chown :: (MonadIO m , PersistFileEntity val
         , PersistEntityBackend val ~ SqlBackend, PersistEntityBackend (OwnerGroups val) ~ SqlBackend)
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
             insertDbfs (AlreadyOwner $ T.pack $ show gid) (makeOwnerGroups k gid umask)

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
             (deleteByDbfs (NotAnOwner $ T.pack $ show gid) $ uniqueOwnerGroups k gid)



-- chownGoal :: UserId -> Maybe UserId -> [GroupId] -> FileId
--              -> SqlPersistT m (Maybe FileId)
-- chownGoal uid muid gids goalid = do
--   prv <- privileged uid
--   own <- isOwner uid goalid
--   umask <- userUmask uid

--   case (writable, prv, own , muid ) of
--     (True, _ , Just uid') -> do changeOwner uid' pid
--                                       changeGroup gids pid umask

--     ( True, _ , Nothing )  -> do changeGroup gids pid umask

--     ( _, True, Just uid')  -> do return Nothing

--     ( _, True, Nothing  )  -> do changeGroup gids pid umask

--     (_, _ , _ )  -> return Nothing

--   where
--     changeUser uid goalid = do update $ \p -> do
--                               set p [ FileUserId =. val uid
--                                     ]
--                               where_ (p^.FileId ==. val goalid)
--                             return (Just goalid)

--     changeGroup  gids goalid umask = do delete $ \p -> do
--                                           where_ (p^.GoalGroupsFileId ==. goalid)
--                                         forM_ gids (\gid -> insert (GoalGroups goalid gid
--                                                                     (not (umaskGroupR umask))
--                                                                     (not (umaskGroupW umask))
--                                                                     (not (umaskGroupX umask))))
--                                         return (Just goalid)




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
        e <- upsertDbfs (makeDirectoryGroupsWithPerm dir gid (Perm r w x))
          [ (I.=.) DirectoryGroupsGroupR   r
          , (I.=.) DirectoryGroupsGroupW   w
          , (I.=.) DirectoryGroupsGroupX   x
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
        e <- upsertDbfs (makeFileGroupsWithPerm file gid (Perm r w x))
          [ (I.=.) FileGroupsGroupR   r
          , (I.=.) FileGroupsGroupW   w
          , (I.=.) FileGroupsGroupX   x
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

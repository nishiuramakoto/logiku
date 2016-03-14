{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables
  #-}

module DBFS
       ( DbfsError(..), UserModOptions(..)
       , Perm(..)
       , mkdir
       , touch , touchAt
       , isPrivileged
       , isGroupOwnerOf


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

         -- programReadable
--         , programWritable
--         , programExecutable

         -- , goalReadable
         -- , goalWritable
         -- , goalExecutable

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

         -- , chownProgram
         -- , chownGoal

       , chmodDirectory
       , chmodFile

       , useradd
       , usermod
       , userdel

       , groupadd
         -- , groupmod
       , groupdel

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
import             Database.Esqueleto
import  qualified  Data.Text as T
import             Data.Foldable hiding(null, mapM_)


data DbfsError = DirectoryDoesNotExist Text
               | UserDoesNotExist   Text
               | GroupAlreadyExists Text
               | PermissionError  Text
               deriving (Eq,Show)

type Result a = Either DbfsError a

data UserModOptions = AddToGroup GroupId
                    | DelFromGroup GroupId
                    | SetDisplayName Text
                    deriving (Eq,Show)


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
      dir <- insert (makeDirectory he name umask)
      groups <- belongs he -- should not throw

      case groups of
        Right gs -> do
          forM_  gs $ \g -> do
            liftIO $ putStrLn $ T.pack $ show (dir,g)
            insert $ makeDirectoryGroups dir g umask
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
      file <- insert (makeFile he dir name umask)
      groups <- belongs he -- should not throw

      case groups of
        Right gs -> do
          forM_  gs $  \g -> do
            -- liftIO $ putStrLn $ T.pack $ show (dir,g)
            insert (makeFileGroups file g umask)
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



-- ------------------------------  chown --------------------------------

-- chownProgram :: UserId -> Maybe UserId -> [GroupId] -> DirectoryId
--                 -> SqlPersistT m (Maybe DirectoryId)
-- chownProgram uid muid gids pid = do
--   prv <- privileged uid
--   own <- isOwner uid pid
--   umask <- userUmask uid

--   case (prv, own , muid ) of
--     ( True, _ , Just uid') -> do changeOwner uid' pid
--                                       changeGroup gids pid umask

--     ( True, _ , Nothing )  -> do changeGroup gids pid umask

--     ( _, True, Just uid')  -> do return Nothing

--     ( _, True, Nothing  )  -> do changeGroup gids pid umask

--     ( _, _ , _ )  -> return Nothing

--   where
--     changeOwner uid pid = do update $ \p -> do
--                               set p [ DirectoryUserId =. val uid
--                                     ]
--                               where_ (p^.DirectoryId ==. val pid)
--                             return (Just pid)

--     changeGroup  gids pid umask = do delete $ \p -> do
--                                        where_ (p^.ProgramGroupsDirectoryId ==. pid)
--                                      forM_ gids (\gid -> insert (ProgramGroups pid gid
--                                                                  (not (umaskGroupR umask))
--                                                                  (not (umaskGroupW umask))
--                                                                  (not (umaskGroupX umask))))
--                                      return (Just pid)


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

chmodDirectory :: MonadIO m
                  => UserAccountId -> DirectoryId -> Maybe Perm -> [(GroupId,Perm)] -> Maybe Perm
                  -> SqlPersistT m (Result DirectoryId)
chmodDirectory he dir ownerPerm groupPerms everyonePerm  = do
  own      <- he `isOwnerOfDirectory` dir
  prv      <- isPrivileged he

  if (prv || own)
    then
    do chmodU ownerPerm
       mapM_  chmodG groupPerms
       chmodA  everyonePerm
       return $ Right $ dir
    else
    return $ Left $ PermissionError $ T.pack $ show he ++ " needs to be the owner or root"

    where
      chmodU (Just (Perm r w x))  = do update $ \directory -> do
                                         set directory [ DirectoryOwnerR =. val r
                                                       , DirectoryOwnerW =. val w
                                                       , DirectoryOwnerX =. val x
                                                       ]
                                         where_ (directory^.DirectoryId ==. val dir)
                                       return ()
      chmodU Nothing = return ()

      chmodA (Just (Perm r w x)) = do update $ \directory -> do
                                        set directory [ DirectoryEveryoneR =. val r
                                                      , DirectoryEveryoneW =. val w
                                                      , DirectoryEveryoneX =. val x
                                                      ]
                                        where_ (directory^.DirectoryId ==. val dir)
                                      liftIO $ putStrLn $ T.pack $ show $ Perm r w x
                                      return ()
      chmodA Nothing = return ()


  -- Can set any group permission even if he is not the owner of the group
      chmodG (gid, Perm r w x) = do
        liftIO $ putStrLn $ T.pack $ "chmodG:" ++ show (dir,gid, Perm r w x)
        _dirgrp <- upsert (makeDirectoryGroupsWithPerm dir gid (Perm r w x))
          [ (I.=.) DirectoryGroupsGroupR   r
          , (I.=.) DirectoryGroupsGroupW   w
          , (I.=.) DirectoryGroupsGroupX   x
          ]
        return ()
        -- update $ \directoryGroups -> do
        --   set directoryGroups [ DirectoryGroupsGroupR =. val r
        --                       , DirectoryGroupsGroupW =. val w
        --                       , DirectoryGroupsGroupX =. val x
        --                       ]
        --   where_ (directoryGroups^.DirectoryGroupsDirectoryId ==. val dir
        --           &&. directoryGroups^.DirectoryGroupsGroupId ==. val gid)



isOwnerOfFile :: MonadIO m => UserAccountId -> FileId -> SqlPersistT m Bool
isOwnerOfFile he file = do
  mfile <- get file
  case mfile of
    Just f -> return $ fileUserId f == he
    _      -> return False


chmodFile :: MonadIO m
                  => UserAccountId -> FileId -> Maybe Perm -> [(GroupId,Perm)] -> Maybe Perm
                  -> SqlPersistT m (Result FileId)
chmodFile he file ownerPerm groupPerms everyonePerm  = do
  own      <- he `isOwnerOfFile` file
  prv      <- isPrivileged he

  if (prv || own)
    then
    do chmodU ownerPerm
       mapM_  chmodG groupPerms
       chmodA  everyonePerm
       return $ Right $ file
    else
    return $ Left $ PermissionError $ T.pack $ show he ++ " must be the owner or root"

    where
      chmodU (Just (Perm r w x))  = do update $ \f -> do
                                         set f [ FileOwnerR =. val r
                                               , FileOwnerW =. val w
                                               , FileOwnerX =. val x
                                               ]
                                         where_ (f^.FileId ==. val file)
                                       return ()
      chmodU Nothing = return ()

      chmodA (Just (Perm r w x)) = do update $ \f -> do
                                        set f [ FileEveryoneR =. val r
                                              , FileEveryoneW =. val w
                                              , FileEveryoneX =. val x
                                              ]
                                        where_ (f^.FileId ==. val file)
                                      liftIO $ putStrLn $ T.pack $ show $ Perm r w x
                                      return ()
      chmodA Nothing = return ()


  -- Can set any group permission even if he is not the owner of the group
      chmodG (gid, Perm r w x) = do
        liftIO $ putStrLn $ T.pack $ "chmodG:" ++ show (file,gid, Perm r w x)
        _filegrp <- upsert (makeFileGroupsWithPerm file gid (Perm r w x))
          [ (I.=.) FileGroupsGroupR   r
          , (I.=.) FileGroupsGroupW   w
          , (I.=.) FileGroupsGroupX   x
          ]
        return ()
        -- update $ \directoryGroups -> do
        --   set directoryGroups [ DirectoryGroupsGroupR =. val r
        --                       , DirectoryGroupsGroupW =. val w
        --                       , DirectoryGroupsGroupX =. val x
        --                       ]
        --   where_ (directoryGroups^.DirectoryGroupsDirectoryId ==. val dir
        --           &&. directoryGroups^.DirectoryGroupsGroupId ==. val gid)



 ------------------------------ User ------------------------------


isPrivileged :: MonadIO m => UserAccountId -> SqlPersistT m Bool
isPrivileged he = maybe False userAccountPrivileged <$> get he

useradd :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result UserAccountId)
useradd he ident = do
  prv <- isPrivileged he
  if prv
    then  Right <$>  (insert $ makeUserAccount ident)
    else  return $ Left  $ PermissionError "needs to be root to create a user"


usermod :: MonadIO m
           => UserAccountId -> UserAccountId -> [UserModOptions] -> SqlPersistT m (Result UserAccountId)
usermod he him opts = foldlM (>>>) (Right him) opts
  where
    Left  a   >>> _   = return $ Left a

    Right _ >>> AddToGroup gid = do
      prv <- isPrivileged he
      own <- he `isGroupOwnerOf`  gid
      if (prv || own)
        then do _<- insert $ makeGroupMembers gid him
                return $ Right him
        else do return $ Left $ PermissionError $
                  T.concat [ T.pack $  show he , "is neither root nor the group owner" ]

    Right _ >>> DelFromGroup gid = do
      prv <- isPrivileged he
      own <- he `isGroupOwnerOf`  gid
      if (prv || own)
        then do delete $
                  from $ \groupMembers -> do
                    where_ (groupMembers^.GroupMembersGroupId ==. val gid
                            &&. groupMembers^.GroupMembersMember ==. val him)
                return $ Right him
        else do return $ Left $ PermissionError $
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


isGroupOwnerOf :: MonadIO m => UserAccountId -> GroupId -> SqlPersistT m Bool
isGroupOwnerOf he gid =  do
  mgroup <- get gid
  case mgroup of
    Just grp  -> return (groupOwner grp == he)
    Nothing     -> return False


-- Doesn't delete the programs by default
userdel :: MonadIO m => UserAccountId -> UserAccountId -> SqlPersistT m (Result UserAccountId)
userdel he him = do
  prv <- isPrivileged he

  if prv || he == him
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

    else do
    return $ Left $ PermissionError (T.pack (show he ++ " needs to be root to delete " ++ show him))

--------------------------------  Group --------------------------------
groupadd :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result GroupId)
groupadd he groupName = do
  -- Anyone can make a group (TODO:should this be changed?)
  gid <- insert (makeGroup groupName he)
  return $ Right gid

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

{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables
  #-}

module DBFS
       ( DbfsError(..), UserModOptions(..)

       ,  mkdir
       , touch
       , lsRootByOwnerAccess
       , lsRootByGroupAccess
       , lsRootByEveryoneAccess
       , lsRootOwnerReadable
       , isPrivileged
       , isGroupOwnerOf
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

         -- , chmodProgram
         -- , chmodGoal

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
import             Database.Esqueleto
import  qualified  Data.Text as T
import             Data.Foldable

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


-------------------------- Entity creation  --------------------------
mkdir :: MonadIO m => UserAccountId -> Text -> SqlPersistT m (Result DirectoryId)
mkdir uid name = do
  muser <- get uid
  case muser of
    Just user -> do dir <- insert (makeDirectory uid name (umaskFromUserAccount user))
                    return $ Right dir
    Nothing   -> return $ Left $ UserDoesNotExist $ T.pack $  "user does not exist:" ++ show uid


touch :: MonadIO m => UserAccountId -> DirectoryId -> Text -> SqlPersistT m (Result FileId)
touch uid dir name = do
  muser <- get uid
  mdir  <- get dir
  case (muser, mdir) of
    (Just user, Just _) ->  do file <- insert (makeFile uid dir name (umaskFromUserAccount user))
                               return $ Right file
    (_      , Nothing)  -> return $ Left $ DirectoryDoesNotExist $ T.pack (show dir)
    (Nothing, _      )  -> return $ Left $ UserDoesNotExist      $ T.pack (show uid)


-- lsRoot :: UserId -> Int -> Int -> Handler [ Entity PrologProgram ]
-- lsRoot uid n m
--   | n < m     = findP uid [] [OffsetBy n, LimitTo (m-n) ]
--   | otherwise = findP uid [] []

-- findP :: UserId -> [ Filter PrologProgram ] -> [ SelectOpt PrologProgram ] -> Handler [ Entity PrologProgram ]
-- findP uid filter select = undefined

-- _selectGroupPrograms :: UserId -> SqlPersistT Handler [ (Entity PrologProgram
--                                                        , Entity PrologProgramGroupSecurity
--                                                        , Entity GroupMember) ]
-- _selectGroupPrograms uid   =
--   select $
--   from $ \(prog `InnerJoin` flag `InnerJoin` grp) -> do
--      on (grp ^. GroupMemberGroupId ==. flag ^. PrologProgramGroupSecurityGroupId)
--      on (flag ^. PrologProgramGroupSecurityPrologProgramId ==. prog ^. PrologProgramId)
--      where_ ( grp ^. GroupMemberMember ==. val uid )
--      return ( prog, flag, grp)


---------------------- Access function template ----------------------
lsRootByOwnerAccess :: MonadIO m
                       =>  UserAccountId -> EntityField Directory Bool
                       -> SqlPersistT m [Entity Directory]
lsRootByOwnerAccess uid field =
  select $
    from $ \dir -> do
      where_( dir^.DirectoryUserId  ==. val uid
              &&. dir^.field        ==. val True
          )
      return dir

lsRootByEveryoneAccess ::  MonadIO m
                           => UserAccountId -> EntityField Directory Bool
                           -> SqlPersistT m [Entity Directory]
lsRootByEveryoneAccess uid field =
  select $
    from $ \dir -> do
      where_( dir^.DirectoryUserId ==. val uid
              &&. dir^.field ==. val True
            )
      return dir



lsRootByGroupAccess :: MonadIO m
                       => UserAccountId -> EntityField DirectoryGroups Bool
                       -> SqlPersistT m [Entity Directory]
lsRootByGroupAccess uid field =
  select $
        from $ \(dir `InnerJoin` directoryGroups `InnerJoin` groupMembers) -> do
          on (groupMembers^.GroupMembersGroupId ==. directoryGroups^.DirectoryGroupsGroupId)
          on (directoryGroups^.DirectoryGroupsDirectoryId ==. dir^.DirectoryId)
          where_ ( groupMembers^.GroupMembersMember ==. val uid
                   &&. directoryGroups^.field ==. val True )
          return dir


-- ----------------------  Goal Access templates ------------------------

-- selectGoalOwnerAccess :: UserId -> PrologGoalId -> EntityField PrologGoalOwnerSecurity Bool
--                             -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalOwnerAccess uid pid field =
--   select $
--     from $ \(ownerFlag `InnerJoin` prog) -> do
--       on (ownerFlag^.PrologGoalOwnerSecurityPrologGoalId ==. prog^.PrologGoalId)
--       where_( prog^.PrologGoalUserId ==. val uid
--               &&. prog^.PrologGoalId ==. val pid
--               &&. ownerFlag^. field ==. val True
--           )
--       return prog

-- selectGoalEveryoneAccess ::  PrologGoalId -> EntityField PrologGoalEveryoneSecurity Bool
--                                 -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalEveryoneAccess pid field =
--   select $
--     from $ \(everyoneFlag `InnerJoin` prog) -> do
--       on (everyoneFlag^.PrologGoalEveryoneSecurityPrologGoalId ==. prog^.PrologGoalId)
--       where_( prog^.PrologGoalId ==. val pid
--               &&. everyoneFlag^. field ==. val True
--             )
--       return prog



-- selectGoalGroupAccess ::  UserId -> PrologGoalId -> EntityField PrologGoalGroupSecurity Bool
--                              -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalGroupAccess uid pid field =
--   select $
--         from $ \(prog `InnerJoin` flag `InnerJoin` grp) -> do
--           on (grp^.GroupMemberGroupId ==. flag^.PrologGoalGroupSecurityGroupId)
--           on (flag^.PrologGoalGroupSecurityPrologGoalId ==. prog^.PrologGoalId)
--           where_ ( grp^.GroupMemberMember ==. val uid
--                    &&. prog^.PrologGoalId ==. val pid
--                    &&. flag^. field ==. val True )
--           return prog



-- ------------------------ Permission API for programs  ------------------------
lsRootOwnerReadable :: MonadIO m
                                =>  UserAccountId -> SqlPersistT m [Entity Directory]
lsRootOwnerReadable uid = lsRootByOwnerAccess uid  DirectoryOwnerR

-- selectProgramOwnerWritable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
-- selectProgramOwnerWritable uid pid = selectProgramOwnerAccess uid pid  PrologProgramOwnerSecurityW

-- selectProgramOwnerExecutable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
-- selectProgramOwnerExecutable uid pid = selectProgramOwnerAccess uid pid  PrologProgramOwnerSecurityX

-- selectProgramGroupReadable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
-- selectProgramGroupReadable uid pid = selectProgramGroupAccess uid pid  PrologProgramGroupSecurityR

-- selectProgramGroupWritable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
-- selectProgramGroupWritable uid pid = selectProgramGroupAccess uid pid  PrologProgramGroupSecurityW

-- selectProgramGroupExecutable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
-- selectProgramGroupExecutable uid pid = selectProgramGroupAccess uid pid  PrologProgramGroupSecurityX

-- selectProgramEveryoneReadable :: PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
-- selectProgramEveryoneReadable  pid = selectProgramEveryoneAccess pid PrologProgramEveryoneSecurityR

-- selectProgramEveryoneWritable ::  PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
-- selectProgramEveryoneWritable  pid = selectProgramEveryoneAccess pid PrologProgramEveryoneSecurityW

-- selectProgramEveryoneExecutable :: PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
-- selectProgramEveryoneExecutable  pid = selectProgramEveryoneAccess pid PrologProgramEveryoneSecurityX


-- programReadable :: UserId -> PrologProgramId -> SqlPersistT Handler Bool
-- programReadable uid pid = negateMaybe' $ runMaybeT $ do
--   _<- MaybeT $ negateMaybe $ selectProgramOwnerReadable uid pid
--   _<- MaybeT $ negateMaybe $ selectProgramEveryoneReadable pid
--   do  MaybeT $ negateMaybe $ selectProgramGroupReadable uid pid

-- programWritable :: UserId -> PrologProgramId -> SqlPersistT Handler Bool
-- programWritable uid pid = negateMaybe' $ runMaybeT $ do
--   _<- MaybeT $ negateMaybe $ selectProgramOwnerWritable uid pid
--   _<- MaybeT $ negateMaybe $ selectProgramEveryoneWritable pid
--   do  MaybeT $ negateMaybe $ selectProgramGroupWritable uid pid

-- programExecutable :: UserId -> PrologProgramId -> SqlPersistT Handler Bool
-- programExecutable uid pid = negateMaybe' $ runMaybeT $ do
--   _<- MaybeT $ negateMaybe $ selectProgramOwnerExecutable uid pid
--   _<- MaybeT $ negateMaybe $ selectProgramEveryoneExecutable pid
--   do  MaybeT $ negateMaybe $ selectProgramGroupExecutable uid pid


-- ------------------------ Permission Api for Goals  ------------------------

-- selectGoalOwnerReadable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalOwnerReadable uid pid = selectGoalOwnerAccess uid pid  PrologGoalOwnerSecurityR

-- selectGoalOwnerWritable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalOwnerWritable uid pid = selectGoalOwnerAccess uid pid  PrologGoalOwnerSecurityW

-- selectGoalOwnerExecutable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalOwnerExecutable uid pid = selectGoalOwnerAccess uid pid  PrologGoalOwnerSecurityX

-- selectGoalGroupReadable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalGroupReadable uid pid = selectGoalGroupAccess uid pid  PrologGoalGroupSecurityR

-- selectGoalGroupWritable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalGroupWritable uid pid = selectGoalGroupAccess uid pid  PrologGoalGroupSecurityW

-- selectGoalGroupExecutable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalGroupExecutable uid pid = selectGoalGroupAccess uid pid  PrologGoalGroupSecurityX

-- selectGoalEveryoneReadable ::  PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalEveryoneReadable  pid = selectGoalEveryoneAccess pid PrologGoalEveryoneSecurityR

-- selectGoalEveryoneWritable :: PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalEveryoneWritable  pid = selectGoalEveryoneAccess pid PrologGoalEveryoneSecurityW

-- selectGoalEveryoneExecutable :: PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
-- selectGoalEveryoneExecutable  pid = selectGoalEveryoneAccess pid PrologGoalEveryoneSecurityX


-- goalReadable :: UserId -> PrologGoalId -> SqlPersistT Handler Bool
-- goalReadable uid pid = negateMaybe' $ runMaybeT $ do
--   _ <- MaybeT $ negateMaybe $ selectGoalOwnerReadable uid pid
--   _ <- MaybeT $ negateMaybe $ selectGoalEveryoneReadable pid
--   do   MaybeT $ negateMaybe $ selectGoalGroupReadable uid pid

-- goalWritable :: UserId -> PrologGoalId -> SqlPersistT Handler Bool
-- goalWritable uid pid = negateMaybe' $ runMaybeT $ do
--   _ <- MaybeT $ negateMaybe $ selectGoalOwnerWritable uid pid
--   _ <- MaybeT $ negateMaybe $ selectGoalEveryoneWritable pid
--   do   MaybeT $ negateMaybe $ selectGoalGroupWritable uid pid

-- goalExecutable :: UserId -> PrologGoalId -> SqlPersistT Handler Bool
-- goalExecutable uid pid = negateMaybe' $ runMaybeT $ do
--   _ <- MaybeT $ negateMaybe $ selectGoalOwnerExecutable uid pid
--   _ <- MaybeT $ negateMaybe $ selectGoalEveryoneExecutable pid
--   do   MaybeT $ negateMaybe $ selectGoalGroupExecutable uid pid




-- --------------------  lsHeadPrograms  --------------------

-- lsHeadPrograms :: Int64 -> Int64 -> UserId ->  SqlPersistT Handler [Entity PrologProgram]
-- lsHeadPrograms lim offs uid  =
--       select $
--       from $ \(program
--                `FullOuterJoin` ownerFlags
--                `FullOuterJoin` everyoneFlags
--                `FullOuterJoin` groupFlags
--                `FullOuterJoin` userGroups )  -> do
--                on (groupFlags^.PrologProgramGroupSecurityGroupId ==. userGroups^.GroupMemberGroupId)
--                on (program^.PrologProgramId ==. groupFlags^.PrologProgramGroupSecurityPrologProgramId)
--                on (program^.PrologProgramId ==. everyoneFlags^.PrologProgramEveryoneSecurityPrologProgramId)
--                on (program^.PrologProgramId ==. ownerFlags^.PrologProgramOwnerSecurityPrologProgramId)

--                where_ ( programOwnerReadableCond uid program ownerFlags
--                         ||. programGroupReadableCond uid groupFlags userGroups
--                         ||. programEveryoneReadableCond everyoneFlags )
--                limit lim
--                offset offs
--                return program


-- programOwnerReadableCond :: UserId
--                             -> SqlExpr (Entity PrologProgram)
--                             -> SqlExpr (Entity PrologProgramOwnerSecurity)
--                             -> SqlExpr (Value Bool)
-- programOwnerReadableCond uid prog ownerFlags = prog^.PrologProgramUserId ==. val uid
--                                             &&. ownerFlags^.PrologProgramOwnerSecurityR ==. val True

-- programGroupReadableCond :: UserId
--                             -> SqlExpr (Entity PrologProgramGroupSecurity)
--                             -> SqlExpr (Entity GroupMember)
--                             -> SqlExpr (Value Bool)
-- programGroupReadableCond uid groupFlags userGroups = userGroups^.GroupMemberMember ==. val uid
--                                                      &&. groupFlags^.PrologProgramGroupSecurityR ==. val True

-- programEveryoneReadableCond :: SqlExpr (Entity PrologProgramEveryoneSecurity) -> SqlExpr (Value Bool)
-- programEveryoneReadableCond everyOneFlags = everyOneFlags^.PrologProgramEveryoneSecurityR ==. val True


-- -------------------------------- lsHeadGoals ---------------------------------

-- lsHeadGoals :: Int64 -> Int64 -> UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologGoal]
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
--                on (groupFlags^.PrologGoalGroupSecurityGroupId ==. userGroups^.GroupMemberGroupId)
--                on (goal^.PrologGoalId ==. groupFlags^.PrologGoalGroupSecurityPrologGoalId)
--                on (goal^.PrologGoalId ==. everyoneFlags^.PrologGoalEveryoneSecurityPrologGoalId)
--                on (goal^.PrologGoalId ==. ownerFlags^.PrologGoalOwnerSecurityPrologGoalId)

--                where_ ( goalOwnerReadableCond uid goal ownerFlags
--                         ||. goalGroupReadableCond uid groupFlags userGroups
--                         ||. goalEveryoneReadableCond everyoneFlags )
--                limit lim
--                offset offs
--                return goal

-- goalOwnerReadableCond :: UserId -> SqlExpr (Entity PrologGoal) -> SqlExpr (Entity PrologGoalOwnerSecurity)
--                          -> SqlExpr (Value Bool)
-- goalOwnerReadableCond uid goal ownerFlags = goal^.PrologGoalUserId ==. val uid
--                                             &&. ownerFlags^.PrologGoalOwnerSecurityR ==. val True

-- goalGroupReadableCond :: UserId -> SqlExpr (Entity PrologGoalGroupSecurity) -> SqlExpr (Entity GroupMember)
--                          -> SqlExpr (Value Bool)
-- goalGroupReadableCond uid groupFlags userGroups = userGroups^.GroupMemberMember ==. val uid
--                                                   &&. groupFlags^.PrologGoalGroupSecurityR ==. val True

-- goalEveryoneReadableCond :: SqlExpr (Entity PrologGoalEveryoneSecurity)
--                             -> SqlExpr (Value Bool)
-- goalEveryoneReadableCond everyOneFlags = everyOneFlags^.PrologGoalEveryoneSecurityR ==. val True


-- ---------------------------- FindPrograms ----------------------------

-- findPrograms :: UserId
--           ->  (SqlExpr (Entity PrologProgram) -> SqlExpr (Value Bool))
--           ->  SqlQuery ()
--           ->  SqlPersistT Handler [Entity PrologProgram]
-- findPrograms  uid programFilter query =
--   select $
--   from $ \((program
--             `FullOuterJoin` programOwnerFlags
--             `FullOuterJoin` programEveryoneFlags
--             `FullOuterJoin` programGroupFlags
--             `FullOuterJoin` programUserGroups)
--           )  -> do
--            on (programUserGroups^.GroupMemberGroupId ==. programGroupFlags^.PrologProgramGroupSecurityGroupId)
--            on (program^.PrologProgramId ==. programGroupFlags^.PrologProgramGroupSecurityPrologProgramId)
--            on (program^.PrologProgramId ==. programEveryoneFlags^.PrologProgramEveryoneSecurityPrologProgramId)
--            on (program^.PrologProgramId ==. programOwnerFlags^.PrologProgramOwnerSecurityPrologProgramId)

--            where_ (( programOwnerReadableCond uid program programOwnerFlags
--                      ||. programGroupReadableCond uid programGroupFlags programUserGroups
--                      ||. programEveryoneReadableCond programEveryoneFlags )
--                    &&. programFilter program )
--            query
--            return (program)

-- ---------------------------- FindGoals ----------------------------


-- findGoals :: UserId
--           ->  (SqlExpr (Entity PrologProgram) -> SqlExpr (Value Bool))
--           ->  (SqlExpr (Entity PrologGoal)    -> SqlExpr (Value Bool))
--           ->  SqlQuery ()
--           ->  SqlPersistT Handler [(Entity PrologProgram, Entity PrologGoal)]
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
--            on (goalUserGroups^.GroupMemberGroupId ==. goalGroupFlags^.PrologGoalGroupSecurityGroupId)
--            on (goal^.PrologGoalId ==. goalGroupFlags^.PrologGoalGroupSecurityPrologGoalId)
--            on (goal^.PrologGoalId ==. goalEveryoneFlags^.PrologGoalEveryoneSecurityPrologGoalId)
--            on (goal^.PrologGoalId ==. goalOwnerFlags^.PrologGoalOwnerSecurityPrologGoalId)

--            on (goal^.PrologGoalPrologProgramId ==. program^.PrologProgramId)

--            on (programUserGroups^.GroupMemberGroupId ==. programGroupFlags^.PrologProgramGroupSecurityGroupId)
--            on (program^.PrologProgramId ==. programGroupFlags^.PrologProgramGroupSecurityPrologProgramId)
--            on (program^.PrologProgramId ==. programEveryoneFlags^.PrologProgramEveryoneSecurityPrologProgramId)
--            on (program^.PrologProgramId ==. programOwnerFlags^.PrologProgramOwnerSecurityPrologProgramId)


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

-- readProgram :: UserId -> PrologProgramId -> SqlPersistT Handler (Maybe (Entity PrologProgram))
-- readProgram uid pid = do
--   readable <- programReadable uid pid
--   if readable
--     then do ps <- select $
--                   from $ \program -> do
--                     where_ (program^.PrologProgramId ==. val pid)
--                     return program
--             case ps of
--               [p] -> return (Just p)
--               _   -> throwM $ FileSystemError "impossible error"
--     else return Nothing

-- ---------------------------- ReadGoal  ----------------------------

-- readGoal :: UserId -> PrologGoalId -> SqlPersistT Handler (Maybe (Entity PrologGoal))
-- readGoal uid gid = do
--   readable <- goalReadable uid gid
--   if readable
--     then do gs <- select $
--                   from $ \goal -> do
--                     where_ (goal^.PrologGoalId ==. val gid)
--                     return goal
--             case gs of
--               [g] -> return (Just g)
--               _   -> throwM $ FileSystemError "impossible error"
--     else return Nothing

-- ----------------------------  Write ------------------------------

-- writeProgram :: UserId -> Entity PrologProgram -> SqlPersistT Handler (Maybe (Entity PrologProgram))
-- writeProgram uid prog@(Entity pid (PrologProgram uid' name expl code)) = do
--   writable <- programWritable uid pid
--   if writable
--     then do update $ \p -> do
--               set p [ PrologProgramUserId      =. val uid'
--                     , PrologProgramName        =. val name
--                     , PrologProgramExplanation =. val expl
--                     , PrologProgramCode        =. val code
--                     ]
--               where_ (p^.PrologProgramId ==. val pid)
--             return (Just prog)
--     else
--     return Nothing



-- writeGoal :: UserId -> Entity PrologGoal -> SqlPersistT Handler (Maybe (Entity PrologGoal))
-- writeGoal uid goal@(Entity gid (PrologGoal uid' pid name expl code)) = do
--   writable <- goalWritable uid gid
--   if writable
--     then do update $ \p -> do
--               set p [ PrologGoalUserId      =. val uid'
--                     , PrologGoalPrologProgramId   =. val pid
--                     , PrologGoalName        =. val name
--                     , PrologGoalExplanation =. val expl
--                     , PrologGoalCode        =. val code
--                     ]
--               where_ (p^.PrologGoalId ==. val gid)
--             return (Just goal)
--     else
--     return Nothing


-- ------------------------------  Create  ------------------------------
-- createProgram ::  UserId -> Text -> Text -> Text
--                   -> SqlPersistT Handler (Maybe PrologProgramId)
-- createProgram  uid name expl code = do
--   exists <- userExists uid
--   if not exists
--     then return Nothing
--     else do
--     umask <- userUmask uid
--     pid <- insert $ PrologProgram uid name expl code
--            (not (umaskOwnerR umask))
--            (not (umaskOwnerW umask))
--            (not (umaskOwnerX umask))
--            (not (umaskEveryoneR umask))
--            (not (umaskEveryoneW umask))
--            (not (umaskEveryoneX umask))
--     return $ Just pid

-- createGoal ::  UserId -> PrologProgramId -> Text -> Text -> Text
--            -> SqlPersistT Handler (Maybe PrologProgramId)
-- createGoal  uid pid name expl code = do
--   exists <- userExists uid
--   x      <- programExecutable uid pid

--   if not exists || not x
--     then return Nothing
--     else do
--       umask <- userUmask uid
--       pid <- insert $ PrologGoal uid pid name expl code
--              (not (umaskOwnerR umask))
--              (not (umaskOwnerW umask))
--              (not (umaskOwnerX umask))
--              (not (umaskEveryoneR umask))
--              (not (umaskEveryoneW umask))
--              (not (umaskEveryoneX umask))
--       return $ Just pid


-- ---------------------------- rmdir/unlink ----------------------------
-- rmdirProgram :: UserId -> PrologProgramId -> SqlPersistT Handler (Maybe PrologProgramId)
-- rmdirProgram uid pid = do
--   writable <- programWritable uid pid
--   goals    <- lsGoals uid pid
--   if writable && null goals
--     then rm pid
--     else return Nothing

--   where
--     rm pid = do delete $
--                   from $ \p -> do
--                     where_ (p^.ProgramGroupsPrologProgramId ==. val pid)
--                 delete $
--                   from $ \p -> do
--                     where_ (p^.PrologProgramsTagsPrologProgramId ==. val pid)
--                 delete $
--                   from $ \p -> do
--                     where_ (p^.PrologProgramId ==. val pid)

-- unlinkGoal :: UserId -> PrologGoalId -> SqlPersistT Handler (Maybe PrologGoalId)
-- unlinkGoal uid pid = do
--   writable <- programWritable uid pid
--   if writable
--     then unlink' pid
--     else return  Nothing

--   where
--     unlink' pid = do delete $
--                        from $ \p -> do
--                          where_ (p^.GoalGroupsPrologGoalId ==. val pid)

--                      delete $
--                        from $ \p -> do
--                          where_ (p^.PrologGoalsTagsPrologGoalId ==. val pid)

--                      delete $
--                        from $ \p -> do
--                          where_ (p^.PrologGoalId ==. val pid)



-- ------------------------------  chown --------------------------------

-- chownProgram :: UserId -> Maybe UserId -> [GroupId] -> PrologProgramId
--                 -> SqlPersistT Handler (Maybe PrologProgramId)
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
--                               set p [ PrologProgramUserId =. val uid
--                                     ]
--                               where_ (p^.PrologProgramId ==. val pid)
--                             return (Just pid)

--     changeGroup  gids pid umask = do delete $ \p -> do
--                                        where_ (p^.ProgramGroupsPrologProgramId ==. pid)
--                                      forM_ gids (\gid -> insert (ProgramGroups pid gid
--                                                                  (not (umaskGroupR umask))
--                                                                  (not (umaskGroupW umask))
--                                                                  (not (umaskGroupX umask))))
--                                      return (Just pid)


-- chownGoal :: UserId -> Maybe UserId -> [GroupId] -> PrologGoalId
--              -> SqlPersistT Handler (Maybe PrologGoalId)
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
--                               set p [ PrologGoalUserId =. val uid
--                                     ]
--                               where_ (p^.PrologGoalId ==. val goalid)
--                             return (Just goalid)

--     changeGroup  gids goalid umask = do delete $ \p -> do
--                                           where_ (p^.GoalGroupsPrologGoalId ==. goalid)
--                                         forM_ gids (\gid -> insert (GoalGroups goalid gid
--                                                                     (not (umaskGroupR umask))
--                                                                     (not (umaskGroupW umask))
--                                                                     (not (umaskGroupX umask))))
--                                         return (Just goalid)


-- ------------------------------  chmod --------------------------------
-- chmodProgram :: UserId ->  Maybe Perm -> [(GroupId,Perm)] -> Maybe Perm -> PrologProgramId
--                 -> SqlPersistT Handler (Maybe PrologProgramId)
-- chmodProgram uid muperm gperms maperm pid = do
--   own      <- isOwner uid pid
--   prv      <- privileged uid

--   if (prv || own)
--     then
--     do chmodU uid umperm pid
--        chmodG uid gids  pid
--        chmodA uid amperm pid
--     else
--     return Nothing

--     where
--       chmodU uid (Just (Perm r w x)) pid = do update $ \p -> do
--                                           set p [ PrologProgramOwnerR =. val r
--                                                 , PrologProgramOwnerW =. val w
--                                                 , PrologProgramOwnerX =. val x
--                                                 ]
--                                           where_ (p^.PrologProgramId ==. val pid)
--       chmodU uid Nothing pid = return Nothing

--       chmodA uid (Just (Perm r w x)) pid = do update $ \p -> do
--                                           set p [ PrologProgramEveryoneR =. val r
--                                                 , PrologProgramEveryoneW =. val w
--                                                 , PrologProgramEveryoneX =. val x
--                                                 ]
--                                           where_ (p^.PrologProgramId ==. val pid)
--       chmodA uid Nothing pid = return Nothing

--       chmodG uid gids pid = mapM_ chmodG'each gids
--         where
--           chmodG'each (gid, Perm r w x) = do update $ \p -> do
--                                                set p [ ProgramGroupsGroupR =. val r
--                                                      , ProgramGroupsGroupW =. val w
--                                                      , ProgramGroupsGroupX =. val x
--                                                      ]
--                                                where_ (p^.ProgramGroupsPrologProgramId ==. val pid
--                                                        &&. p^.ProgramGroupsGroupId ==. gid)



-- chmodProgram :: UserId ->  Maybe Perm -> [(GroupId,Perm)] -> Maybe Perm -> PrologGoalId
--                 -> SqlPersistT Handler (Maybe PrologGoalId)
-- chmodProgram uid muperm gperms maperm pid = do
--   own      <- isOwner uid pid
--   prv      <- privileged uid

--   if (prv || own)
--     then
--     do chmodU uid umperm pid
--        chmodG uid gids  pid
--        chmodA uid amperm pid
--     else
--     return Nothing

--     where
--       chmodU uid (Just (Perm r w x)) pid = do update $ \p -> do
--                                           set p [ PrologGoalOwnerR =. val r
--                                                 , PrologGoalOwnerW =. val w
--                                                 , PrologGoalOwnerX =. val x
--                                                 ]
--                                           where_ (p^.PrologGoalId ==. val pid)
--       chmodU uid Nothing pid = return Nothing

--       chmodA uid (Just (Perm r w x)) pid = do update $ \p -> do
--                                           set p [ PrologGoalEveryoneR =. val r
--                                                 , PrologGoalEveryoneW =. val w
--                                                 , PrologGoalEveryoneX =. val x
--                                                 ]
--                                           where_ (p^.PrologGoalId ==. val pid)
--       chmodA uid Nothing pid = return Nothing

--       chmodG uid gids pid = mapM_ chmodG'each gids
--         where
--           chmodG'each (gid, Perm r w x) = do update $ \p -> do
--                                                set p [ GoalGroupsGroupR =. val r
--                                                      , GoalGroupsGroupW =. val w
--                                                      , GaolGroupsGroupX =. val x
--                                                      ]
--                                                where_ (p^.GoalGroupsPrologGoalId ==. val pid
--                                                        &&. p^.GoalGroupsGroupId ==. gid)



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

-- groupmod :: UserId -> GroupId -> Text -> Maybe Text -> SqlPersistT Handler (Result GroupId)
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

-- valid :: SqlPersistT Handler Bool
-- valid = allM [ mapM (\g -> validPid (prologGoalPrologProgramId g)) =<< allGoals
--              ,  mapM (\g -> validGid (groupMembersGroupId g)) =<< allGroupMember
--              ,  mapM (\g -> validUid (groupMembersMember g)) =<< allGroupMember
--              ,  mapM (\g -> validPid (programGroupsPrologProgramId g)) =<< allProgramGroups
--              ,  mapM (\g -> validGid (programGroupsGroupId g)) =<<  allProgramGroups
--              ,  mapM (\g -> validGoalId (goalGroupsPrologGoalId g)) =<< allGoalGroups
--              ,  mapM (\g -> validGid (goalGroupsGroupId g)) =<< allGoalGroups
--              ,  mapM (\g -> validPid (prologProgramTagsPrologProgramId g)) =<< allPrologProgramsTags
--              ,  mapM (\g -> validTagId (prologProgramTagsTagId g)) =<< allPrologProgramsTags
--              ,  mapM (\g -> validGoalid (prologGoalTagsPrologGoalId g)) =<< allPrologGoalTags
--              ,  mapM (\g -> validTagId (prologGoalTagsTagId g)) =<< allPrologGoalTags
--              ]


------------------------ Auxillary functions  ------------------------
_negateMaybe :: Monad m => m [a] -> m (Maybe Bool)
_negateMaybe m  = do x <- m
                     case x of
                       []  -> return (Just True)
                       _   -> return Nothing

_negateMaybe' :: Monad m => m (Maybe Bool) -> m Bool
_negateMaybe' m = do x <- m
                     case x of
                       Just _ -> return False
                       Nothing -> return True

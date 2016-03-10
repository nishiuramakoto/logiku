module DBFS
       (   programReadable
         , programWritable
         , programExecutable

         , goalReadable
         , goalWritable
         , goalExecutable

         , lsPrograms
         , lsGoals

         , findPrograms
         , findGoals

         , readProgram
         , readGoal

         , writeProgram
         , writeGoal

         , createProgram
         , createGoal

         , rmProgram
         , rmGoal

         , chownProgram
         , chownGoal

         , chmodProgram
         , chmodGoal

         , useradd
         , usermod
         , userdel

         , groupadd
         , groupmod
         , groupdel

         , programTagsAdd
         , programTagsDel
         , programTagsFind

         , goalTagsAdd
         , goalTagsDel
         , goalTagsFind

           -- TODO User accounting functions
  ) where


import             Import hiding ((==.), (>=.) ,(||.)  , on , Value , update , (=.) )
import             Database.Esqueleto
import             Control.Monad.Trans.Maybe


data DbfsError = GoalAccessError String
               | ProgramAccessError String
               | FileSystemError String
               deriving (Show)

data Perm = Perm R W X
            deriving Show

type UMask = Perm
type Result a = Either DbfsError a

_test1 :: SqlPersistT Handler [Entity PrologGoalEveryoneSecurity]
_test1 = do people <- select $
                     from $ \person -> do
                       return person
            return people


_test2 :: SqlPersistT Handler [Entity PrologGoalEveryoneSecurity]
_test2 = do people <- select $
                     from $ \person -> do
                       where_ (person ^. PrologGoalEveryoneSecurityW ==. val True)
                       return person
            return people

_test3 :: SqlPersistT Handler [Entity Group]
_test3 =  select $
         from $ \p -> do
           where_ (p ^. GroupExplanation ==. just (val "abc"))
           return p


_test4 :: SqlPersistT Handler [(Entity PrologProgram, Entity PrologGoal)]
_test4 = select $
        from $ \(b,p) -> do
          where_ (b ^. PrologProgramUserId ==. p ^. PrologGoalUserId)
          orderBy [asc (b ^. PrologProgramName)]
          return (b,p)

_test5 :: SqlPersistT Handler [(Entity PrologProgram, Maybe (Entity PrologGoal))]
_test5 = select $
        from $ \(p `LeftOuterJoin` mb) -> do
          on (just (p ^. PrologProgramUserId) ==. mb ?. PrologGoalUserId)
          orderBy [asc (p ^. PrologProgramName), asc (mb ?. PrologGoalName) ]
          return (p,mb)

_test6 :: SqlPersistT Handler [(Entity PrologProgram , Entity PrologGoal, Entity Group)]
_test6 = select $
        from $ \(p1 `InnerJoin` f `InnerJoin` p2) -> do
          on (p2 ^. GroupOwner ==. f ^. PrologGoalUserId )
          on (p1 ^. PrologProgramUserId ==. f ^. PrologGoalUserId)
          return (p1,f,p2)




-- lsHeadP :: UserId -> Int -> Int -> Handler [ Entity PrologProgram ]
-- lsHeadP uid n m
--   | n < m     = findP uid [] [OffsetBy n, LimitTo (m-n) ]
--   | otherwise = findP uid [] []

-- findP :: UserId -> [ Filter PrologProgram ] -> [ SelectOpt PrologProgram ] -> Handler [ Entity PrologProgram ]
-- findP uid filter select = undefined

_selectGroupPrograms :: UserId -> SqlPersistT Handler [ (Entity PrologProgram
                                                       , Entity PrologProgramGroupSecurity
                                                       , Entity GroupMember) ]
_selectGroupPrograms uid   =
  select $
  from $ \(prog `InnerJoin` flag `InnerJoin` grp) -> do
     on (grp ^. GroupMemberGroupId ==. flag ^. PrologProgramGroupSecurityGroupId)
     on (flag ^. PrologProgramGroupSecurityPrologProgramId ==. prog ^. PrologProgramId)
     where_ ( grp ^. GroupMemberMember ==. val uid )
     return ( prog, flag, grp)


---------------------- Access function template ----------------------
selectProgramOwnerAccess :: UserId -> PrologProgramId -> EntityField PrologProgramOwnerSecurity Bool
                            -> SqlPersistT Handler [Entity PrologProgram]
selectProgramOwnerAccess uid pid field =
  select $
    from $ \(ownerFlag `InnerJoin` prog) -> do
      on (ownerFlag^.PrologProgramOwnerSecurityPrologProgramId ==. prog^.PrologProgramId)
      where_( prog^.PrologProgramUserId ==. val uid
              &&. prog^.PrologProgramId ==. val pid
              &&. ownerFlag^. field ==. val True
          )
      return prog

selectProgramEveryoneAccess ::  PrologProgramId -> EntityField PrologProgramEveryoneSecurity Bool
                                -> SqlPersistT Handler [Entity PrologProgram]
selectProgramEveryoneAccess pid field =
  select $
    from $ \(everyoneFlag `InnerJoin` prog) -> do
      on (everyoneFlag^.PrologProgramEveryoneSecurityPrologProgramId ==. prog^.PrologProgramId)
      where_( prog^.PrologProgramId ==. val pid
              &&. everyoneFlag^. field ==. val True
            )
      return prog



selectProgramGroupAccess ::  UserId -> PrologProgramId -> EntityField PrologProgramGroupSecurity Bool
                             -> SqlPersistT Handler [Entity PrologProgram]
selectProgramGroupAccess uid pid field =
  select $
        from $ \(prog `InnerJoin` flag `InnerJoin` grp) -> do
          on (grp^.GroupMemberGroupId ==. flag^.PrologProgramGroupSecurityGroupId)
          on (flag^.PrologProgramGroupSecurityPrologProgramId ==. prog^.PrologProgramId)
          where_ ( grp^.GroupMemberMember ==. val uid
                   &&. prog^.PrologProgramId ==. val pid
                   &&. flag^. field ==. val True )

          return prog


----------------------  Goal Access templates ------------------------

selectGoalOwnerAccess :: UserId -> PrologGoalId -> EntityField PrologGoalOwnerSecurity Bool
                            -> SqlPersistT Handler [Entity PrologGoal]
selectGoalOwnerAccess uid pid field =
  select $
    from $ \(ownerFlag `InnerJoin` prog) -> do
      on (ownerFlag^.PrologGoalOwnerSecurityPrologGoalId ==. prog^.PrologGoalId)
      where_( prog^.PrologGoalUserId ==. val uid
              &&. prog^.PrologGoalId ==. val pid
              &&. ownerFlag^. field ==. val True
          )
      return prog

selectGoalEveryoneAccess ::  PrologGoalId -> EntityField PrologGoalEveryoneSecurity Bool
                                -> SqlPersistT Handler [Entity PrologGoal]
selectGoalEveryoneAccess pid field =
  select $
    from $ \(everyoneFlag `InnerJoin` prog) -> do
      on (everyoneFlag^.PrologGoalEveryoneSecurityPrologGoalId ==. prog^.PrologGoalId)
      where_( prog^.PrologGoalId ==. val pid
              &&. everyoneFlag^. field ==. val True
            )
      return prog



selectGoalGroupAccess ::  UserId -> PrologGoalId -> EntityField PrologGoalGroupSecurity Bool
                             -> SqlPersistT Handler [Entity PrologGoal]
selectGoalGroupAccess uid pid field =
  select $
        from $ \(prog `InnerJoin` flag `InnerJoin` grp) -> do
          on (grp^.GroupMemberGroupId ==. flag^.PrologGoalGroupSecurityGroupId)
          on (flag^.PrologGoalGroupSecurityPrologGoalId ==. prog^.PrologGoalId)
          where_ ( grp^.GroupMemberMember ==. val uid
                   &&. prog^.PrologGoalId ==. val pid
                   &&. flag^. field ==. val True )
          return prog



------------------------ Permission API for programs  ------------------------
selectProgramOwnerReadable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramOwnerReadable uid pid = selectProgramOwnerAccess uid pid  PrologProgramOwnerSecurityR

selectProgramOwnerWritable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramOwnerWritable uid pid = selectProgramOwnerAccess uid pid  PrologProgramOwnerSecurityW

selectProgramOwnerExecutable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramOwnerExecutable uid pid = selectProgramOwnerAccess uid pid  PrologProgramOwnerSecurityX

selectProgramGroupReadable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramGroupReadable uid pid = selectProgramGroupAccess uid pid  PrologProgramGroupSecurityR

selectProgramGroupWritable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramGroupWritable uid pid = selectProgramGroupAccess uid pid  PrologProgramGroupSecurityW

selectProgramGroupExecutable :: UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramGroupExecutable uid pid = selectProgramGroupAccess uid pid  PrologProgramGroupSecurityX

selectProgramEveryoneReadable :: PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramEveryoneReadable  pid = selectProgramEveryoneAccess pid PrologProgramEveryoneSecurityR

selectProgramEveryoneWritable ::  PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramEveryoneWritable  pid = selectProgramEveryoneAccess pid PrologProgramEveryoneSecurityW

selectProgramEveryoneExecutable :: PrologProgramId -> SqlPersistT Handler [Entity PrologProgram]
selectProgramEveryoneExecutable  pid = selectProgramEveryoneAccess pid PrologProgramEveryoneSecurityX


programReadable :: UserId -> PrologProgramId -> SqlPersistT Handler Bool
programReadable uid pid = negateMaybe' $ runMaybeT $ do
  _<- MaybeT $ negateMaybe $ selectProgramOwnerReadable uid pid
  _<- MaybeT $ negateMaybe $ selectProgramEveryoneReadable pid
  do  MaybeT $ negateMaybe $ selectProgramGroupReadable uid pid

programWritable :: UserId -> PrologProgramId -> SqlPersistT Handler Bool
programWritable uid pid = negateMaybe' $ runMaybeT $ do
  _<- MaybeT $ negateMaybe $ selectProgramOwnerWritable uid pid
  _<- MaybeT $ negateMaybe $ selectProgramEveryoneWritable pid
  do  MaybeT $ negateMaybe $ selectProgramGroupWritable uid pid

programExecutable :: UserId -> PrologProgramId -> SqlPersistT Handler Bool
programExecutable uid pid = negateMaybe' $ runMaybeT $ do
  _<- MaybeT $ negateMaybe $ selectProgramOwnerExecutable uid pid
  _<- MaybeT $ negateMaybe $ selectProgramEveryoneExecutable pid
  do  MaybeT $ negateMaybe $ selectProgramGroupExecutable uid pid


------------------------ Permission Api for Goals  ------------------------

selectGoalOwnerReadable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalOwnerReadable uid pid = selectGoalOwnerAccess uid pid  PrologGoalOwnerSecurityR

selectGoalOwnerWritable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalOwnerWritable uid pid = selectGoalOwnerAccess uid pid  PrologGoalOwnerSecurityW

selectGoalOwnerExecutable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalOwnerExecutable uid pid = selectGoalOwnerAccess uid pid  PrologGoalOwnerSecurityX

selectGoalGroupReadable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalGroupReadable uid pid = selectGoalGroupAccess uid pid  PrologGoalGroupSecurityR

selectGoalGroupWritable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalGroupWritable uid pid = selectGoalGroupAccess uid pid  PrologGoalGroupSecurityW

selectGoalGroupExecutable :: UserId -> PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalGroupExecutable uid pid = selectGoalGroupAccess uid pid  PrologGoalGroupSecurityX

selectGoalEveryoneReadable ::  PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalEveryoneReadable  pid = selectGoalEveryoneAccess pid PrologGoalEveryoneSecurityR

selectGoalEveryoneWritable :: PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalEveryoneWritable  pid = selectGoalEveryoneAccess pid PrologGoalEveryoneSecurityW

selectGoalEveryoneExecutable :: PrologGoalId -> SqlPersistT Handler [Entity PrologGoal]
selectGoalEveryoneExecutable  pid = selectGoalEveryoneAccess pid PrologGoalEveryoneSecurityX


goalReadable :: UserId -> PrologGoalId -> SqlPersistT Handler Bool
goalReadable uid pid = negateMaybe' $ runMaybeT $ do
  _ <- MaybeT $ negateMaybe $ selectGoalOwnerReadable uid pid
  _ <- MaybeT $ negateMaybe $ selectGoalEveryoneReadable pid
  do   MaybeT $ negateMaybe $ selectGoalGroupReadable uid pid

goalWritable :: UserId -> PrologGoalId -> SqlPersistT Handler Bool
goalWritable uid pid = negateMaybe' $ runMaybeT $ do
  _ <- MaybeT $ negateMaybe $ selectGoalOwnerWritable uid pid
  _ <- MaybeT $ negateMaybe $ selectGoalEveryoneWritable pid
  do   MaybeT $ negateMaybe $ selectGoalGroupWritable uid pid

goalExecutable :: UserId -> PrologGoalId -> SqlPersistT Handler Bool
goalExecutable uid pid = negateMaybe' $ runMaybeT $ do
  _ <- MaybeT $ negateMaybe $ selectGoalOwnerExecutable uid pid
  _ <- MaybeT $ negateMaybe $ selectGoalEveryoneExecutable pid
  do   MaybeT $ negateMaybe $ selectGoalGroupExecutable uid pid




--------------------  lsHeadPrograms  --------------------

lsHeadPrograms :: Int64 -> Int64 -> UserId ->  SqlPersistT Handler [Entity PrologProgram]
lsHeadPrograms lim offs uid  =
      select $
      from $ \(program
               `FullOuterJoin` ownerFlags
               `FullOuterJoin` everyoneFlags
               `FullOuterJoin` groupFlags
               `FullOuterJoin` userGroups )  -> do
               on (groupFlags^.PrologProgramGroupSecurityGroupId ==. userGroups^.GroupMemberGroupId)
               on (program^.PrologProgramId ==. groupFlags^.PrologProgramGroupSecurityPrologProgramId)
               on (program^.PrologProgramId ==. everyoneFlags^.PrologProgramEveryoneSecurityPrologProgramId)
               on (program^.PrologProgramId ==. ownerFlags^.PrologProgramOwnerSecurityPrologProgramId)

               where_ ( programOwnerReadableCond uid program ownerFlags
                        ||. programGroupReadableCond uid groupFlags userGroups
                        ||. programEveryoneReadableCond everyoneFlags )
               limit lim
               offset offs
               return program


programOwnerReadableCond :: UserId
                            -> SqlExpr (Entity PrologProgram)
                            -> SqlExpr (Entity PrologProgramOwnerSecurity)
                            -> SqlExpr (Value Bool)
programOwnerReadableCond uid prog ownerFlags = prog^.PrologProgramUserId ==. val uid
                                            &&. ownerFlags^.PrologProgramOwnerSecurityR ==. val True

programGroupReadableCond :: UserId
                            -> SqlExpr (Entity PrologProgramGroupSecurity)
                            -> SqlExpr (Entity GroupMember)
                            -> SqlExpr (Value Bool)
programGroupReadableCond uid groupFlags userGroups = userGroups^.GroupMemberMember ==. val uid
                                                     &&. groupFlags^.PrologProgramGroupSecurityR ==. val True

programEveryoneReadableCond :: SqlExpr (Entity PrologProgramEveryoneSecurity) -> SqlExpr (Value Bool)
programEveryoneReadableCond everyOneFlags = everyOneFlags^.PrologProgramEveryoneSecurityR ==. val True


-------------------------------- lsHeadGoals ---------------------------------

lsHeadGoals :: Int64 -> Int64 -> UserId -> PrologProgramId -> SqlPersistT Handler [Entity PrologGoal]
lsHeadGoals lim offs uid pid = do
  x <- programExecutable uid pid
  if x then callSelect else throwM (GoalAccessError "Program goal not listable")

  where
    callSelect =
      select $
      from $ \(goal
               `FullOuterJoin` ownerFlags
               `FullOuterJoin` everyoneFlags
               `FullOuterJoin` groupFlags
               `FullOuterJoin` userGroups )  -> do
               on (groupFlags^.PrologGoalGroupSecurityGroupId ==. userGroups^.GroupMemberGroupId)
               on (goal^.PrologGoalId ==. groupFlags^.PrologGoalGroupSecurityPrologGoalId)
               on (goal^.PrologGoalId ==. everyoneFlags^.PrologGoalEveryoneSecurityPrologGoalId)
               on (goal^.PrologGoalId ==. ownerFlags^.PrologGoalOwnerSecurityPrologGoalId)

               where_ ( goalOwnerReadableCond uid goal ownerFlags
                        ||. goalGroupReadableCond uid groupFlags userGroups
                        ||. goalEveryoneReadableCond everyoneFlags )
               limit lim
               offset offs
               return goal

goalOwnerReadableCond :: UserId -> SqlExpr (Entity PrologGoal) -> SqlExpr (Entity PrologGoalOwnerSecurity)
                         -> SqlExpr (Value Bool)
goalOwnerReadableCond uid goal ownerFlags = goal^.PrologGoalUserId ==. val uid
                                            &&. ownerFlags^.PrologGoalOwnerSecurityR ==. val True

goalGroupReadableCond :: UserId -> SqlExpr (Entity PrologGoalGroupSecurity) -> SqlExpr (Entity GroupMember)
                         -> SqlExpr (Value Bool)
goalGroupReadableCond uid groupFlags userGroups = userGroups^.GroupMemberMember ==. val uid
                                                  &&. groupFlags^.PrologGoalGroupSecurityR ==. val True

goalEveryoneReadableCond :: SqlExpr (Entity PrologGoalEveryoneSecurity)
                            -> SqlExpr (Value Bool)
goalEveryoneReadableCond everyOneFlags = everyOneFlags^.PrologGoalEveryoneSecurityR ==. val True


---------------------------- FindPrograms ----------------------------

findPrograms :: UserId
          ->  (SqlExpr (Entity PrologProgram) -> SqlExpr (Value Bool))
          ->  SqlQuery ()
          ->  SqlPersistT Handler [Entity PrologProgram]
findPrograms  uid programFilter query =
  select $
  from $ \((program
            `FullOuterJoin` programOwnerFlags
            `FullOuterJoin` programEveryoneFlags
            `FullOuterJoin` programGroupFlags
            `FullOuterJoin` programUserGroups)
          )  -> do
           on (programUserGroups^.GroupMemberGroupId ==. programGroupFlags^.PrologProgramGroupSecurityGroupId)
           on (program^.PrologProgramId ==. programGroupFlags^.PrologProgramGroupSecurityPrologProgramId)
           on (program^.PrologProgramId ==. programEveryoneFlags^.PrologProgramEveryoneSecurityPrologProgramId)
           on (program^.PrologProgramId ==. programOwnerFlags^.PrologProgramOwnerSecurityPrologProgramId)

           where_ (( programOwnerReadableCond uid program programOwnerFlags
                     ||. programGroupReadableCond uid programGroupFlags programUserGroups
                     ||. programEveryoneReadableCond programEveryoneFlags )
                   &&. programFilter program )
           query
           return (program)

---------------------------- FindGoals ----------------------------


findGoals :: UserId
          ->  (SqlExpr (Entity PrologProgram) -> SqlExpr (Value Bool))
          ->  (SqlExpr (Entity PrologGoal)    -> SqlExpr (Value Bool))
          ->  SqlQuery ()
          ->  SqlPersistT Handler [(Entity PrologProgram, Entity PrologGoal)]
findGoals  uid programFilter goalFilter query =
  select $
  from $ \((program
            `FullOuterJoin` programOwnerFlags
            `FullOuterJoin` programEveryoneFlags
            `FullOuterJoin` programGroupFlags
            `FullOuterJoin` programUserGroups)
           `InnerJoin`
           (goal
            `FullOuterJoin` goalOwnerFlags
            `FullOuterJoin` goalEveryoneFlags
            `FullOuterJoin` goalGroupFlags
            `FullOuterJoin` goalUserGroups
           ))  -> do
           on (goalUserGroups^.GroupMemberGroupId ==. goalGroupFlags^.PrologGoalGroupSecurityGroupId)
           on (goal^.PrologGoalId ==. goalGroupFlags^.PrologGoalGroupSecurityPrologGoalId)
           on (goal^.PrologGoalId ==. goalEveryoneFlags^.PrologGoalEveryoneSecurityPrologGoalId)
           on (goal^.PrologGoalId ==. goalOwnerFlags^.PrologGoalOwnerSecurityPrologGoalId)

           on (goal^.PrologGoalPrologProgramId ==. program^.PrologProgramId)

           on (programUserGroups^.GroupMemberGroupId ==. programGroupFlags^.PrologProgramGroupSecurityGroupId)
           on (program^.PrologProgramId ==. programGroupFlags^.PrologProgramGroupSecurityPrologProgramId)
           on (program^.PrologProgramId ==. programEveryoneFlags^.PrologProgramEveryoneSecurityPrologProgramId)
           on (program^.PrologProgramId ==. programOwnerFlags^.PrologProgramOwnerSecurityPrologProgramId)


           where_ (( goalOwnerReadableCond uid goal goalOwnerFlags
                     ||. goalGroupReadableCond uid goalGroupFlags goalUserGroups
                     ||. goalEveryoneReadableCond goalEveryoneFlags )
                   &&.
                   ( programOwnerReadableCond uid program programOwnerFlags
                     ||. programGroupReadableCond uid programGroupFlags programUserGroups
                     ||. programEveryoneReadableCond programEveryoneFlags )
                   &&. programFilter program &&. goalFilter goal )
           query
           return (program,goal)



---------------------------- ReadProgram  ----------------------------

readProgram :: UserId -> PrologProgramId -> SqlPersistT Handler (Maybe (Entity PrologProgram))
readProgram uid pid = do
  readable <- programReadable uid pid
  if readable
    then do ps <- select $
                  from $ \program -> do
                    where_ (program^.PrologProgramId ==. val pid)
                    return program
            case ps of
              [p] -> return (Just p)
              _   -> throwM $ FileSystemError "impossible error"
    else return Nothing

---------------------------- ReadGoal  ----------------------------

readGoal :: UserId -> PrologGoalId -> SqlPersistT Handler (Maybe (Entity PrologGoal))
readGoal uid gid = do
  readable <- goalReadable uid gid
  if readable
    then do gs <- select $
                  from $ \goal -> do
                    where_ (goal^.PrologGoalId ==. val gid)
                    return goal
            case gs of
              [g] -> return (Just g)
              _   -> throwM $ FileSystemError "impossible error"
    else return Nothing

----------------------------  WriteProgram ------------------------------

writeProgram :: UserId -> Entity PrologProgram -> SqlPersistT Handler (Maybe (Entity PrologProgram))
writeProgram uid prog@(Entity pid (PrologProgram uid' name expl code)) = do
  writable <- programWritable uid pid
  if writable
    then do update $ \p -> do
              set p [ PrologProgramUserId      =. val uid'
                    , PrologProgramName        =. val name
                    , PrologProgramExplanation =. val expl
                    , PrologProgramCode        =. val code
                    ]
              where_ (p^.PrologProgramId ==. val pid)
            return (Just prog)
    else
    return Nothing

----------------------------  WriteGoal ------------------------------

writeGoal :: UserId -> Entity PrologGoal -> SqlPersistT Handler (Maybe (Entity PrologGoal))
writeGoal uid goal@(Entity gid (PrologGoal uid' pid name expl code)) = do
  writable <- goalWritable uid gid
  if writable
    then do update $ \p -> do
              set p [ PrologGoalUserId      =. val uid'
                    , PrologGoalPrologProgramId   =. val pid
                    , PrologGoalName        =. val name
                    , PrologGoalExplanation =. val expl
                    , PrologGoalCode        =. val code
                    ]
              where_ (p^.PrologGoalId ==. val gid)
            return (Just goal)
    else
    return Nothing


------------------------------  Create  ------------------------------
createProgram ::  UserId -> Text -> Text -> Text
                  -> SqlPersistT Handler (Maybe PrologProgramId)
createProgram  uid name expl code = do
  exists <- userExists uid
  if not exists
    then return Nothing
    else do
    umask <- userUmask uid
    pid <- insert $ PrologProgram uid name expl code
           (not (umaskOwnerR umask))
           (not (umaskOwnerW umask))
           (not (umaskOwnerX umask))
           (not (umaskEveryoneR umask))
           (not (umaskEveryoneW umask))
           (not (umaskEveryoneX umask))
    return $ Just pid

createGoal ::  UserId -> PrologProgramId -> Text -> Text -> Text
           -> SqlPersistT Handler (Maybe PrologProgramId)
createGoal  uid pid name expl code = do
  exists <- userExists uid
  x      <- programExecutable uid pid

  if not exists || not x
    then return Nothing
    else do
      umask <- userUmask uid
      pid <- insert $ PrologGoal uid pid name expl code
             (not (umaskOwnerR umask))
             (not (umaskOwnerW umask))
             (not (umaskOwnerX umask))
             (not (umaskEveryoneR umask))
             (not (umaskEveryoneW umask))
             (not (umaskEveryoneX umask))
      return $ Just pid


---------------------------- rmdir/unlink ----------------------------
rmdirProgram :: UserId -> PrologProgramId -> SqlPersistT Handler (Maybe PrologProgramId)
rmdirProgram uid pid = do
  writable <- programWritable uid pid
  goals    <- lsGoals uid pid
  if writable && null goals
    then rm pid
    else return Nothing

  where
    rm pid = do delete $
                  from $ \p -> do
                    where_ (p^.ProgramGroupsPrologProgramId ==. val pid)
                delete $
                  from $ \p -> do
                    where_ (p^.PrologProgramsTagsPrologProgramId ==. val pid)
                delete $
                  from $ \p -> do
                    where_ (p^.PrologProgramId ==. val pid)

unlinkGoal :: UserId -> PrologGoalId -> SqlPersistT Handler (Maybe PrologGoalId)
unlinkGoal uid pid = do
  writable <- programWritable uid pid
  if writable
    then unlink' pid
    else return  Nothing

  where
    unlink' pid = do delete $
                       from $ \p -> do
                         where_ (p^.GoalGroupsPrologGoalId ==. val pid)

                     delete $
                       from $ \p -> do
                         where_ (p^.PrologGoalsTagsPrologGoalId ==. val pid)

                     delete $
                       from $ \p -> do
                         where_ (p^.PrologGoalId ==. val pid)



------------------------------  chown --------------------------------

chownProgram :: UserId -> Maybe UserId -> [GroupId] -> PrologProgramId
                -> SqlPersistT Handler (Maybe PrologProgramId)
chownProgram uid muid gids pid = do
  prv <- privileged uid
  own <- isOwner uid pid
  umask <- userUmask uid

  case (prv, own , muid ) of
    ( True, _ , Just uid') -> do changeOwner uid' pid
                                      changeGroup gids pid umask

    ( True, _ , Nothing )  -> do changeGroup gids pid umask

    ( _, True, Just uid')  -> do return Nothing

    ( _, True, Nothing  )  -> do changeGroup gids pid umask

    ( _, _ , _ )  -> return Nothing

  where
    changeOwner uid pid = do update $ \p -> do
                              set p [ PrologProgramUserId =. val uid
                                    ]
                              where_ (p^.PrologProgramId ==. val pid)
                            return (Just pid)

    changeGroup  gids pid umask = do delete $ \p -> do
                                       where_ (p^.ProgramGroupsPrologProgramId ==. pid)
                                     forM_ gids (\gid -> insert (ProgramGroups pid gid
                                                                 (not (umaskGroupR umask))
                                                                 (not (umaskGroupW umask))
                                                                 (not (umaskGroupX umask))))
                                     return (Just pid)


chownGoal :: UserId -> Maybe UserId -> [GroupId] -> PrologGoalId
             -> SqlPersistT Handler (Maybe PrologGoalId)
chownGoal uid muid gids goalid = do
  prv <- privileged uid
  own <- isOwner uid goalid
  umask <- userUmask uid

  case (writable, prv, own , muid ) of
    (True, _ , Just uid') -> do changeOwner uid' pid
                                      changeGroup gids pid umask

    ( True, _ , Nothing )  -> do changeGroup gids pid umask

    ( _, True, Just uid')  -> do return Nothing

    ( _, True, Nothing  )  -> do changeGroup gids pid umask

    (_, _ , _ )  -> return Nothing

  where
    changeUser uid goalid = do update $ \p -> do
                              set p [ PrologGoalUserId =. val uid
                                    ]
                              where_ (p^.PrologGoalId ==. val goalid)
                            return (Just goalid)

    changeGroup  gids goalid umask = do delete $ \p -> do
                                          where_ (p^.GoalGroupsPrologGoalId ==. goalid)
                                        forM_ gids (\gid -> insert (GoalGroups goalid gid
                                                                    (not (umaskGroupR umask))
                                                                    (not (umaskGroupW umask))
                                                                    (not (umaskGroupX umask))))
                                        return (Just goalid)


------------------------------  chmod --------------------------------
chmodProgram :: UserId ->  Maybe Perm -> [(GroupId,Perm)] -> Maybe Perm -> PrologProgramId
                -> SqlPersistT Handler (Maybe PrologProgramId)
chmodProgram uid muperm gperms maperm pid = do
  own      <- isOwner uid pid
  prv      <- privileged uid

  if (prv || own)
    then
    do chmodU uid umperm pid
       chmodG uid gids  pid
       chmodA uid amperm pid
    else
    return Nothing

    where
      chmodU uid (Just (Perm r w x)) pid = do update $ \p -> do
                                          set p [ PrologProgramOwnerR =. val r
                                                , PrologProgramOwnerW =. val w
                                                , PrologProgramOwnerX =. val x
                                                ]
                                          where_ (p^.PrologProgramId ==. val pid)
      chmodU uid Nothing pid = return Nothing

      chmodA uid (Just (Perm r w x)) pid = do update $ \p -> do
                                          set p [ PrologProgramEveryoneR =. val r
                                                , PrologProgramEveryoneW =. val w
                                                , PrologProgramEveryoneX =. val x
                                                ]
                                          where_ (p^.PrologProgramId ==. val pid)
      chmodA uid Nothing pid = return Nothing

      chmodG uid gids pid = mapM_ chmodG'each gids
        where
          chmodG'each (gid, Perm r w x) = do update $ \p -> do
                                               set p [ ProgramGroupsGroupR =. val r
                                                     , ProgramGroupsGroupW =. val w
                                                     , ProgramGroupsGroupX =. val x
                                                     ]
                                               where_ (p^.ProgramGroupsPrologProgramId ==. val pid
                                                       &&. p^.ProgramGroupsGroupId ==. gid)



chmodProgram :: UserId ->  Maybe Perm -> [(GroupId,Perm)] -> Maybe Perm -> PrologGoalId
                -> SqlPersistT Handler (Maybe PrologGoalId)
chmodProgram uid muperm gperms maperm pid = do
  own      <- isOwner uid pid
  prv      <- privileged uid

  if (prv || own)
    then
    do chmodU uid umperm pid
       chmodG uid gids  pid
       chmodA uid amperm pid
    else
    return Nothing

    where
      chmodU uid (Just (Perm r w x)) pid = do update $ \p -> do
                                          set p [ PrologGoalOwnerR =. val r
                                                , PrologGoalOwnerW =. val w
                                                , PrologGoalOwnerX =. val x
                                                ]
                                          where_ (p^.PrologGoalId ==. val pid)
      chmodU uid Nothing pid = return Nothing

      chmodA uid (Just (Perm r w x)) pid = do update $ \p -> do
                                          set p [ PrologGoalEveryoneR =. val r
                                                , PrologGoalEveryoneW =. val w
                                                , PrologGoalEveryoneX =. val x
                                                ]
                                          where_ (p^.PrologGoalId ==. val pid)
      chmodA uid Nothing pid = return Nothing

      chmodG uid gids pid = mapM_ chmodG'each gids
        where
          chmodG'each (gid, Perm r w x) = do update $ \p -> do
                                               set p [ GoalGroupsGroupR =. val r
                                                     , GoalGroupsGroupW =. val w
                                                     , GaolGroupsGroupX =. val x
                                                     ]
                                               where_ (p^.GoalGroupsPrologGoalId ==. val pid
                                                       &&. p^.GoalGroupsGroupId ==. gid)



------------------------------ User ------------------------------

useradd :: Text -> Maybe Text -> UMask -> Result UserId
useradd ident mpassword umask = do uid <- insert (User ident mpassword umask)
                                   return $ Right uid

usermod :: UserId ->  GroupId -> Result UserId
usermod uid gid = do
  prv <- privileged uid
  own <- groupOwner uid gid

  if (prv || own)
    then
    insert (GroupMembers gid uid)
    return $ Right gid
    else
    return $ Left $ PermissionError "not root or the group owner"

usermod' :: UserId -> GroupId -> Result GroupId
usermod' uid gid = do
  prv <- privileged uid
  own <- groupOwner uid gid

  if (prv || own)
    then
    deleteBy (UniqueGroupMember gid uid)
    return $ Right gid
    else
    return $ Left $ PermissionError "not root or the group owner"

-- Doesn't delete the programs by default
userdel :: UserId -> Bool
userdel uid = do
  delete $ \p -> do
    where_ (p^.GroupMembersMember ==. uid)
  return True

------------------------------  Group --------------------------------
groupadd :: UserId -> Text -> Maybe Text -> SqlPersistT Handler (Result GroupId)
groupadd uid groupName explanation = do
  gid <- insert (Group groupName uid explanation)
  return $ Right gid

groupmod :: UserId -> GroupId -> Text -> Maybe Text -> SqlPersistT Handler (Result GroupId)
groupmod uid gid groupName explanation = do
  prv   <- privileged uid
  owner <- groupOwner gid
  if (prv || owner == uid)
    then do update $ \p -> do
            set [ p^.GroupGroup =. groupName
                , p^.GroupExplanation =. explanation
                ]
            return $ Right gid
    else
    return $ Left $ PermissionError "only root or the owner can modify the group"

groupdel :: UserId -> GroupId -> SqlPersistT Handler (Result Bool)
groupdel uid gid = do
  prv   <- privileged uid
  owner <- groupOwner gid
  if (prv || owner == uid)
    then do deleteBy (UniqueGroupMember gid uid)
            return $ Right gid
    else
    return $ Left $ PermissionError "only root or the owner can modify the group"


------------------------  Database Invariants  ------------------------

valid :: SqlPersistT Handler Bool
valid = allM [ mapM (\g -> validPid (prologGoalPrologProgramId g)) =<< allGoals
             ,  mapM (\g -> validGid (groupMembersGroupId g)) =<< allGroupMember
             ,  mapM (\g -> validUid (groupMembersMember g)) =<< allGroupMember
             ,  mapM (\g -> validPid (programGroupsPrologProgramId g)) =<< allProgramGroups
             ,  mapM (\g -> validGid (programGroupsGroupId g)) =<<  allProgramGroups
             ,  mapM (\g -> validGoalId (goalGroupsPrologGoalId g)) =<< allGoalGroups
             ,  mapM (\g -> validGid (goalGroupsGroupId g)) =<< allGoalGroups
             ,  mapM (\g -> validPid (prologProgramTagsPrologProgramId g)) =<< allPrologProgramsTags
             ,  mapM (\g -> validTagId (prologProgramTagsTagId g)) =<< allPrologProgramsTags
             ,  mapM (\g -> validGoalid (prologGoalTagsPrologGoalId g)) =<< allPrologGoalTags
             ,  mapM (\g -> validTagId (prologGoalTagsTagId g)) =<< allPrologGoalTags
             ]
------------------------ Auxillary functions  ------------------------
negateMaybe :: Monad m => m [a] -> m (Maybe Bool)
negateMaybe m  = do x <- m
                    case x of
                      []  -> return (Just True)
                      _   -> return Nothing

negateMaybe' :: Monad m => m (Maybe Bool) -> m Bool
negateMaybe' m = do x <- m
                    case x of
                      Just _ -> return False
                      Nothing -> return True
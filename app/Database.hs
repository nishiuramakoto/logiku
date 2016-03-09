module Database  where

import             Import
import             Data.Time.LocalTime
import qualified   Data.Text as T
import Control.Monad.Trans.Maybe

type Name         = Text
type Explanation  = Text
type Code         = Text
type TagText      = Text

runDBtime :: YesodDB App a -> Handler a
runDBtime action = do
  ZonedTime localTime _zone  <- liftIO getZonedTime
  $(logInfo) $ T.pack $ "sql begin:" ++ show localTime
  x <- runDB action
  ZonedTime localTime' _zone  <- liftIO getZonedTime
  $(logInfo) $ T.pack $ "sql end:" ++ show localTime'
  return x

entityToId :: Entity t -> Key t
entityToId (Entity id' _) = id'

deleteEntity :: forall t (m :: * -> *).
                (MonadIO m, PersistStore (PersistEntityBackend t)) =>
                Entity t -> ReaderT (PersistEntityBackend t) m ()
deleteEntity (Entity id' _) = delete id'



userExists :: UserAccountId -> Handler Bool
userExists uid = do user <- runDB $ get uid
                    case user of
                      Just _ -> return True
                      Nothing -> return False

createProgram ::  UserAccountId -> Text -> Text -> Text
                  -> Handler (Maybe PrologProgramId)
createProgram  uid name expl code = runDB $ do
  muser <- get uid
  case muser of
    Just user -> do
      pid <-  insert $ PrologProgram uid name expl code
              (not (userAccountUmaskOwnerR user))
              (not (userAccountUmaskOwnerW user))
              (not (userAccountUmaskOwnerX user))
              (not (userAccountUmaskEveryoneR user))
              (not (userAccountUmaskEveryoneW user))
              (not (userAccountUmaskEveryoneX user))
      return $ Just pid
    Nothing -> return Nothing

createGoal ::  UserAccountId -> PrologProgramId -> Text -> Text -> Text
           -> Handler (Maybe PrologGoalId)
createGoal  uid pid name expl code = runDB $ do
  muser <- get uid
  case muser of
    Just user -> do
      goalid <- insert $ PrologGoal uid pid name expl code
                (not (userAccountUmaskOwnerR user))
                (not (userAccountUmaskOwnerW user))
                (not (userAccountUmaskOwnerX user))
                (not (userAccountUmaskEveryoneR user))
                (not (userAccountUmaskEveryoneW user))
                (not (userAccountUmaskEveryoneX user))
      return $ Just goalid
    Nothing -> return Nothing


writeProgram :: UserAccountId -> Text -> Text -> Text
                ->  Handler (Maybe PrologProgramId)
writeProgram uid name expl code = runDB $ do
  mpid <- getBy $ UniquePrologProgram uid name
  case mpid of
    Just (Entity pid _) -> do update pid [ PrologProgramUserId     =.  uid
                                         , PrologProgramName       =.  name
                                         , PrologProgramExplanation =.  expl
                                         , PrologProgramCode        =.  code
                                         ]
                              return $ Just pid

    Nothing -> return Nothing


----------------------------  Insert or Nothing ------------------------------

insertUser :: UserAccount -> Handler (Maybe UserAccountId)
insertUser user = do
  muser <- runDBtime $ getBy $ UniqueUserAccount (userAccountIdent user)
  case muser of
    Just _user -> return Nothing
    Nothing   -> Just <$> (runDBtime $ insert $ makeUser (userAccountIdent user) (userAccountPassword user))

insertPrologProgram :: PrologProgram -> Handler (Maybe PrologProgramId)
insertPrologProgram program = do
  let name' = T.strip (prologProgramName program)
  mProgram <- runDBtime $ getBy $ UniquePrologProgram (prologProgramUserId program) name'
  case mProgram of
    Just _program -> return Nothing
    Nothing      -> do
      createProgram
        (prologProgramUserId program)
        name'
        (prologProgramExplanation program)
        (prologProgramCode program)


insertPrologGoal :: PrologGoal -> Handler (Maybe PrologGoalId)
insertPrologGoal goal = do
  let name' = T.strip (prologGoalName goal)
  mProgram <- runDBtime $ getBy $ UniquePrologGoal
              (prologGoalUserId goal)
              (prologGoalPrologProgramId goal)
              name'
  case mProgram of
    Just _program -> return Nothing
    Nothing      ->
      createGoal
      (prologGoalUserId goal)
      (prologGoalPrologProgramId goal)
      name'
      (prologGoalExplanation goal)
      (prologGoalCode goal)


insertTag :: Tag -> Handler (Maybe TagId)
insertTag (Tag tag) = do
  mTag <- runDBtime $ getBy $ UniqueTag tag
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ Tag tag)

insertPrologProgramTag :: PrologProgramTags -> Handler (Maybe PrologProgramTagsId)
insertPrologProgramTag (PrologProgramTags progId tagId) = do
  mTag <- runDBtime $ getBy $ UniquePrologProgramTags progId tagId
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ PrologProgramTags progId tagId)

insertPrologGoalTag :: PrologGoalTags -> Handler (Maybe PrologGoalTagsId)
insertPrologGoalTag (PrologGoalTags goalId tagId) = do
  mTag <- runDBtime $ getBy $ UniquePrologGoalTags goalId tagId
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ PrologGoalTags goalId tagId)

------------------------  Insert or Replace --------------------------

upsertPrologProgram :: PrologProgram -> Handler (Entity PrologProgram)
upsertPrologProgram pp = runDBtime $ upsert pp []

upsertPrologGoal :: PrologGoal -> Handler (Entity PrologGoal)
upsertPrologGoal pg =  runDBtime $ upsert pg []

----------------------------  Selecting for All Users ------------------------------

countPrograms :: Handler Int
countPrograms =  runDBtime $ count ([] :: [Filter PrologProgram])


countUserPrograms :: UserAccountId -> Handler Int
countUserPrograms uid = runDBtime $ count [PrologProgramUserId ==. uid]

selectPrograms :: Int -> Int -> Handler [Entity PrologProgram]
selectPrograms n m
  | n < m     = runDBtime $ selectList [] [OffsetBy n, LimitTo (m-n) ]
  | otherwise = runDBtime $ selectList [] []

selectUserPrograms :: UserAccountId -> Int -> Int -> Handler [Entity PrologProgram]
selectUserPrograms uid n m
  | n < m     = runDBtime $ selectList [PrologProgramUserId ==. uid ] [OffsetBy n, LimitTo (m-n) ]
  | otherwise = runDBtime $ selectList [] []

selectFirstProgram :: Handler (Maybe (Entity PrologProgram))
selectFirstProgram =  do
  entity <- runDBtime $ selectList [] [Asc PrologProgramId , LimitTo 1 ]
  case entity of
    [ e ]
        ->  return $ Just e
    _   ->  return Nothing

selectFirstUserProgram :: UserAccountId -> Handler (Maybe (Entity PrologProgram))
selectFirstUserProgram uid =  do
  entity <- runDBtime $ selectList [PrologProgramUserId ==. uid ] [Asc PrologProgramId , LimitTo 1 ]
  case entity of
    [ e ]
        ->  return $ Just e
    _   ->  return Nothing

selectLastProgram :: Handler (Maybe (Entity PrologProgram))
selectLastProgram =  do
  entity <- runDBtime $ selectList [] [Desc PrologProgramId , LimitTo 1 ]
  case entity of
    [ e ]
        ->  return $ Just e
    _   ->  return Nothing

selectLastUserProgram :: UserAccountId -> Handler (Maybe (Entity PrologProgram))
selectLastUserProgram uid =  do
  entity <- runDBtime $ selectList [PrologProgramUserId ==. uid] [Desc PrologProgramId , LimitTo 1 ]
  case entity of
    [ e ]
        ->  return $ Just e
    _   ->  return Nothing

selectNextProgram :: Maybe PrologProgramId -> Handler (Maybe (Entity PrologProgram))
selectNextProgram  Nothing  = selectFirstProgram
selectNextProgram (Just progId) =  do
  nextProgram <- runDBtime $ selectList [PrologProgramId >. progId] [LimitTo 1]
  case nextProgram of
    [e]
      -> return  $ Just e
    _ -> selectFirstProgram

selectNextUserProgram :: UserAccountId -> Maybe PrologProgramId -> Handler (Maybe (Entity PrologProgram))
selectNextUserProgram  uid Nothing  = selectFirstUserProgram uid
selectNextUserProgram  uid (Just progId) =  do
  nextProgram <- runDBtime $ selectList [PrologProgramUserId ==. uid, PrologProgramId >. progId ] [LimitTo 1]
  case nextProgram of
    [e]
      -> return  $ Just e
    _ -> selectFirstUserProgram uid



selectPrevProgram :: Maybe PrologProgramId -> Handler (Maybe (Entity PrologProgram))
selectPrevProgram  Nothing  = selectLastProgram
selectPrevProgram (Just progId ) = do
  prevProgram <- runDBtime $ selectList [PrologProgramId <. progId] [LimitTo 1]
  case prevProgram of
    [e]
      -> return $ Just e
    _ -> selectLastProgram

selectPrevUserProgram :: UserAccountId -> Maybe PrologProgramId -> Handler (Maybe (Entity PrologProgram))
selectPrevUserProgram  uid Nothing  = selectLastUserProgram uid
selectPrevUserProgram  uid (Just progId ) = do
  prevProgram <- runDBtime $ selectList [PrologProgramUserId ==. uid, PrologProgramId <. progId ] [LimitTo 1]
  case prevProgram of
    [e]
      -> return $ Just e
    _ -> selectLastProgram

countProgramGoals :: PrologProgramId -> Handler Int
countProgramGoals pid =  runDBtime $ count [ PrologGoalPrologProgramId ==. pid ]

countUserProgramGoals :: UserAccountId -> PrologProgramId -> Handler Int
countUserProgramGoals uid pid =  runDBtime $ count [ PrologGoalUserId ==. uid, PrologGoalPrologProgramId ==. pid ]

selectProgramGoals :: Int -> Int -> PrologProgramId  -> Handler [Entity PrologGoal]
selectProgramGoals n m pid
  | m > n     = runDBtime $ selectList [PrologGoalPrologProgramId ==. pid ] [OffsetBy n , LimitTo (m-n)]
  | otherwise = runDBtime $ selectList [PrologGoalPrologProgramId ==. pid ] []

selectUserProgramGoals :: UserAccountId -> Int -> Int -> PrologProgramId  -> Handler [Entity PrologGoal]
selectUserProgramGoals uid n m pid
  | m > n     = runDBtime $ selectList [PrologGoalUserId ==. uid ,  PrologGoalPrologProgramId ==. pid ]
                                   [OffsetBy n , LimitTo (m-n)]
  | otherwise = runDBtime $ selectList [PrologGoalUserId ==. uid ,  PrologGoalPrologProgramId ==. pid ] []

selectFirstUserIdent :: Handler (Maybe Text)
selectFirstUserIdent = do
  us <- runDB $ selectList [] [ Asc UserAccountId, LimitTo 1 ]
  case us of
    [ Entity _uid user ] ->  return $ Just (userAccountIdent user)
    _ -> return Nothing


-- submitGoal :: PrologProgramId -> PrologGoalForm -> CC (PS Html) Handler (Maybe (Key PrologGoal))
-- submitGoal progId (PrologGoalForm name (Textarea code)) = do
--   -- lift $ $(logInfo) $ "submitting goal:" ++ name
--   maybeGoal <- lift $ runDBtime $ getBy $ PrologGoalUniqueName progId name
--   case maybeGoal of
--     Just _  -> do
--       klabel <- lift $ generateCcLabel
--       html   <- lift $ defaultLayout $(widgetFile "prolog_goal_editor_error_duplicate_goal")
--       inquire klabel html
--       return Nothing
--     Nothing -> do
--       goalId <- lift $ runDBtime $ insert $ PrologGoal name progId  code
--       -- lift $ $(logInfo) $ "submitted goal:" ++ name
--       return $ Just goalId


----------------------------  Deletions ------------------------------

deleteProgram :: PrologProgramId -> Handler ()
deleteProgram pid = do
  goals <- selectProgramGoals 0 0 pid -- select all goals
  mapM_ ( deleteGoal . entityToId )  goals
  ts <- runDBtime $ selectList [ PrologProgramTagsPrologProgramId ==. pid ] []
  runDBtime $ mapM_ deleteEntity ts
  runDBtime $ delete pid


deleteGoal :: PrologGoalId -> Handler ()
deleteGoal gid = do
  runDBtime $ selectList [ PrologGoalTagsPrologGoalId ==. gid ] [] >>=  mapM_ deleteEntity
  runDBtime $ delete gid


------------------------------ Getters  ------------------------------

getUserId :: Text -> Handler (Maybe UserAccountId)
getUserId uident = runMaybeT $ do
  Entity uid _ <- MaybeT $ runDB $ getBy (UniqueUserAccount uident)
  return uid

getUserIdent :: UserAccountId -> Handler (Maybe Text)
getUserIdent uid = do
  muser <- runDB $ get uid
  return $ fmap getUid muser

  where
    getUid user = userAccountIdent user


getProgramId :: UserAccountId -> Text -> Handler (Maybe PrologProgramId)
getProgramId uid name = runMaybeT $ do
  let name' = T.strip name
  Entity pid _ <- MaybeT $ runDB $ getBy (UniquePrologProgram uid name')
  return pid


getGoalId :: UserAccountId -> PrologProgramId -> Text -> Handler (Maybe PrologGoalId)
getGoalId uid pid goalName = runMaybeT $ do
  let goalName' = T.strip goalName
  Entity gid _ <- MaybeT $ runDB $ getBy (UniquePrologGoal uid pid goalName')
  return gid

getGoalCode :: PrologGoalId -> Handler (Maybe Text)
getGoalCode gid = runMaybeT $ prologGoalCode <$> (MaybeT $ runDB $ get gid)

getProgramCode :: PrologProgramId -> Handler (Maybe Text)
getProgramCode pid = runMaybeT $ prologProgramCode <$> (MaybeT $ runDB $ get pid)

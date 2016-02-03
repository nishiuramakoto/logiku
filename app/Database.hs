module Database  where

import             Import
import             Data.Time.LocalTime
import qualified   Data.Text as T
type Name         = Text
type Explanation  = Text
type Code         = Text
type TagText      = Text

runDBtime :: YesodDB App a -> Handler a
runDBtime action = do
  ZonedTime localTime zone  <- liftIO getZonedTime
  $(logInfo) $ T.pack $ "sql begin:" ++ show localTime
  x <- runDB action
  ZonedTime localTime zone  <- liftIO getZonedTime
  $(logInfo) $ T.pack $ "sql end:" ++ show localTime
  return x

entityToId :: Entity t -> Key t
entityToId (Entity id' _) = id'

deleteEntity :: forall t (m :: * -> *).
                (MonadIO m, PersistStore (PersistEntityBackend t)) =>
                Entity t -> ReaderT (PersistEntityBackend t) m ()
deleteEntity (Entity id' _) = delete id'

----------------------------  Insert or Nothing ------------------------------

insertUser :: User -> Handler (Maybe UserId)
insertUser (User ident mpassword)  = do
  muser <- runDBtime $ getBy $ UniqueUser ident
  case muser of
    Just _user -> return Nothing
    Nothing   -> Just <$> (runDBtime $ insert $ User ident mpassword)

insertPrologProgram :: PrologProgram -> Handler (Maybe PrologProgramId)
insertPrologProgram (PrologProgram userId name explanation code) = do
  mProgram <- runDBtime $ getBy $ UniquePrologProgram userId name
  case mProgram of
    Just _program -> return Nothing
    Nothing      -> Just <$> (runDBtime $ insert $ PrologProgram userId name explanation code)

insertPrologGoal :: PrologGoal -> Handler (Maybe PrologGoalId)
insertPrologGoal (PrologGoal userId progId name explanation code) = do
  mProgram <- runDBtime $ getBy $ UniquePrologGoal userId progId name
  case mProgram of
    Just _program -> return Nothing
    Nothing      -> Just <$> (runDBtime $ insert $ PrologGoal userId progId name explanation code)

insertTag :: Tag -> Handler (Maybe TagId)
insertTag (Tag tag) = do
  mTag <- runDBtime $ getBy $ UniqueTag tag
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ Tag tag)

insertPrologProgramTag :: PrologProgramsTags -> Handler (Maybe PrologProgramsTagsId)
insertPrologProgramTag (PrologProgramsTags progId tagId) = do
  mTag <- runDBtime $ getBy $ UniquePrologProgramsTags progId tagId
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ PrologProgramsTags progId tagId)

insertPrologGoalTag :: PrologGoalsTags -> Handler (Maybe PrologGoalsTagsId)
insertPrologGoalTag (PrologGoalsTags goalId tagId) = do
  mTag <- runDBtime $ getBy $ UniquePrologGoalsTags goalId tagId
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ PrologGoalsTags goalId tagId)

------------------------  Insert or Replace --------------------------

upsertPrologProgram :: PrologProgram -> Handler (Entity PrologProgram)
upsertPrologProgram pp = runDBtime $ upsert pp []

upsertPrologGoal :: PrologGoal -> Handler (Entity PrologGoal)
upsertPrologGoal pg =  runDBtime $ upsert pg []

----------------------------  Selecting for All Users ------------------------------

countPrograms :: Handler Int
countPrograms =  runDBtime $ count ([] :: [Filter PrologProgram])


countUserPrograms :: UserId -> Handler Int
countUserPrograms uid = runDBtime $ count [PrologProgramUserId ==. uid]

selectPrograms :: Int -> Int -> Handler [Entity PrologProgram]
selectPrograms n m
  | n < m     = runDBtime $ selectList [] [OffsetBy n, LimitTo (m-n) ]
  | otherwise = runDBtime $ selectList [] []

selectUserPrograms :: UserId -> Int -> Int -> Handler [Entity PrologProgram]
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

selectFirstUserProgram :: UserId -> Handler (Maybe (Entity PrologProgram))
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

selectLastUserProgram :: UserId -> Handler (Maybe (Entity PrologProgram))
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

selectNextUserProgram :: UserId -> Maybe PrologProgramId -> Handler (Maybe (Entity PrologProgram))
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

selectPrevUserProgram :: UserId -> Maybe PrologProgramId -> Handler (Maybe (Entity PrologProgram))
selectPrevUserProgram  uid Nothing  = selectLastUserProgram uid
selectPrevUserProgram  uid (Just progId ) = do
  prevProgram <- runDBtime $ selectList [PrologProgramUserId ==. uid, PrologProgramId <. progId ] [LimitTo 1]
  case prevProgram of
    [e]
      -> return $ Just e
    _ -> selectLastProgram

countProgramGoals :: PrologProgramId -> Handler Int
countProgramGoals pid =  runDBtime $ count [ PrologGoalPrologProgramId ==. pid ]

countUserProgramGoals :: UserId -> PrologProgramId -> Handler Int
countUserProgramGoals uid pid =  runDBtime $ count [ PrologGoalUserId ==. uid, PrologGoalPrologProgramId ==. pid ]

selectProgramGoals :: Int -> Int -> PrologProgramId  -> Handler [Entity PrologGoal]
selectProgramGoals n m pid
  | m > n     = runDBtime $ selectList [PrologGoalPrologProgramId ==. pid ] [OffsetBy n , LimitTo (m-n)]
  | otherwise = runDBtime $ selectList [PrologGoalPrologProgramId ==. pid ] []

selectUserProgramGoals :: UserId -> Int -> Int -> PrologProgramId  -> Handler [Entity PrologGoal]
selectUserProgramGoals uid n m pid
  | m > n     = runDBtime $ selectList [PrologGoalUserId ==. uid ,  PrologGoalPrologProgramId ==. pid ]
                                   [OffsetBy n , LimitTo (m-n)]
  | otherwise = runDBtime $ selectList [PrologGoalUserId ==. uid ,  PrologGoalPrologProgramId ==. pid ] []



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

deleteProgram :: PrologProgramId -> Handler ()
deleteProgram pid = do
  goals <- selectProgramGoals 0 0 pid -- select all goals
  mapM_ ( deleteGoal . entityToId )  goals
  ts <- runDBtime $ selectList [ PrologProgramsTagsPrologProgramId ==. pid ] []
  runDBtime $ mapM_ deleteEntity ts
  runDBtime $ delete pid


deleteGoal :: PrologGoalId -> Handler ()
deleteGoal gid = do
  runDBtime $ selectList [ PrologGoalsTagsPrologGoalId ==. gid ] [] >>=  mapM_ deleteEntity
  runDBtime $ delete gid

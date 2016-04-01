module Database  where

import             Import hiding(writeFile)
import             Data.Time.LocalTime
import qualified   Data.Text as T
import             Control.Monad.Trans.Maybe
import             DBFS
import             Control.Monad.Trans.Either
import             Constructors


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

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left  a) = Nothing
eitherToMaybe (Right b) = Just b


userExists :: UserAccountId -> Handler Bool
userExists uid = do user <- runDB $ get uid
                    case user of
                      Just _ -> return True
                      Nothing -> return False

createProgram :: UserAccountId -> Text -> Text -> Text
                 -> Handler (Maybe DirectoryId)
createProgram  uid name expl code = runDB $ fmap eitherToMaybe $ runEitherT $ do
  Entity key dir <- EitherT $ uid `opendir` name
  EitherT $ uid `writeDirectory` Entity key dir { directoryExplanation = expl
                                                , directoryCode = code }
  return $ key



createGoal ::  UserAccountId -> DirectoryId -> Text -> Text -> Text
           -> Handler (Maybe FileId)
createGoal  uid dir name expl code = runDB $ fmap eitherToMaybe $ runEitherT $ do
  Entity key file <- EitherT $ uid `fopen` dir $ name
  _               <- EitherT $ uid `writeFile` Entity key file { fileExplanation = expl
                                                               , fileCode = code }
  return key

writeProgram :: UserAccountId -> Text -> Text -> Text
                ->  Handler (Maybe DirectoryId)
writeProgram uid name expl code = runDB $ fmap eitherToMaybe $ runEitherT $ do
  Entity key dir  <- EitherT $ uid `opendir` name
  _               <- EitherT $ uid `writeDirectory` Entity key dir { directoryExplanation = expl
                                                                   , directoryCode        = code
                                                                   }
  return key

----------------------------  Insert or Nothing ------------------------------

insertUser :: UserAccount -> Handler (Maybe UserAccountId)
insertUser user = do
  muser <- runDBtime $ getBy $ UniqueUserAccount (userAccountIdent user)
  time  <- liftIO $ getCurrentTime
  case muser of
    Just _user -> return Nothing
    Nothing   -> Just <$> (runDBtime $ insert $ (makeUserAccount (userAccountIdent user) time time time)
                           { userAccountPassword = userAccountPassword user } )

insertDirectory :: Directory -> Handler (Maybe DirectoryId)
insertDirectory program = do
  let name' = T.strip (directoryName program)
  mProgram <- runDBtime $ getBy $ UniqueDirectory (directoryUserId program) name'
  case mProgram of
    Just _program -> return Nothing
    Nothing      -> do
      createProgram
        (directoryUserId program)
        name'
        (directoryExplanation program)
        (directoryCode program)


insertFile :: File -> Handler (Maybe FileId)
insertFile goal = do
  let name' = T.strip (fileName goal)
  mProgram <- runDBtime $ getBy $ UniqueFile
              (fileUserId goal)
              (fileDirectoryId goal)
              name'
  case mProgram of
    Just _program -> return Nothing
    Nothing      ->
      createGoal
      (fileUserId goal)
      (fileDirectoryId goal)
      name'
      (fileExplanation goal)
      (fileCode goal)


insertTag :: Tag -> Handler (Maybe TagId)
insertTag (Tag tag) = do
  mTag <- runDBtime $ getBy $ UniqueTag tag
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ Tag tag)

insertDirectoryTag :: DirectoryTag -> Handler (Maybe DirectoryTagId)
insertDirectoryTag (DirectoryTag progId tagId) = do
  mTag <- runDBtime $ getBy $ UniqueDirectoryTag progId tagId
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ DirectoryTag progId tagId)

insertFileTag :: FileTag -> Handler (Maybe FileTagId)
insertFileTag (FileTag goalId tagId) = do
  mTag <- runDBtime $ getBy $ UniqueFileTag goalId tagId
  case mTag of
    Just _  -> return Nothing
    Nothing -> Just <$> (runDBtime $ insert $ FileTag goalId tagId)

------------------------  Insert or Replace --------------------------

upsertDirectory :: Directory -> Handler (Entity Directory)
upsertDirectory pp = runDBtime $ upsert pp []

upsertFile :: File -> Handler (Entity File)
upsertFile pg =  runDBtime $ upsert pg []

----------------------------  Selecting for All Users ------------------------------

countPrograms :: Handler Int
countPrograms =  runDBtime $ count ([] :: [Filter Directory])


countUserPrograms :: UserAccountId -> Handler Int
countUserPrograms uid = runDBtime $ count [DirectoryUserId ==. uid]

selectPrograms :: Int -> Int -> Handler [Entity Directory]
selectPrograms n m
  | n < m     = runDBtime $ selectList [] [OffsetBy n, LimitTo (m-n) ]
  | otherwise = runDBtime $ selectList [] []

selectUserPrograms :: UserAccountId -> Int -> Int -> Handler [Entity Directory]
selectUserPrograms uid n m
  | n < m     = runDBtime $ selectList [DirectoryUserId ==. uid ] [OffsetBy n, LimitTo (m-n) ]
  | otherwise = runDBtime $ selectList [] []

selectFirstProgram :: Handler (Maybe (Entity Directory))
selectFirstProgram =  do
  entity <- runDBtime $ selectList [] [Asc DirectoryId , LimitTo 1 ]
  case entity of
    [ e ]
        ->  return $ Just e
    _   ->  return Nothing

selectFirstUserProgram :: UserAccountId -> Handler (Maybe (Entity Directory))
selectFirstUserProgram uid =  do
  entity <- runDBtime $ selectList [DirectoryUserId ==. uid ] [Asc DirectoryId , LimitTo 1 ]
  case entity of
    [ e ]
        ->  return $ Just e
    _   ->  return Nothing

selectLastProgram :: Handler (Maybe (Entity Directory))
selectLastProgram =  do
  entity <- runDBtime $ selectList [] [Desc DirectoryId , LimitTo 1 ]
  case entity of
    [ e ]
        ->  return $ Just e
    _   ->  return Nothing

selectLastUserProgram :: UserAccountId -> Handler (Maybe (Entity Directory))
selectLastUserProgram uid =  do
  entity <- runDBtime $ selectList [DirectoryUserId ==. uid] [Desc DirectoryId , LimitTo 1 ]
  case entity of
    [ e ]
        ->  return $ Just e
    _   ->  return Nothing

selectNextProgram :: Maybe DirectoryId -> Handler (Maybe (Entity Directory))
selectNextProgram  Nothing  = selectFirstProgram
selectNextProgram (Just progId) =  do
  nextProgram <- runDBtime $ selectList [DirectoryId >. progId] [LimitTo 1]
  case nextProgram of
    [e]
      -> return  $ Just e
    _ -> selectFirstProgram

selectNextUserProgram :: UserAccountId -> Maybe DirectoryId -> Handler (Maybe (Entity Directory))
selectNextUserProgram  uid Nothing  = selectFirstUserProgram uid
selectNextUserProgram  uid (Just progId) =  do
  nextProgram <- runDBtime $ selectList [DirectoryUserId ==. uid, DirectoryId >. progId ] [LimitTo 1]
  case nextProgram of
    [e]
      -> return  $ Just e
    _ -> selectFirstUserProgram uid



selectPrevProgram :: Maybe DirectoryId -> Handler (Maybe (Entity Directory))
selectPrevProgram  Nothing  = selectLastProgram
selectPrevProgram (Just progId ) = do
  prevProgram <- runDBtime $ selectList [DirectoryId <. progId] [LimitTo 1]
  case prevProgram of
    [e]
      -> return $ Just e
    _ -> selectLastProgram

selectPrevUserProgram :: UserAccountId -> Maybe DirectoryId -> Handler (Maybe (Entity Directory))
selectPrevUserProgram  uid Nothing  = selectLastUserProgram uid
selectPrevUserProgram  uid (Just progId ) = do
  prevProgram <- runDBtime $ selectList [DirectoryUserId ==. uid, DirectoryId <. progId ] [LimitTo 1]
  case prevProgram of
    [e]
      -> return $ Just e
    _ -> selectLastProgram

countProgramGoals :: DirectoryId -> Handler Int
countProgramGoals pid =  runDBtime $ count [ FileDirectoryId ==. pid ]

countUserProgramGoals :: UserAccountId -> DirectoryId -> Handler Int
countUserProgramGoals uid pid =  runDBtime $ count [ FileUserId ==. uid, FileDirectoryId ==. pid ]

selectProgramGoals :: Int -> Int -> DirectoryId  -> Handler [Entity File]
selectProgramGoals n m pid
  | m > n     = runDBtime $ selectList [FileDirectoryId ==. pid ] [OffsetBy n , LimitTo (m-n)]
  | otherwise = runDBtime $ selectList [FileDirectoryId ==. pid ] []

selectUserProgramGoals :: UserAccountId -> Int -> Int -> DirectoryId  -> Handler [Entity File]
selectUserProgramGoals uid n m pid
  | m > n     = runDBtime $ selectList [FileUserId ==. uid ,  FileDirectoryId ==. pid ]
                                   [OffsetBy n , LimitTo (m-n)]
  | otherwise = runDBtime $ selectList [FileUserId ==. uid ,  FileDirectoryId ==. pid ] []

selectFirstUserIdent :: Handler (Maybe Text)
selectFirstUserIdent = do
  us <- runDB $ selectList [] [ Asc UserAccountId, LimitTo 1 ]
  case us of
    [ Entity _uid user ] ->  return $ Just (userAccountIdent user)
    _ -> return Nothing


-- submitGoal :: DirectoryId -> FileForm -> CC (PS Html) Handler (Maybe (Key File))
-- submitGoal progId (FileForm name (Textarea code)) = do
--   -- lift $ $(logInfo) $ "submitting goal:" ++ name
--   maybeGoal <- lift $ runDBtime $ getBy $ FileUniqueName progId name
--   case maybeGoal of
--     Just _  -> do
--       klabel <- lift $ generateCcLabel
--       html   <- lift $ defaultLayout $(widgetFile "prolog_goal_editor_error_duplicate_goal")
--       inquire klabel html
--       return Nothing
--     Nothing -> do
--       goalId <- lift $ runDBtime $ insert $ File name progId  code
--       -- lift $ $(logInfo) $ "submitted goal:" ++ name
--       return $ Just goalId


----------------------------  Deletions ------------------------------

deleteProgram :: DirectoryId -> Handler ()
deleteProgram pid = do
  goals <- selectProgramGoals 0 0 pid -- select all goals
  mapM_ ( deleteGoal . entityToId )  goals
  ts <- runDBtime $ selectList [ DirectoryTagDirectoryId ==. pid ] []
  runDBtime $ mapM_ deleteEntity ts
  runDBtime $ delete pid


deleteGoal :: FileId -> Handler ()
deleteGoal gid = do
  runDBtime $ selectList [ FileTagFileId ==. gid ] [] >>=  mapM_ deleteEntity
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


getProgramId :: UserAccountId -> Text -> Handler (Maybe DirectoryId)
getProgramId uid name = runMaybeT $ do
  let name' = T.strip name
  Entity pid _ <- MaybeT $ runDB $ getBy (UniqueDirectory uid name')
  return pid


getGoalId :: UserAccountId -> DirectoryId -> Text -> Handler (Maybe FileId)
getGoalId uid pid goalName = runMaybeT $ do
  let goalName' = T.strip goalName
  Entity gid _ <- MaybeT $ runDB $ getBy (UniqueFile uid pid goalName')
  return gid

getGoalCode :: FileId -> Handler (Maybe Text)
getGoalCode gid = runMaybeT $ fileCode <$> (MaybeT $ runDB $ get gid)

getProgramCode :: DirectoryId -> Handler (Maybe Text)
getProgramCode pid = runMaybeT $ directoryCode <$> (MaybeT $ runDB $ get pid)

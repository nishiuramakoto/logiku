-- Temporary version, NOT THREAD SAFE
{-# OPTIONS_GHC -w #-}

module Handler.PrologProgram  where

import             Authentication
import             Import hiding (parseQuery)
import             Control.Monad.CC.CCCxe
import             CCGraph
-- import             Data.Text(Text)
import qualified   Data.Text as T
import             Text.Read(reads)
import             Data.Time.LocalTime
import             Language.Prolog2
import             Database
import             DBFS
import             Constructors
import             Form
import             Text.Parsec
import             Control.Monad.Trans.Maybe


-- For testing
import             Handler.PrologTest
-------------------------- Helper functions --------------------------


type ProgramError = Either RuntimeError ParseError

entityToVal :: Entity t -> t
entityToVal (Entity _ val) = val

readInt :: Text -> Maybe Int
readInt s = case reads (T.unpack s) of
  [(i, s')] -> if s' == "" then Just i else Nothing
  _         -> Nothing

either3iso :: Either a (Either b c) -> Either (Either a b) c
either3iso x = case x of Left a -> Left (Left a)
                         Right (Left b) -> Left (Right b)
                         Right (Right c) -> Right c

programCheck' :: Text -> Either RuntimeError (Either ParseError Program)
programCheck' text = runIdentity $ evalPrologT $ consultString (T.unpack text)

programCheck :: Text -> Either ProgramError Program
programCheck text = either3iso $ programCheck' text

goalCheck' :: Text -> Either RuntimeError (Either ParseError [Goal])
goalCheck' text = runIdentity $ evalPrologT $ parseQuery (T.unpack text)

goalCheck :: Text -> Either ProgramError [Goal]
goalCheck text = either3iso $ goalCheck' text

programOK :: Text -> Bool
programOK text =  case programCheck text of
  Left  _    -> False
  Right _    -> True


maybeUserId' :: Handler (Maybe UserAccountId)
maybeUserId' = do
  muident <- maybeUserIdent
  -- muident <- selectFirstUserIdent
  $(logInfo) $ T.pack $ "muident" ++ show muident
  created <- liftIO $ getCurrentTime
  case muident of
    Just ident -> do Entity uid _ <- runDB $ upsert (makeUserAccount ident created created created ) ([] :: [Update UserAccount])
                     return $ Just uid
    Nothing    -> return Nothing


runTime :: (MonadIO m,MonadLogger m) => Text -> m a ->  m a
runTime tag action = do
  ZonedTime localTime _zone  <- liftIO getZonedTime
  $(logInfo) $ T.concat [ tag , ":start: " , T.pack $ show localTime ]
  x <- action
  ZonedTime localTime' _zone  <- liftIO getZonedTime
  $(logInfo) $ T.concat [ tag , ":end  : " , T.pack $ show localTime' ]
  return x

runCCTime :: Text -> CC CCP Handler a -> CC CCP Handler a
runCCTime tag action = do
  ZonedTime localTime _zone  <- liftIO getZonedTime
  lift $ $(logInfo) $ T.concat [ tag , ":start: " , T.pack $ show localTime ]
  x <- action
  ZonedTime localTime' _zone  <- liftIO getZonedTime
  lift $ $(logInfo) $ T.concat [ tag , ":end  : " , T.pack $ show localTime' ]
  return x



maybeNotFound :: MaybeT Handler a -> Handler a
maybeNotFound body = do m <- runMaybeT body
                        case m of
                          Just html -> return html
                          Nothing   -> notFound


------------------------------ Handlers ------------------------------

-- On the first page of the continuation, AJAX won't work.
-- However, if it is redirected from some other page, it works ok. What is happening?
-- For now we just redirect to the real page rather than investigating deeper.

getPrologProgramR ::  Handler Html
getPrologProgramR  = redirect PrologProgramImplR

getPrologProgramImplR ::  Handler Html
getPrologProgramImplR  = do
  uid <-  getUserAccountId
  st  <- startState
  run $ ccMain st uid

postPrologProgramContR  :: Int -> Handler Html
postPrologProgramContR node = do
  not_found_html <- defaultLayout [whamlet|postPrologProgramContR: Not Found|]
  resume node  not_found_html

postPrologProgramContSilentR :: Handler Html
postPrologProgramContSilentR = do
  not_found_html <- defaultLayout [whamlet|postPrologProgramContSilentR: Not Found|]

  mklabel <- lookupPostParam "_klabel"
  case join $ fmap readInt mklabel of
    Just node ->  resume node  not_found_html
    _ ->  return not_found_html


getPrologProgramContR :: Int -> Handler Html
getPrologProgramContR node = do
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume node not_found_html


-- postSyntaxCheckR :: Handler Html
-- postSyntaxCheckR = do
--   $(logInfo) "postSyntaxCheckR"
--   defaultLayout [whamlet|postSyntaxCheckR|]

-- Serve Ajax Request
postSyntaxCheckR :: Handler Value
postSyntaxCheckR = do
  $(logInfo) $ "postSyntaxCheckR"
  program <- (requireJsonBody :: Handler Directory)
  -- Comment code muid <- (requireJsonBody :: Handler Comment)

  $(logInfo) $ "postSyntaxCheckR"
  let result = case programCheck (directoryCode program) of
        Left err -> Just (show err)
        Right _  -> Nothing
  $(logInfo) $ T.pack $ show result
  returnJson result

postAjaxTestR :: Handler Value
postAjaxTestR = do
  $(logInfo) $ "getAjaxTestR"
  _program <- (requireJsonBody :: Handler Directory)
  $(logInfo) $ "getAjaxTestR"

  returnJson True

--------------------------  Session values  --------------------------
sessionProgramName =  "programName"
sessionUserIdent   = "userIdent"
------------------------------  Types --------------------------------

data DirectoryAction = Cancel | Save | New | Prev | Next | Delete
                         | CheckSyntax   | Run
                         | AddGoal | DeleteGoal | EditProgram

                deriving (Eq,Show)

type ProgramName = Text
type ProgramCode = Textarea
type ProgramExplanation = Textarea
type UserIdent   = Text

-------------------------- Program editor  --------------------------



directoryForm ::  ProgramName ->  ProgramExplanation -> ProgramCode
                  -> Html -> MForm Handler (FormResult DirectoryForm, Widget)
directoryForm name expl code = renderDivs $ DirectoryForm
              <$> areq textField      "プログラム名: "  (Just name)
              <*> areq textareaField  "説明: "          (Just expl)
              <*> areq textareaField  "コード: "        (Just code)

directoryWidget :: CCState -> CCNode ->  Widget -> Enctype -> Bool -> Widget
directoryWidget st node formWidget _enctype forceSave = do
  uid <- handlerToWidget $ getUserAccountId
  setTitle "View Prolog Program"
  addScript $ StaticR css_ace_src_noconflict_ace_js
    -- addStylesheet $ StaticR css_bootstrap_css
  $(widgetFile "prolog_program_editor")

directoryHtml ::  CCState -> ProgramName ->  ProgramExplanation -> ProgramCode -> Bool
                  ->  CCNode -> CC CCP Handler  Html
directoryHtml st name expl code forceSave node = do
  (formWidget, enctype) <- lift $ generateCCFormPost $ directoryForm name expl code
  lift $ defaultLayout $ directoryWidget st node formWidget enctype forceSave

inquireDirectory ::  CCState -> ProgramName -> ProgramExplanation ->  ProgramCode -> Bool
                        -> CC CCP Handler (CCState, Maybe DirectoryAction)
inquireDirectory st name expl code  forceSave = do

  inquirePostUntilButton st
    (directoryHtml st name expl code forceSave)
    (directoryForm name expl code)
    [ ("save", Save)
    , ("next", Next)
    , ("prev" , Prev)
    , ("add_goal", AddGoal)
    , ("delete", Delete)
    , ("checkSyntax" , CheckSyntax)
    , ("run", Run)
    ]

-------------------------- Goal editor  --------------------------


fileEditorForm ::  Html -> MForm Handler (FormResult FileForm, Widget)
fileEditorForm = renderDivs $ FileForm
              <$> areq textField      "ゴール名:" Nothing
              <*> areq textareaField  "説明: "  Nothing
              <*> areq textareaField  "コード:" Nothing

fileEditorWidget :: CCState -> CCNode -> UserIdent ->  ProgramName -> ProgramExplanation -> ProgramCode
                    -> [File] -> Widget -> Enctype  -> Widget
fileEditorWidget st node userIdent programName programExplanation programCode goals
                   formWidget  enctype = do
                         setTitle "Edit Prolog Goals"
                         $(widgetFile "prolog_goal_editor")

fileEditorHtml ::  CCState -> UserIdent -> ProgramName -> ProgramExplanation -> ProgramCode -> [File]
                   -> CCNode -> CC CCP Handler Html
fileEditorHtml  st userIdent name explanation code goals node = do
  (formWidget , enctype ) <- lift $ generateCCFormPost $ fileEditorForm
  lift $ defaultLayout $
           fileEditorWidget st node userIdent name explanation code goals  formWidget  enctype

inquireFileEditor :: CCState -> UserIdent -> ProgramName -> ProgramExplanation -> ProgramCode -> [File]
                        -> CC CCP Handler (CCState, Maybe DirectoryAction)
inquireFileEditor st userIdent name explanation code goals  = do
  inquirePostButton st  (fileEditorHtml st userIdent name explanation code goals) (fileEditorForm)
    [ ("submit", Save), ("back", EditProgram) ]

----------------------------  Dummy page  ----------------------------

dummyForm :: Html -> MForm Handler (FormResult DummyForm, Widget)
dummyForm = renderDivs $ DummyForm <$> areq boolField "" Nothing
dummyWidget :: CCState -> CCNode -> Widget -> Enctype -> Widget
dummyWidget st node formWidget enctype =  $(widgetFile "dummy-page")
dummyHtml :: CCState -> CCNode -> CC CCP Handler Html
dummyHtml st node = do
  (formWidget, enctype) <- lift $ generateCCFormPost $ dummyForm
  lift $ defaultLayout $ dummyWidget st node formWidget enctype
inquireDummy :: CCState -> CC CCP Handler (CCState, Maybe Bool)
inquireDummy st = do
  inquirePostButton st (dummyHtml st) dummyForm [ ("ok", True) ]

---------------- inquire response to the parse error  ----------------
inquireParseError :: CCState -> ParseError -> ProgramName -> ProgramCode -> CC CCP Handler ()
inquireParseError st err name code = do
   lift $ $(logInfo)  $ "parse error:" ++ (T.pack (show err))
--   klabel <- generateCCLabel
--   html   <- widgetToHtml $(widgetFile "parse_error")
--   (answer, maybeAction) <- inquirePostButton klabel html
   return ()

-- ---------------- inquire response to the parse error  ----------------
inquireParseSuccess :: CCState -> ProgramName -> ProgramCode
                   -> CC CCP Handler ()
inquireParseSuccess st name code = do
   lift $ $(logInfo)  "parse success"
   return ()

------------------------------  finish  ------------------------------
directoryFinishHtml :: CC CCP Handler Html
directoryFinishHtml = lift $ redirect HomeR

------------------------  Application logic  ------------------------

-- entityKey (Entity key _) = key

ccMain :: CCState -> UserAccountId -> CC CCP Handler Html
ccMain st uid =  do
  -- lift $ do
  --   (widget, enctype) <- generateFormPost prologTestForm
  --   defaultLayout $ prologTestWidget widget enctype

--   lift $ redirect PrologTestR
  -- inquireDummy

  mentity <- lift $ selectFirstUserProgram uid
  loopBrowse st uid (fmap entityKey mentity) False
  directoryFinishHtml >>=  inquireFinish

loopBrowse :: CCState -> UserAccountId -> Maybe DirectoryId -> Bool -> CC CCP Handler ()
loopBrowse st uid Nothing forceSave = do
  ((_, FormDirectoryForm (FormSuccess (DirectoryForm name (Textarea expl) (Textarea code))))
    , maybeAction )  <- inquireDirectory st  "" (Textarea "") (Textarea "") forceSave

  case maybeAction of
    Just Save -> do
      mkey <- lift $ createProgram uid name expl code
      case mkey of
        Just pid ->  do mprog <- lift $ runDB $ get pid
                        case mprog of
                          Just prog -> loopBrowse st uid (Just  pid) forceSave
                          Nothing   -> loopBrowse st uid Nothing forceSave
        Nothing  ->  loopBrowse st uid Nothing forceSave

    Just Next ->   do mentity <- lift $ selectFirstUserProgram uid
                      loopBrowse st uid (entityKey <$> mentity) forceSave

    Just Prev ->  do  mentity <- lift $ selectLastUserProgram uid
                      loopBrowse st uid  (entityKey <$> mentity) forceSave
    _ -> loopBrowse st uid Nothing forceSave

loopBrowse st uid (Just pid) forceSave = do
  mentity <- lift $ runDB $ get pid
  case mentity of
    Just currentProgram -> loopBrowse' st currentProgram
    Nothing -> return ()

  where
    loopBrowse' st currentProgram = do

      let uid' = directoryUserId currentProgram
          name = Import.directoryName   currentProgram
          expl = Import.directoryExplanation   currentProgram
          code = Import.directoryCode   currentProgram
      lift $ $(logInfo) $ T.pack $ show uid ++ show uid' ++ show name ++ show code ++ show forceSave

      ((_, FormDirectoryForm (FormSuccess (DirectoryForm newName (Textarea newExplanation) (Textarea newCode)))), maybeAction )  <-  inquireDirectory st name (Textarea expl) (Textarea code) forceSave

      if (not (elem maybeAction [Just Next, Just Prev, Just AddGoal]) &&  uid /= uid')
        then do lift $ setMessage $ toHtml $ ("他のユーザのプログラムは変更できません" :: Text)
                loopBrowse st uid (Just pid) forceSave
        else case maybeAction of
        Just Save -> do
          if forceSave || programOK newCode
            then do lift $ $(logInfo)  $ "saving code:" ++ T.pack (show newName)
                    newProg <- lift $ writeProgram uid newName newExplanation newCode
                    loopBrowse st uid newProg False

            else do lift $ $(logInfo) $ "Enter force save mode:" ++ T.pack (show newName)
                    loopBrowse st uid (Just pid)  True

        Just Next ->   do mentity <- lift $ selectNextUserProgram uid (Just pid)
                          loopBrowse st uid (entityKey <$> mentity) False

        Just Prev ->  do mentity <- lift $ selectPrevUserProgram uid  (Just pid)
                         loopBrowse st uid (entityKey <$> mentity) False

        Just AddGoal -> do loopGoals  st uid  pid
                           loopBrowse st uid  (Just pid) False

        Just Delete  -> do mentity <- lift $ selectNextUserProgram uid (Just pid)
                           -- lift $ deleteProgram pid
                           loopBrowse st uid (entityKey <$> mentity) False

        _ -> loopBrowse st uid (Just pid) forceSave




loopGoals :: CCState -> UserAccountId ->  DirectoryId ->  CC CCP Handler ()
loopGoals  st uid pid = do
  lift $ $(logInfo) "loopGoals"
  ment <- lift $ runDB $ get pid
  case ment of
    Just prog -> go prog
    Nothing     -> return ()
    where
      go  program = do
        goals      <- lift $ selectUserProgramGoals uid 0 0 pid -- current user
        muserIdent <- lift $ getUserIdent (directoryUserId program)  -- program owner
        case muserIdent of
          Just userIdent -> loopGoals' st userIdent pid goals
          Nothing        -> do lift $ setMessage "Database is broken. Please report to the maintainer."
                               loopBrowse st  uid (Just pid) False

loopGoals' :: CCState -> UserIdent ->  DirectoryId -> [Entity File] -> CC CCP Handler ()
loopGoals' st userIdent pid goals = do
  mprog <- lift $ runDB $ get pid
  case mprog of
    Just prog -> go prog
    Nothing   -> return ()

  where
    go prog = do
      let name = Import.directoryName prog
          expl = Import.directoryExplanation prog
          code = Import.directoryCode prog

      lift $ $(logInfo) "loopGoals'"
      lift $ setSession "userIdent"   userIdent
      lift $ setSession "programName" name

      ( (_, FormFileForm resultForm) , _ )
             <- inquireFileEditor st userIdent name  (Textarea expl) (Textarea code) (map entityToVal goals)

      lift $ $(logInfo) "loopGoals'"
      mval <- lift $ lookupGetParam "action"
      lift $ $(logInfo) $ T.pack $ show mval

      case resultForm of
        FormSuccess _ ->  loopGoals' st userIdent pid goals
        err           ->  do lift $ defaultLayout [whamlet|show err|]
                             return ()



postGoalR :: Handler Value
postGoalR = do
  mval <- runMaybeT maybeGoal
  case mval of
    Just val -> returnJson val
    Nothing  -> notFound

maybeGoal :: MaybeT Handler FileEditResponseJson
maybeGoal = do
  FileEditRequestJson goalName explanation code <- lift (requireJsonBody :: Handler FileEditRequestJson)

  user     <- MaybeT $ lookupSession "userIdent"
  progName <- MaybeT $ lookupSession "programName"

  lift $ $(logInfo) "maybeGoal"

  uid <- MaybeT $ getUserId user
  pid <- MaybeT $ getProgramId uid progName

  lift $  $(logInfo) "maybeGoal"

  gid <- MaybeT $ createGoal uid pid goalName explanation code

  return (FileEditResponseJson True  Nothing)

postDeleteGoalR :: Handler Value
postDeleteGoalR = do
  mval <- runMaybeT maybeDeleteGoal
  case mval of
    Just val -> returnJson val
    Nothing  -> returnJson (FileEditResponseJson False (Just "delete error"))


maybeDeleteGoal :: MaybeT Handler FileEditResponseJson
maybeDeleteGoal = do
  jsonVal@(FileEditRequestJson goalName _ _) <- lift (requireJsonBody :: Handler FileEditRequestJson)

  user     <- MaybeT $ lookupSession sessionUserIdent
  progName <- MaybeT $ lookupSession sessionProgramName

  lift $ $(logInfo) "maybeDeleteGoal"

  uid <- MaybeT $ getUserId user
  pid <- MaybeT $ getProgramId uid progName

  lift $  $(logInfo) "maybeDeleteGoal"

  gid <- MaybeT $ getGoalId uid pid goalName

  lift $ deleteGoal gid
  return $ FileEditResponseJson True Nothing


getPrologGoalRunnerR :: Handler Html
getPrologGoalRunnerR = maybeNotFound $ do
  goalName   <- MaybeT $ lookupGetParam "goal"
  progName   <- MaybeT $ lookupSession sessionProgramName
  userIdent  <- MaybeT $ lookupSession sessionUserIdent

  uid <- MaybeT $ getUserId userIdent
  pid <- MaybeT $ getProgramId uid progName
  gid <- MaybeT $ getGoalId uid pid goalName

  progCode <- MaybeT $ getProgramCode  pid
  goalCode <- MaybeT $ getGoalCode gid

  st <- lift startState
  lift $ executeDirectory st progCode goalCode
  -- lift $ defaultLayout $ [whamlet|#{show (progCode,goalCode)}|]

executeDirectory :: CCState -> Text -> Text -> Handler Html
executeDirectory st progCode goalCode =
  case (programCheck  progCode , goalCheck goalCode) of
  (Right clauses, Right terms)   -> run $ prologExecuteCCMain st progCode goalCode

  (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in the program #{show err}|]
  (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in the goals   #{show err}|]

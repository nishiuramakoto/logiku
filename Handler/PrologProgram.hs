-- A temporary version, NOT THREAD SAFE

module Handler.PrologProgram  where

import             Authentication
import             Import hiding (parseQuery)
import             Control.Monad.CC.CCCxe
import             ContMap
-- import             Data.Text(Text)
import qualified   Data.Text as T
import             Text.Read(reads)
import             Data.Time.LocalTime
import             Language.Prolog2
import             Database
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


maybeUserId :: Handler (Maybe UserAccountId)
maybeUserId = do
  muident <- maybeUserIdent
  -- muident <- selectFirstUserIdent
  $(logInfo) $ T.pack $ "muident" ++ show muident
  case muident of
    Just ident -> do Entity uid _ <- runDB $ upsert (makeUser ident) ([] :: [Update UserAccount])
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

runCcTime :: Text -> CC (PS Html) Handler a -> CC (PS Html) Handler a
runCcTime tag action = do
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
  muid <-  maybeUserId
  case muid of
    Just uid ->  run $ ccMain uid
    Nothing  ->  do
      setMessage $ toHtml ("Login first" :: Text)
      redirect Portfolio01R

postPrologProgramContR  :: Int -> Handler Html
postPrologProgramContR klabel = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html

postPrologProgramContSilentR :: Handler Html
postPrologProgramContSilentR = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]

  mklabel <- lookupPostParam "_klabel"
  case join $ fmap readInt mklabel of
    Just klabel ->  resume klabel cont_html not_found_html
    _ ->  return not_found_html


getPrologProgramContR :: Int -> Handler Html
getPrologProgramContR klabel = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html


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

data DirectoryForm = DirectoryForm { directoryName        :: Text
                                           , directoryExplanation :: Textarea
                                           , directoryProgram     :: Textarea
                                           } deriving Show


directoryForm ::  ProgramName ->  ProgramExplanation -> ProgramCode
                  -> Html -> MForm Handler (FormResult DirectoryForm, Widget)
directoryForm name expl code = renderDivs $ DirectoryForm
              <$> areq textField      "プログラム名: "  (Just name)
              <*> areq textareaField  "説明: "          (Just expl)
              <*> areq textareaField  "コード: "        (Just code)

directoryWidget :: ContId ->  Widget -> Enctype -> Bool -> Widget
directoryWidget klabel formWidget _enctype forceSave = do
  setTitle "View Prolog Program"
  addScript $ StaticR css_ace_src_noconflict_ace_js
    -- addStylesheet $ StaticR css_bootstrap_css
  $(widgetFile "prolog_program_editor")

directoryHtml ::  ProgramName ->  ProgramExplanation -> ProgramCode -> Bool
                  ->  CC (PS Html) Handler (ContId, Html)
directoryHtml name expl code forceSave = do
  (klabel, formWidget, enctype) <- lift $ generateCcFormPost $ directoryForm name expl code
  html   <- lift $ defaultLayout $ directoryWidget klabel  formWidget enctype forceSave
  return (klabel, html)

inquireDirectory ::  ProgramName -> ProgramExplanation ->  ProgramCode -> Bool
                        -> CC (PS Html) Handler (ContId, Maybe DirectoryAction, DirectoryForm)
inquireDirectory name expl code  forceSave = do

  (klabel, html)         <- directoryHtml name expl code forceSave

  (answer, maybeAction)  <- inquirePostUntilButton klabel html (directoryForm name expl code)
                            [ ("save", Save) , ("next", Next) , ("prev" , Prev) , ("add_goal", AddGoal)
                            , ("delete", Delete) , ("checkSyntax" , CheckSyntax) , ("run", Run)
                            ]
  return (klabel, maybeAction, answer)

-------------------------- Goal editor  --------------------------

data FileForm = FileForm { goalName   ::  Text
                                     , goalExpl   ::  Textarea
                                     , goalCode   ::  Textarea
                                     } deriving Show
data PrologRunnerForm = PrologRunnerForm deriving Show

fileEditorForm ::  Html -> MForm Handler (FormResult FileForm, Widget)
fileEditorForm = renderDivs $ FileForm
              <$> areq textField      "ゴール名:" Nothing
              <*> areq textareaField  "説明: "  Nothing
              <*> areq textareaField  "コード:" Nothing

fileEditorWidget :: UserIdent ->  ProgramName -> ProgramExplanation -> ProgramCode  -> [File]
                       -> ContId ->  Widget -> Enctype
                       -> Widget
fileEditorWidget userIdent programName programExplanation programCode goals
                       klabel  formWidget  enctype
  = do   setTitle "Edit Prolog Goals"
         $(widgetFile "prolog_goal_editor")

fileEditorHtml ::  UserIdent -> ProgramName -> ProgramExplanation -> ProgramCode -> [File]
                         -> CC (PS Html) Handler (ContId, Html)
fileEditorHtml  userIdent name explanation code goals = do
  (klabel , formWidget , enctype ) <- lift $ generateCcFormPost $ fileEditorForm
  html  <- lift $ defaultLayout $
           fileEditorWidget userIdent name explanation code goals
                                  klabel  formWidget  enctype
  return (klabel, html)

inquireFileEditor :: UserIdent -> ProgramName -> ProgramExplanation -> ProgramCode -> [File]
                        -> CC (PS Html) Handler (ContId, Maybe DirectoryAction, FormResult FileForm)
inquireFileEditor userIdent name explanation code goals  = do
  (klabel, html)         <- fileEditorHtml userIdent name explanation code goals
  (answer, maybeAction)  <- inquirePostButton klabel html (fileEditorForm)
                            [ ("submit", Save), ("back", EditProgram) ]
  return (klabel, maybeAction, answer)

----------------------------  Dummy page  ----------------------------

data DummyForm = DummyForm Bool deriving Show
dummyForm :: Html -> MForm Handler (FormResult DummyForm, Widget)
dummyForm = renderDivs $ DummyForm <$> areq boolField "" Nothing
dummyWidget :: ContId -> Widget -> Enctype -> Widget
dummyWidget klabel formWidget enctype = $(widgetFile "dummy-page")
dummyHtml :: CC (PS Html) Handler (ContId, Html)
dummyHtml = do
  (klabel, formWidget, enctype) <- lift $ generateCcFormPost $ dummyForm
  html <- lift $ defaultLayout $ dummyWidget klabel formWidget enctype
  return (klabel, html)
inquireDummy :: CC (PS Html) Handler ContId
inquireDummy = do
  (klabel, html) <- dummyHtml
  inquirePostButton klabel html dummyForm [ ("ok", True) ]
  return klabel

---------------- inquire response to the parse error  ----------------
inquireParseError ::  ParseError -> ProgramName -> ProgramCode
                  -> CC (PS Html) Handler ()
inquireParseError err name code = do
   lift $ $(logInfo)  $ "parse error:" ++ (T.pack (show err))
--   klabel <- generateCcLabel
--   html   <- widgetToHtml $(widgetFile "parse_error")
--   (answer, maybeAction) <- inquirePostButton klabel html
   return ()

-- ---------------- inquire response to the parse error  ----------------
inquireParseSuccess :: ProgramName -> ProgramCode
                   -> CC (PS Html) Handler ()
inquireParseSuccess name code = do
   lift $ $(logInfo)  "parse success"
   return ()

------------------------------  finish  ------------------------------
directoryFinishHtml :: CC (PS Html) Handler Html
directoryFinishHtml = lift $ redirect Portfolio01R


------------------------  Application logic  ------------------------

-- entityKey (Entity key _) = key

ccMain :: UserAccountId -> CC (PS Html) Handler Html
ccMain uid =  do
  -- lift $ do
  --   (widget, enctype) <- generateFormPost prologTestForm
  --   defaultLayout $ prologTestWidget widget enctype

--   lift $ redirect PrologTestR
  -- inquireDummy

  mentity <- lift $ selectFirstUserProgram uid
  loopBrowse uid (fmap entityKey mentity) False
  directoryFinishHtml >>=  inquireFinish

loopBrowse :: UserAccountId -> Maybe DirectoryId -> Bool -> CC (PS Html) Handler ()
loopBrowse uid Nothing forceSave = do
  (   _klabel
    , maybeAction
    , _newProgram@(DirectoryForm name (Textarea expl) (Textarea code)))
               <- inquireDirectory  "" (Textarea "") (Textarea "") forceSave

  case maybeAction of
    Just Save -> do
      mkey <- lift $ createProgram uid name expl code
      case mkey of
        Just pid ->  do mprog <- lift $ runDB $ get pid
                        case mprog of
                          Just prog -> loopBrowse uid (Just  pid) forceSave
                          Nothing   -> loopBrowse uid Nothing forceSave
        Nothing  ->  loopBrowse uid Nothing forceSave

    Just Next ->   do mentity <- lift $ selectFirstUserProgram uid
                      loopBrowse uid (entityKey <$> mentity) forceSave

    Just Prev ->  do  mentity <- lift $ selectLastUserProgram uid
                      loopBrowse uid  (entityKey <$> mentity) forceSave
    _ -> loopBrowse uid Nothing forceSave

loopBrowse uid (Just pid) forceSave = do
  mentity <- lift $ runDB $ get pid
  case mentity of
    Just currentProgram -> loopBrowse' currentProgram
    Nothing -> return ()

  where
    loopBrowse' currentProgram = do

      let uid' = directoryUserId currentProgram
          name = Import.directoryName   currentProgram
          expl = Import.directoryExplanation   currentProgram
          code = Import.directoryCode   currentProgram
      lift $ $(logInfo) $ T.pack $ show uid ++ show uid' ++ show name ++ show code ++ show forceSave

      (   _klabel
        , maybeAction
        , (DirectoryForm newName (Textarea newExplanation) (Textarea newCode)))
                  <-  inquireDirectory name (Textarea expl) (Textarea code) forceSave

      if (not (elem maybeAction [Just Next, Just Prev, Just AddGoal]) &&  uid /= uid')
        then do lift $ setMessage $ toHtml $ ("他のユーザのプログラムは変更できません" :: Text)
                loopBrowse uid (Just pid) forceSave
        else case maybeAction of
        Just Save -> do
          if forceSave || programOK newCode
            then do lift $ $(logInfo)  $ "saving code:" ++ T.pack (show newName)
                    newProg <- lift $ writeProgram uid newName newExplanation newCode
                    loopBrowse uid newProg False

            else do lift $ $(logInfo) $ "Enter force save mode:" ++ T.pack (show newName)
                    loopBrowse uid (Just pid)  True

        Just Next ->   do mentity <- lift $ selectNextUserProgram uid (Just pid)
                          loopBrowse uid (entityKey <$> mentity) False

        Just Prev ->  do mentity <- lift $ selectPrevUserProgram uid  (Just pid)
                         loopBrowse uid (entityKey <$> mentity) False

        Just AddGoal -> do loopGoals  uid  pid
                           loopBrowse uid  (Just pid) False

        Just Delete  -> do mentity <- lift $ selectNextUserProgram uid (Just pid)
                           -- lift $ deleteProgram pid
                           loopBrowse uid (entityKey <$> mentity) False

        _ -> loopBrowse uid (Just pid) forceSave




loopGoals :: UserAccountId ->  DirectoryId ->  CC (PS Html) Handler ()
loopGoals  uid pid = do
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
          Just userIdent -> loopGoals' userIdent pid goals
          Nothing        -> do lift $ setMessage "Database is broken. Please report to the maintainer."
                               loopBrowse uid (Just pid) False

loopGoals' :: UserIdent ->  DirectoryId -> [Entity File] -> CC (PS Html) Handler ()
loopGoals' userIdent pid goals = do
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

      (   _klabel
        , _maybeAction
        , resultForm )
         <- inquireFileEditor userIdent name  (Textarea expl) (Textarea code) (map entityToVal goals)

      lift $ $(logInfo) "loopGoals'"
      mval <- lift $ lookupGetParam "action"
      lift $ $(logInfo) $ T.pack $ show mval

      case resultForm of
        FormSuccess _ ->  loopGoals' userIdent pid goals
        err           ->  do lift $ defaultLayout [whamlet|show err|]
                             return ()



postGoalR :: Handler Value
postGoalR = do
  mval <- runMaybeT maybeGoal
  case mval of
    Just val -> returnJson val
    Nothing  -> notFound

maybeGoal :: MaybeT Handler FileJson
maybeGoal = do
  FileJson goalName explanation code <- lift (requireJsonBody :: Handler FileJson)

  user     <- MaybeT $ lookupSession "userIdent"
  progName <- MaybeT $ lookupSession "programName"

  lift $ $(logInfo) "maybeGoal"

  uid <- MaybeT $ getUserId user
  pid <- MaybeT $ getProgramId uid progName

  lift $  $(logInfo) "maybeGoal"

  gid <- MaybeT $ createGoal uid pid goalName explanation code

  return (FileJson goalName explanation code)

postDeleteGoalR :: Handler Value
postDeleteGoalR = do
  mval <- runMaybeT maybeDeleteGoal
  case mval of
    Just val -> returnJson val
    Nothing  -> notFound


maybeDeleteGoal :: MaybeT Handler FileJson
maybeDeleteGoal = do
  jsonVal@(FileJson goalName _ _) <- lift (requireJsonBody :: Handler FileJson)

  user     <- MaybeT $ lookupSession sessionUserIdent
  progName <- MaybeT $ lookupSession sessionProgramName

  lift $ $(logInfo) "maybeDeleteGoal"

  uid <- MaybeT $ getUserId user
  pid <- MaybeT $ getProgramId uid progName

  lift $  $(logInfo) "maybeDeleteGoal"

  gid <- MaybeT $ getGoalId uid pid goalName

  lift $ deleteGoal gid
  return jsonVal


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

  lift $ executeDirectory progCode goalCode
  -- lift $ defaultLayout $ [whamlet|#{show (progCode,goalCode)}|]

executeDirectory :: Text -> Text -> Handler Html
executeDirectory progCode goalCode =
  case (programCheck  progCode , goalCheck goalCode) of
  (Right clauses, Right terms)   -> run $ prologExecuteCcMain progCode goalCode

  (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in the program #{show err}|]
  (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in the goals   #{show err}|]

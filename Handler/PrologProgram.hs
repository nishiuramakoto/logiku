-- A temporary version, NOT THREAD SAFE

module Handler.PrologProgram  where

import             Import
import             Control.Monad.CC.CCCxe
import             ContMap
import             Data.Text(Text)
import qualified   Data.Text as T
import             Text.Read(reads)
import             Data.Time.LocalTime
import             Prolog(consultString, parseQuery)
import             Text.Parsec
import             Database
import             Authentication

-------------------------- Helper functions --------------------------
readInt :: Text -> Maybe Int
readInt s = case reads (T.unpack s) of
  [(i, s')] -> if s' == "" then Just i else Nothing
  _         -> Nothing


syntaxOK :: Text -> Bool
syntaxOK text =
  case consultString (T.unpack text) of
  Left  _    -> False
  Right _    -> True


maybeUserId :: Handler (Maybe UserId)
maybeUserId = do
  muident <- maybeUserIdent
  $(logInfo) $ T.pack $ "muident" ++ show muident
  case muident of
    Just ident -> do Entity uid _ <- runDB $ upsert (User ident Nothing) ([] :: [Update User])
                     return $ Just uid
    Nothing    -> return Nothing

runCCtime :: CC (PS Html) Handler a ->  CC (PS Html) Handler a
runCCtime action = do
  ZonedTime localTime zone  <- liftIO getZonedTime
  lift $ $(logInfo) $ T.pack $ "CC begin:" ++ show localTime
  x <- action
  ZonedTime localTime zone  <- liftIO getZonedTime
  lift $ $(logInfo) $ T.pack $ "CC end:" ++ show localTime
  return x


runTime :: (MonadIO m,MonadLogger m) => Text -> m a ->  m a
runTime tag action = do
  ZonedTime localTime zone  <- liftIO getZonedTime
  $(logInfo) $ T.concat [ tag , ":start: " , T.pack $ show localTime ]
  x <- action
  ZonedTime localTime zone  <- liftIO getZonedTime
  $(logInfo) $ T.concat [ tag , ":end  : " , T.pack $ show localTime ]
  return x

runCcTime :: Text -> CC (PS Html) Handler a -> CC (PS Html) Handler a
runCcTime tag action = do
  ZonedTime localTime zone  <- liftIO getZonedTime
  lift $ $(logInfo) $ T.concat [ tag , ":start: " , T.pack $ show localTime ]
  x <- action
  ZonedTime localTime zone  <- liftIO getZonedTime
  lift $ $(logInfo) $ T.concat [ tag , ":end  : " , T.pack $ show localTime ]
  return x


------------------------------ Handlers ------------------------------

getPrologProgramR ::  Handler Html
getPrologProgramR  = do
  muid <- runTime "maybeUserId" $ maybeUserId
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

postSyntaxCheckR :: Handler Value
postSyntaxCheckR = do
  PrologProgram _uid _name _expl code <- (requireJsonBody :: Handler PrologProgram)
  let result = case consultString (T.unpack code) of
        Left error -> Just (show error)
        Right _    -> Nothing
  -- $(logInfo) $ T.pack $ show result
  returnJson result

------------------------------  Types --------------------------------

data PrologProgramAction = Cancel | Save | New | Prev | Next | Delete
                         | CheckSyntax
                         | AddGoal | DeleteGoal | EditProgram

                deriving (Eq,Show)

type ProgramName = Text
type ProgramCode = Textarea
type ProgramExplanation = Textarea

-------------------------- Program editor  --------------------------

data PrologProgramForm = PrologProgramForm { prologProgramName        :: Text
                                           , prologProgramExplanation :: Textarea
                                           , prologProgramProgram     :: Textarea
                                           } deriving Show


prologProgramForm ::  ProgramName ->  ProgramExplanation -> ProgramCode
                  -> Html -> MForm Handler (FormResult PrologProgramForm, Widget)
prologProgramForm name expl program = renderDivs $ PrologProgramForm
              <$> areq textField      "プログラム名: "  (Just name)
              <*> areq textareaField  "説明: "          (Just expl)
              <*> areq textareaField  "コード: "        (Just program)

prologProgramWidget :: ContId ->  Widget -> Enctype -> Bool -> Widget
prologProgramWidget klabel formWidget enctype forceSave = do
  setTitle "View Prolog Program"
  addScript $ StaticR css_ace_src_noconflict_ace_js
    -- addStylesheet $ StaticR css_bootstrap_css
  $(widgetFile "prolog_program_editor")

prologProgramHtml ::  ProgramName ->  ProgramExplanation -> ProgramCode -> Bool
                  ->  CC (PS Html) Handler (ContId, Html)
prologProgramHtml name expl program forceSave = do
  (klabel, formWidget, enctype) <- runCcTime "generateCcFormPost" $
                                   lift $ generateCcFormPost $ prologProgramForm name expl program
  html   <- runCcTime "defaultLayout" $
            lift $ defaultLayout $ runTime "widget" $ prologProgramWidget klabel  formWidget enctype forceSave
  return (klabel, html)

inquirePrologProgram ::  ProgramName -> ProgramExplanation ->  ProgramCode -> Bool
                        -> CC (PS Html) Handler (ContId, Maybe PrologProgramAction, PrologProgramForm)
inquirePrologProgram name expl program  forceSave = do

  (klabel, html)         <- runCcTime "prologProgramHtml" $ prologProgramHtml name expl program forceSave

  (answer, maybeAction)  <- inquirePostUntilButton klabel html (prologProgramForm name expl program)
                            [ ("save", Save) , ("next", Next) , ("prev" , Prev) , ("add_goal", AddGoal)
                            , ("delete", Delete) , ("checkSyntax" , CheckSyntax)
                            ]
  return (klabel, maybeAction, answer)

-------------------------- Goal editor  --------------------------

data PrologGoalForm = PrologGoalForm { goalName   ::  Text
                                     , goalExpl   ::  Textarea
                                     , goalCode   ::  Textarea
                                     } deriving Show

prologGoalEditorForm ::  Html -> MForm Handler (FormResult PrologGoalForm, Widget)
prologGoalEditorForm = renderDivs $ PrologGoalForm
              <$> areq textField      "ゴール名:" Nothing
              <*> areq textareaField  "説明: "  Nothing
              <*> areq textareaField  "コード:" Nothing

prologGoalEditorWidget :: ProgramName -> ProgramExplanation -> ProgramCode
                       -> [PrologGoal] -> ContId ->  Widget -> Enctype
                       -> Widget
prologGoalEditorWidget programName programExplanation programCode goals  klabel  formWidget enctype = do
  setTitle "Edit Prolog Goals"
  $(widgetFile "prolog_goal_editor")

prologGoalEditorHtml ::  ProgramName -> ProgramExplanation -> ProgramCode -> [PrologGoal]
                         -> CC (PS Html) Handler (ContId, Html)
prologGoalEditorHtml  name explanation code goals = do
  (klabel, formWidget, enctype) <- lift $ generateCcFormPost $ prologGoalEditorForm
  html  <- lift $ defaultLayout $ prologGoalEditorWidget name explanation code goals klabel formWidget enctype
  return (klabel, html)

inquirePrologGoalEditor :: ProgramName -> ProgramExplanation -> ProgramCode -> [PrologGoal]
                        -> CC (PS Html) Handler (ContId, Maybe PrologProgramAction, FormResult PrologGoalForm)
inquirePrologGoalEditor name explanation code goals  = do
  (klabel, html)         <- prologGoalEditorHtml name explanation code goals
  (answer, maybeAction)  <- inquirePostButton klabel html (prologGoalEditorForm)
                            [ ("save", Save), ("back", EditProgram) ]
  return (klabel, maybeAction, answer)

---------------- inquire response to the parse error  ----------------
-- inquireParseError ::  ParseError -> ProgramName -> ProgramCode
--                   -> CC (PS Html) Handler ()
-- inquireParseError error name code = do
--   lift $ $(logInfo)  $ "parse error:" ++ (T.pack (show error))
--   klabel <- generateCcLabel
--   html   <- widgetToHtml $(widgetFile "parse_error")
--   (answer, maybeAction) <- inquirePostButton klabel html
--   return ()

-- ---------------- inquire response to the parse error  ----------------
-- inquireParseSuccess :: ProgramName -> ProgramCode
--                   -> CC (PS Html) Handler ()
-- inquireParseSuccess name code = do
--   lift $ $(logInfo)  "parse success"
--   return ()

------------------------------  finish  ------------------------------
prologProgramFinishHtml :: CC (PS Html) Handler Html
prologProgramFinishHtml = lift $ redirect Portfolio01R


------------------------  Application logic  ------------------------

ccMain :: UserId -> CC (PS Html) Handler Html
ccMain uid =  do
  program <- lift $ selectFirstUserProgram uid
  loopBrowse uid program False
  prologProgramFinishHtml >>=  inquireFinish

loopBrowse :: UserId -> Maybe (Entity PrologProgram) -> Bool -> CC (PS Html) Handler ()
loopBrowse uid Nothing forceSave = do
  (   _klabel
    , maybeAction
    , newProgram@(PrologProgramForm name (Textarea expl) (Textarea code)))
               <- inquirePrologProgram  "" (Textarea "") (Textarea "") forceSave

  case maybeAction of
    Just Save -> do
      mkey <- lift $ insertPrologProgram (PrologProgram uid name expl code)
      case mkey of
        Just pid ->  do mprog <- lift $ runDB $ get pid
                        case mprog of
                          Just prog -> loopBrowse uid (Just (Entity pid prog)) forceSave
                          Nothing   -> loopBrowse uid Nothing forceSave
        Nothing  ->  loopBrowse uid Nothing forceSave

    Just Next ->  runCCtime $ do nextProgram <- lift $ selectFirstUserProgram uid
                                 loopBrowse uid nextProgram forceSave

    Just Prev ->  do prevProgram <- lift $ selectLastUserProgram uid
                     loopBrowse uid  prevProgram forceSave
    _ -> loopBrowse uid Nothing forceSave

loopBrowse uid (Just entity@(Entity pid currentProgram@(PrologProgram uid' name expl code ))) forceSave = do
  lift $ $(logInfo) $ T.pack $ show uid ++ show uid' ++ show name ++ show code ++ show forceSave

  (   _klabel
    , maybeAction
    , (PrologProgramForm newName (Textarea newExplanation) (Textarea newCode)))
                <-  runCCtime $ inquirePrologProgram name (Textarea expl) (Textarea code) forceSave

  let newProgram = PrologProgram uid newName newExplanation newCode

  if (not (elem maybeAction [Just Next, Just Prev, Just AddGoal]) &&  uid /= uid')
    then do lift $ setMessage $ toHtml $ ("他のユーザのプログラムは変更できません" :: Text)
            loopBrowse uid (Just (Entity pid currentProgram)) forceSave
    else case maybeAction of
    Just Save -> do  if forceSave || syntaxOK newCode
                       then do lift $ $(logInfo)  $ "saving code:" ++ T.pack (show newProgram)
                               newProg <- lift $ upsertPrologProgram newProgram
                               loopBrowse uid (Just newProg) False

                       else do lift $ $(logInfo) $ "Enter force save mode:" ++ T.pack (show newProgram)
                               loopBrowse uid (Just (Entity pid newProgram))  True

    Just Next ->   do nextProgram <- runCCtime $ lift $ selectNextUserProgram uid (Just pid)
                      runCCtime $ loopBrowse uid nextProgram False

    Just Prev ->  do prevProgram <- lift $ selectPrevUserProgram uid  (Just pid)
                     loopBrowse uid prevProgram False

    Just AddGoal -> do loopGoals  uid  entity
                       loopBrowse uid (Just entity) False

    Just Delete  -> do nextProgram <- lift $ selectNextUserProgram uid (Just pid)
                       lift $ deleteProgram pid
                       loopBrowse uid nextProgram False
    -- Just CheckSyntax -> do
    --   case (newName, newCode) of
    --     (Just name, Just (Textarea code)) -> case consultString $ T.unpack code of
    --       Left error -> do inquireParseError error name (Textarea code)
    --                        loopBrowse $ Just (progId, newProgram)

    --       Right _    -> do inquireParseSuccess name (Textarea code)
    --                        loopBrowse $ Just (progId, newProgram)
    --     _   -> loopBrowse (Just (progId, newProgram))

    _ -> loopBrowse uid (Just entity) forceSave


loopGoals :: UserId -> Entity PrologProgram ->  CC (PS Html) Handler ()
loopGoals  uid currentEntity@(Entity pid (PrologProgram uid'  name expl code)) = do
  goals <- lift $ selectUserProgramGoals uid 0 0 pid
  ( _klabel   , maybeAction   , resultForm )
              <- inquirePrologGoalEditor name (Textarea expl) (Textarea code) (map entityToVal goals)

  case resultForm of
    FormSuccess (PrologGoalForm newName (Textarea newExpl) (Textarea newCode))  -> do
      let newGoal = PrologGoal uid pid newName newExpl newCode
      entity <-  lift $ upsertPrologGoal newGoal
      loopGoals uid currentEntity

    _ -> do
      v <- lift $ lookupGetParam "action"
      case v of
        Just "back" -> loopBrowse uid (Just currentEntity) False
        _ ->           loopGoals uid currentEntity
  where
    entityToVal (Entity _ val) = val

-- A temporary version, NOT THREAD SAFE

module Handler.PrologProgram where

import             Import
import             Control.Monad.CC.CCCxe
import             ContMap
import             Data.Text(Text)
import qualified   Data.Text as T
import             Text.Read(reads)

------------------------------ Handlers ------------------------------

getPrologProgramR :: Handler Html
getPrologProgramR = run ccMain

postPrologProgramContR  :: Int -> Handler Html
postPrologProgramContR klabel = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html

postPrologProgramContPostR :: Handler Html
postPrologProgramContPostR = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]

  mklabel <- lookupPostParam "_klabel"
  case mklabel of
    Just  klabel'  ->
      case reads (T.unpack klabel') of
      [(klabel'' , _)] -> do
        resume klabel'' cont_html not_found_html
    _ -> do
      return not_found_html

------------------------------  Types --------------------------------

data PrologProgramAction = Cancel | Submit | New | Prev | Next | Delete | AddGoal | DeleteGoal
                deriving (Eq,Show)

type ProgramName = Text
type ProgramCode = Textarea

-------------------------- Program editor  --------------------------

data PrologProgramForm = PrologProgramForm { prologProgramName     :: Maybe Text
                                           , prologProgramProgram  :: Maybe Textarea
                                           } deriving Show


prologProgramForm :: Maybe ProgramName -> Maybe ProgramCode
                  -> Html -> MForm Handler (FormResult PrologProgramForm, Widget)
prologProgramForm name program = renderDivs $ PrologProgramForm
              <$> aopt textField      "Program Name:"  (Just name)
              <*> aopt textareaField  "Prolog Program:" (Just program)

prologProgramWidget :: ContId ->  Widget -> Enctype -> Widget
prologProgramWidget klabel formWidget enctype = do
  setTitle "View Prolog Program"
  $(widgetFile "prolog_program_editor")

prologProgramHtml :: Maybe ProgramName -> Maybe ProgramCode
                  ->  CC (PS Html) Handler (ContId, Html)
prologProgramHtml name program = do
  (klabel, formWidget, enctype) <- lift $ generateKFormPost $ prologProgramForm name program
  html   <- lift $ defaultLayout $ prologProgramWidget klabel  formWidget enctype
  return (klabel, html)

inquirePrologProgram :: Maybe ProgramName -> Maybe ProgramCode
                        -> CC (PS Html) Handler (ContId, Maybe PrologProgramAction, PrologProgramForm)
inquirePrologProgram name program  = do
  (klabel, html)         <- prologProgramHtml name program
  (answer, maybeAction)  <- inquirePostUntilButton klabel html (prologProgramForm name program)
                            [ ("submit", Submit) , ("next", Next) , ("prev" , Prev) , ("add_goal", AddGoal)
                            , ("delete", Delete)
                            ]
  return (klabel, maybeAction, answer)

-------------------------- Goal editor  --------------------------

data PrologGoalForm = PrologGoalForm { goalName   ::  Text
                                     , goalCode   ::  Textarea
                                     } deriving Show

prologGoalEditorForm ::  Html -> MForm Handler (FormResult PrologGoalForm, Widget)
prologGoalEditorForm = renderDivs $ PrologGoalForm
              <$> areq textField      "Goal Name:" Nothing
              <*> areq textareaField  "Goal Code:" Nothing

prologGoalEditorWidget :: ProgramName -> ProgramCode -> [PrologGoal] -> ContId ->  Widget -> Enctype
                       -> Widget
prologGoalEditorWidget programName programCode goals  klabel  formWidget enctype = do
  setTitle "Edit Prolog Goals"
  $(widgetFile "prolog_goal_editor")

prologGoalEditorHtml ::  ProgramName -> ProgramCode -> [PrologGoal]
                         -> CC (PS Html) Handler (ContId, Html)
prologGoalEditorHtml  name code goals = do
  (klabel, formWidget, enctype) <- lift $ generateKFormPost $ prologGoalEditorForm
  html  <- lift $ defaultLayout $ prologGoalEditorWidget name code goals klabel formWidget enctype
  return (klabel, html)

inquirePrologGoalEditor :: ProgramName -> ProgramCode -> [PrologGoal]
                        -> CC (PS Html) Handler (ContId, Maybe PrologProgramAction, PrologGoalForm)
inquirePrologGoalEditor name code goals  = do
  (klabel, html)         <- prologGoalEditorHtml name code goals
  (answer, maybeAction)  <- inquirePostUntilButton klabel html (prologGoalEditorForm)
                            [ ("submit", Submit) ]
  return (klabel, maybeAction, answer)

------------------------------  finish  ------------------------------
prologProgramFinishHtml :: CC (PS Html) Handler Html
prologProgramFinishHtml = lift $ redirect Portfolio01R

------------------------  Database functions  ------------------------
selectFirstProgram :: CC (PS Html) Handler (Maybe (PrologProgramId, PrologProgramForm))
selectFirstProgram = lift $ do
--  $(logInfo) "selectFirst"
  entity <- runDB $ selectList [] [Asc PrologProgramName , LimitTo 1 ]
  case entity of
    [ Entity id (PrologProgram name program) ]
        ->  return (Just (id, PrologProgramForm (Just name) (Just $ Textarea program) ))
    _   ->  return Nothing

selectLastProgram :: CC (PS Html) Handler (Maybe (PrologProgramId, PrologProgramForm))
selectLastProgram = lift $ do
--  $(logInfo) "selectLast"
  entity <- runDB $ selectList [] [Desc PrologProgramName , LimitTo 1 ]
  case entity of
    [ Entity id (PrologProgram name program) ]
        ->  return (Just (id,PrologProgramForm (Just name) (Just $ Textarea program)  ))
    _   ->  return Nothing


selectNextProgram :: PrologProgramForm -> CC (PS Html) Handler (Maybe (PrologProgramId, PrologProgramForm))
selectNextProgram (PrologProgramForm Nothing     body ) = selectFirstProgram
selectNextProgram (PrologProgramForm (Just name) body ) =  do
--  lift $ $(logInfo) "selectNext"
  nextProgram <- lift $ runDB $ selectList [PrologProgramName >. name] [LimitTo 1]
  case nextProgram of
    [Entity progId (PrologProgram name' program)]
      -> return $ Just $ (progId, PrologProgramForm (Just name') (Just (Textarea program)))
    _ -> selectFirstProgram

selectPrevProgram :: PrologProgramForm -> CC (PS Html) Handler (Maybe (PrologProgramId, PrologProgramForm))
selectPrevProgram (PrologProgramForm Nothing     body  ) = selectLastProgram
selectPrevProgram (PrologProgramForm (Just name) body  ) = do
--  lift $ $(logInfo) "selectPrev"
  nextProgram <- lift $ runDB $ selectList [PrologProgramName <. name] [LimitTo 1]
  case nextProgram of
    [Entity progId  (PrologProgram name' program)]
      -> return $ Just (progId, PrologProgramForm (Just name') (Just $ Textarea program))
    _ -> selectLastProgram

submit :: PrologProgramForm -> CC (PS Html) Handler (Maybe PrologProgramId)
submit (PrologProgramForm (Just name) (Just (Textarea body)) ) = lift $ do
  progId <- runDB $ insert $ PrologProgram name body
  prog   <- runDB $ get progId
  case prog of
    Just _  -> return (Just progId)
    Nothing -> return Nothing
submit (PrologProgramForm _ _ )            = return Nothing

submitGoal :: PrologProgramId -> PrologGoalForm -> CC (PS Html) Handler (Key PrologGoal)
submitGoal progId (PrologGoalForm name (Textarea code)) = do
  goalId <- lift $ runDB $ insert $ PrologGoal name progId code
  return goalId

selectGoalEntities :: PrologProgramId -> CC (PS Html) Handler [Entity PrologGoal]
selectGoalEntities progId = do
  goals  <- lift $ runDB $ selectList [ PrologGoalProgramId ==. progId ] []
  return goals

selectGoals :: PrologProgramId -> CC (PS Html) Handler [PrologGoal]
selectGoals progId = do
  goals <- selectGoalEntities progId
  return $ map toPrologGoalForm goals
    where
      toPrologGoalForm :: Entity PrologGoal -> PrologGoal
      toPrologGoalForm (Entity goalId prologGoal) = prologGoal

deleteProgram :: PrologProgramId -> CC (PS Html) Handler ()
deleteProgram progId = do
  goals <- selectGoalEntities progId
  mapM_ deleteGoalEntity goals
  lift $ runDB $ delete progId

deleteGoal :: PrologGoalId -> CC (PS Html) Handler ()
deleteGoal goalId = do
  lift $ runDB $ delete goalId

deleteGoalEntity :: Entity PrologGoal -> CC (PS Html) Handler ()
deleteGoalEntity (Entity goalId _goal) = deleteGoal goalId

------------------------  Application logics  ------------------------


ccMain :: CC (PS Html) Handler Html
ccMain =  do
  program <- selectFirstProgram
  loopBrowse program

  prologProgramFinishHtml >>=  inquireFinish

loopBrowse :: Maybe (PrologProgramId, PrologProgramForm) -> CC (PS Html) Handler ()

loopBrowse Nothing = do
  (_klabel, maybeAction, newProgram ) <- inquirePrologProgram  Nothing Nothing
  case maybeAction of
    Just Submit -> do
      success <- submit newProgram
      case success of
        Just newProgId ->  loopBrowse (Just (newProgId,newProgram))
        Nothing        ->  loopBrowse Nothing

    Just Next ->  do nextProgram <- selectFirstProgram
                     loopBrowse nextProgram

    Just Prev ->  do prevProgram <- selectLastProgram
                     loopBrowse prevProgram
    _ -> loopBrowse Nothing

loopBrowse (Just (progId, currentProgram@(PrologProgramForm name program ))) = do
  (_klabel, maybeAction, newProgram ) <- inquirePrologProgram  name  program
  case maybeAction of
    Just Submit -> do
      success <- submit newProgram
      case success of
        Just newProgId ->  loopBrowse (Just (newProgId, newProgram))
        Nothing        ->  loopBrowse (Just (progId   , currentProgram))

    Just Next ->  do nextProgram <- selectNextProgram currentProgram -- sort by name
                     loopBrowse nextProgram

    Just Prev ->  do prevProgram <- selectPrevProgram currentProgram
                     loopBrowse $ prevProgram

    Just AddGoal -> do loopGoals progId currentProgram

    Just Delete  -> do deleteProgram progId
                       loopBrowse Nothing

    _ -> loopBrowse (Just (progId,currentProgram))


loopGoals :: PrologProgramId -> PrologProgramForm -> CC (PS Html) Handler ()
loopGoals  progId currentProgram@(PrologProgramForm (Just name) (Just code)) = do
  goals <- selectGoals progId
  (_klabel, maybeAction, newGoal) <- inquirePrologGoalEditor name code goals
  case maybeAction of
    Just Submit -> do _ <- submitGoal progId newGoal
                      loopGoals progId currentProgram

    _           -> loopGoals progId currentProgram

loopGoals progId _ = loopBrowse Nothing

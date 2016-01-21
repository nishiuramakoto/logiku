-- A temporary version, NOT THREAD SAFE

module Handler.PrologProgram where

import             Import
import             Control.Monad.CC.CCCxe
import             ContMap

------------------------------ Handlers ------------------------------

getPrologProgramR :: Handler Html
getPrologProgramR = run ccMain

postPrologProgramContR  :: Int -> Handler Html
postPrologProgramContR klabel = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html

------------------------------  Types --------------------------------

data PrologProgramAction = Cancel | Submit | New | Prev | Next | AddGoal
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
  $(logInfo) "selectFirst"
  entity <- runDB $ selectList [] [Asc PrologProgramName , LimitTo 1 ]
  case entity of
    [ Entity id (PrologProgram name program) ]
        ->  return (Just (id, PrologProgramForm (Just name) (Just $ Textarea program) ))
    _   ->  return Nothing

selectLastProgram :: CC (PS Html) Handler (Maybe (PrologProgramId, PrologProgramForm))
selectLastProgram = lift $ do
  $(logInfo) "selectLast"
  entity <- runDB $ selectList [] [Desc PrologProgramName , LimitTo 1 ]
  case entity of
    [ Entity id (PrologProgram name program) ]
        ->  return (Just (id,PrologProgramForm (Just name) (Just $ Textarea program)  ))
    _   ->  return Nothing


selectNextProgram :: PrologProgramForm -> CC (PS Html) Handler (Maybe (PrologProgramId, PrologProgramForm))
selectNextProgram (PrologProgramForm Nothing     body ) = selectFirstProgram
selectNextProgram (PrologProgramForm (Just name) body ) =  do
  lift $ $(logInfo) "selectNext"
  nextProgram <- lift $ runDB $ selectList [PrologProgramName >. name] [LimitTo 1]
  case nextProgram of
    [Entity id (PrologProgram name' program)]
      -> return $ Just $ (id, PrologProgramForm (Just name') (Just (Textarea program)))
    _ -> selectFirstProgram

selectPrevProgram :: PrologProgramForm -> CC (PS Html) Handler (Maybe (PrologProgramId, PrologProgramForm))
selectPrevProgram (PrologProgramForm Nothing     body  ) = selectLastProgram
selectPrevProgram (PrologProgramForm (Just name) body  ) = do
  lift $ $(logInfo) "selectPrev"
  nextProgram <- lift $ runDB $ selectList [PrologProgramName <. name] [LimitTo 1]
  case nextProgram of
    [Entity id  (PrologProgram name' program)]
      -> return $ Just (id, PrologProgramForm (Just name') (Just $ Textarea program))
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
submitGoal id (PrologGoalForm name (Textarea code)) = do
  goalId <- lift $ runDB $ insert $ PrologGoal name id code
  return goalId

selectGoals :: PrologProgramId -> CC (PS Html) Handler [PrologGoal]
selectGoals id = do
  goals <- lift $ runDB $ selectList [ PrologGoalProgramId ==. id ] []
  return $ map toPrologGoalForm goals
    where
      toPrologGoalForm :: Entity PrologGoal -> PrologGoal
      toPrologGoalForm (Entity goalId prologGoal) = prologGoal

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

    _ -> loopBrowse (Just (progId,currentProgram))


loopGoals :: PrologProgramId -> PrologProgramForm -> CC (PS Html) Handler ()
loopGoals  id currentProgram@(PrologProgramForm (Just name) (Just code)) = do
  goals <- selectGoals id
  (_klabel, maybeAction, newGoal) <- inquirePrologGoalEditor name code goals
  case maybeAction of
    Just Submit -> do submitGoal id newGoal
                      loopGoals id currentProgram

    Nothing     -> loopGoals id currentProgram

loopGoals id _ = loopBrowse Nothing

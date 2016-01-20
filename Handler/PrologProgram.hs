-- A temporary version, NOT THREAD SAFE

module Handler.PrologProgram (
  getPrologProgramR,
  postPrologProgramContR
  )  where

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

data PrologProgramAction = Cancel | Submit | New | Run | Prev | Next | Delete
                deriving (Eq,Show)

type ProgramName = Text
type ProgramBody = Textarea

-------------------------- inquire program  --------------------------

data PrologProgramForm = PrologProgramForm { prologProgramName     :: Text
                                           , prologProgramProgram  :: Textarea
                                           } deriving Show


prologProgramForm :: Maybe ProgramName -> Maybe ProgramBody
                  -> Html -> MForm Handler (FormResult PrologProgramForm, Widget)
prologProgramForm name program = renderDivs $ PrologProgramForm
              <$> areq textField     "Program Name:"  name
              <*> areq textareaField "Prolog Program" program

prologProgramWidget :: ContId ->  Widget -> Enctype -> Widget
prologProgramWidget klabel formWidget enctype = do
  setTitle "View Prolog Program"
  $(widgetFile "prolog_program")

prologProgramHtml :: Maybe ProgramName -> Maybe ProgramBody
                  ->  CC (PS Html) Handler (ContId, Html)
prologProgramHtml name program = do
  (klabel, formWidget, enctype) <- lift $ generateKFormPost $ prologProgramForm name program
  html   <- lift $ defaultLayout $ prologProgramWidget klabel  formWidget enctype
  return (klabel, html)

inquirePrologProgram :: Maybe ProgramName -> Maybe ProgramBody
                        -> CC (PS Html) Handler (ContId, Maybe PrologProgramAction, PrologProgramForm)
inquirePrologProgram name program  = do
  (klabel, html)         <- prologProgramHtml name program
  (answer, maybeAction)  <- inquirePostUntilButton klabel html (prologProgramForm name program)
                            [("submit", Submit) , ("next", Next) , ("prev" , Prev) ]
  return (klabel, maybeAction, answer)

------------------------------  finish  ------------------------------
prologProgramFinishHtml :: CC (PS Html) Handler Html
prologProgramFinishHtml = lift $ redirect Portfolio01R

------------------------  Database functions  ------------------------
selectFirstProgram :: CC (PS Html) Handler (Maybe PrologProgramForm)
selectFirstProgram = lift $ do
  entity <- runDB $ selectList [] [Asc PrologProgramName , LimitTo 1 ]
  case entity of
    [ Entity id (PrologProgram name program) ] ->  return (Just (PrologProgramForm name (Textarea program)))
    _                       ->  return Nothing


selectNextProgram :: PrologProgramForm -> CC (PS Html) Handler (Maybe PrologProgramForm)
selectNextProgram (PrologProgramForm name body) = lift $ do
  nextProgram <- runDB $ selectList [PrologProgramName >. name] [LimitTo 1]
  case nextProgram of
    [Entity id (PrologProgram name program)] -> return $ Just $ PrologProgramForm name (Textarea program)
    _ ->                     return Nothing

selectPrevProgram :: PrologProgramForm -> CC (PS Html) Handler (Maybe PrologProgramForm)
selectPrevProgram (PrologProgramForm name body) = lift $ do
  nextProgram <- runDB $ selectList [PrologProgramName <. name] [LimitTo 1]
  case nextProgram of
    [Entity id  (PrologProgram name program)] -> return $ Just $ PrologProgramForm name (Textarea program)
    _ ->                     return Nothing

submit :: PrologProgramForm -> CC (PS Html) Handler Bool
submit (PrologProgramForm name (Textarea body)) = lift $ do
  progId <- runDB $ insert $ PrologProgram name body
  prog   <- runDB $ get progId
  case prog of
    Just _  -> return True
    Nothing -> return False

------------------------  Application logics  ------------------------


ccMain :: CC (PS Html) Handler Html
ccMain =  do
  program <- selectFirstProgram
  loopBrowse program

  prologProgramFinishHtml >>=  inquireFinish

loopBrowse :: Maybe PrologProgramForm -> CC (PS Html) Handler ()

loopBrowse Nothing = do
  (_klabel, maybeAction, newProgram ) <- inquirePrologProgram  Nothing Nothing
  case maybeAction of
    Just Submit -> do
      success <- submit newProgram
      if success
        then loopBrowse (Just newProgram)
        else loopBrowse Nothing
    _ -> loopBrowse Nothing

loopBrowse (Just currentProgram@(PrologProgramForm name program)) = do
  (_klabel, maybeAction, newProgram ) <- inquirePrologProgram  (Just name) (Just program)
  case maybeAction of
    Just Submit -> do
      success <- submit newProgram
      if success
        then loopBrowse (Just newProgram)
        else loopBrowse (Just currentProgram)

    Just Next ->  do nextProgram <- selectNextProgram currentProgram
                     loopBrowse nextProgram

    Just Prev ->  do prevProgram <- selectPrevProgram currentProgram
                     loopBrowse prevProgram


    _ -> loopBrowse (Just currentProgram)

module Handler.PrologTest (
  prologTestWidget,
  prologTestForm,
  getPrologTestR,
  postPrologExecuteTestR,
  getPrologExecuteTestContR,
  categoryTree,
  prologExecuteCcMain,
  prologExecuteTestFinishHtml
  ) where

import Import hiding(parseQuery,catch)
import Prolog(resolve,consultString,parseQuery)
import qualified Data.Text as T
import ContMap
import Control.Monad.CC.CCCxe
import Prolog.Syntax
import Prolog.Unifier


data PrologTestForm = PrologTestForm
                      { program :: Textarea
                      , goal    :: Textarea
                      } deriving Show

prologTestWidget :: Widget -> Enctype -> Widget
prologTestWidget prolog_execute_test_widget _enctype = $(widgetFile "prolog_test")

prologTestForm :: Html -> MForm Handler (FormResult PrologTestForm, Widget)
prologTestForm = renderDivs $ PrologTestForm
                 <$> areq textareaField "Program:" Nothing
                 <*> areq textareaField "Goal:   " Nothing

getPrologTestR :: Handler Html
getPrologTestR = do
  (widget, enctype) <- generateFormPost prologTestForm
  defaultLayout $ prologTestWidget widget enctype

executePrologProgram :: Text -> Text -> Handler Html
executePrologProgram progCode goalCode =
  case (consultString (T.unpack progCode), parseQuery (T.unpack goalCode)) of
  (Right clauses, Right terms)   -> run $ prologExecuteCcMain clauses terms

  (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in the program #{show err}|]
  (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in the goals   #{show err}|]


postPrologExecuteTestR :: Handler Html
postPrologExecuteTestR = do
  ((result, _widget), _enctype) <- runFormPost prologTestForm
  case result of
    FormSuccess (PrologTestForm (Textarea program) (Textarea goal)) ->
      executePrologProgram program goal
    FormMissing     -> defaultLayout $ [whamlet| Form Missing|]
    FormFailure err -> defaultLayout $ [whamlet| Form failure   #{show err}|]

getPrologExecuteTestContR :: Int -> Handler Html
getPrologExecuteTestContR klabel = do
  cont_html      <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html




categoryTree :: Widget
categoryTree =  toWidget $(widgetFile "css-tree")


prologExecuteTestFinishHtml :: [Unifier] -> CC (PS Html) Handler Html
prologExecuteTestFinishHtml unifiers =
  lift $ defaultLayout $ [whamlet| #{show unifiers}|]

prologExecuteCcMain :: Program -> [Goal] -> CC (PS Html) Handler Html
prologExecuteCcMain program goals = do
  unifiers <- resolve program goals
  prologExecuteTestFinishHtml unifiers >>= inquireFinish

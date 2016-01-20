module Handler.PrologTest where

import Import hiding(parseQuery,catch)
import Prolog(resolve,consultString,parseQuery)
import qualified Data.Text as T


data PrologTestForm = PrologTestForm
                      { program :: Textarea
                      , goal    :: Textarea
                      } deriving Show

prologTestWidget :: Widget -> Enctype -> Widget
prologTestWidget prolog_execute_test_widget enctype = $(widgetFile "prolog_test")

prologTestForm :: Html -> MForm Handler (FormResult PrologTestForm, Widget)
prologTestForm = renderDivs $ PrologTestForm
                 <$> areq textareaField "Program:" Nothing
                 <*> areq textareaField "Goal:   " Nothing

getPrologTestR :: Handler Html
getPrologTestR = do
  (widget, enctype) <- generateFormPost prologTestForm
  defaultLayout $ prologTestWidget widget enctype

postPrologExecuteTestR :: Handler Html
postPrologExecuteTestR = do
  ((result, widget), enctype) <- runFormPost prologTestForm
  case result of
    FormSuccess (PrologTestForm (Textarea program) (Textarea goal)) ->
      case (consultString (T.unpack program), parseQuery (T.unpack goal)) of
      (Right clauses, Right terms)   -> do
        unifiers <- resolve clauses terms
        defaultLayout $ [whamlet| #{show unifiers}|]
      (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in the program #{show err}|]
      (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in the goals   #{show err}|]
    FormMissing     -> defaultLayout $ [whamlet| Form Missing|]
    FormFailure err -> defaultLayout $ [whamlet| Form failure   #{show err}|]


categoryTree :: Widget
categoryTree =  toWidget $(widgetFile "css-tree")

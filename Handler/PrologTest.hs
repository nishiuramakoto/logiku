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
import Language.Prolog2(resolveToTerms,consultString,parseQuery,evalPrologT,RuntimeError)
import Language.Prolog2.Syntax

import qualified Data.Text as T
import ContMap
import Control.Monad.CC.CCCxe


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
  run $ prologExecuteCcMain progCode goalCode

  -- case (consultString (T.unpack progCode), parseQuery (T.unpack goalCode)) of
  -- (Right clauses, Right terms)   -> run $ prologExecuteCcMain clauses terms

  -- (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in the program #{show err}|]
  -- (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in the goals   #{show err}|]


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


prologExecuteTestFinishHtml :: [[Term]] -> CC (PS Html) Handler Html
prologExecuteTestFinishHtml unifiers =
  lift $ defaultLayout $ [whamlet| #{show unifiers}|]

prologExecuteTestSyntaxErrorHtml :: String -> CC (PS Html) Handler Html
prologExecuteTestSyntaxErrorHtml error =
  lift $ defaultLayout $ [whamlet| #{show error}|]

prologExecuteTestRuntimeErrorHtml :: RuntimeError -> CC (PS Html) Handler Html
prologExecuteTestRuntimeErrorHtml error =
  lift $ defaultLayout $ [whamlet| #{show error}|]

prologExecuteCcMain :: Text -> Text -> CC (PS Html) Handler Html
prologExecuteCcMain progCode goalCode = do
   result <- evalPrologT $ do
        eprog <- consultString (T.unpack progCode)
        case eprog of
          Left  err  -> return $ Left $ "syntax error in program:" ++ show err
          Right prog -> do egoal <- parseQuery (T.unpack goalCode)
                           case egoal of
                             Left  err -> return $ Left $ "syntax error in query:" ++ show err
                             Right goal -> do Right <$>  resolveToTerms prog goal

   case result of
    Left  err         ->  prologExecuteTestRuntimeErrorHtml err >>= inquireFinish
    Right (Left err)  ->  prologExecuteTestSyntaxErrorHtml err >>= inquireFinish
    Right (Right tss) ->  prologExecuteTestFinishHtml tss >>= inquireFinish

module Handler.PrologTest (
  prologTestWidget,
  prologTestForm,
  getPrologTestR,
  postPrologExecuteTestR,
  getPrologExecuteTestContR,
  categoryTree,
  prologExecuteCCMain,
  prologExecuteTestFinishHtml
  ) where

import Import hiding(parseQuery,catch,Form)
import Language.Prolog2(resolveToTerms,consultString,parseQuery,evalPrologT,RuntimeError)
import Language.Prolog2.Syntax

import qualified Data.Text as T
import CCGraph
import Form
import Control.Monad.CC.CCCxe


data PrologTestForm = PrologTestForm
                      { _program :: Textarea
                      , _goal    :: Textarea
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

executePrologProgram :: CCState -> Text -> Text -> Handler Html
executePrologProgram st progCode goalCode = do
  CCTypeHtml html <- run $ prologExecuteCCMain st progCode goalCode
  return html

  -- case (consultString (T.unpack progCode), parseQuery (T.unpack goalCode)) of
  -- (Right clauses, Right terms)   -> run $ prologExecuteCCMain clauses terms

  -- (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in the program #{show err}|]
  -- (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in the goals   #{show err}|]


postPrologExecuteTestR :: Handler Html
postPrologExecuteTestR = do
  ((result, _widget), _enctype) <- runFormPost prologTestForm
  st <- startState

  case result of
    FormSuccess (PrologTestForm (Textarea program) (Textarea goal)) ->
      executePrologProgram st program goal
    FormMissing     -> defaultLayout $ [whamlet| Form Missing|]
    FormFailure err -> defaultLayout $ [whamlet| Form failure   #{show err}|]

getPrologExecuteTestContR :: CCNode -> Handler Html
getPrologExecuteTestContR node = do
  not_found_html <- defaultLayout [whamlet|PrologExecuteTestContR: cont not found|]
  CCTypeHtml html <- resume (node, const $ return $ CCTypeHtml not_found_html) (CCTypeHtml not_found_html)
  return html


categoryTree :: Widget
categoryTree =  toWidget $(widgetFile "css-tree")


prologExecuteTestFinishHtml :: [[Term]] -> CC CCP Handler Html
prologExecuteTestFinishHtml unifiers =
  lift $ defaultLayout $ [whamlet| #{show unifiers}|]

prologExecuteTestSyntaxErrorHtml :: String -> CC CCP Handler Html
prologExecuteTestSyntaxErrorHtml err =
  lift $ defaultLayout $ [whamlet| #{show err}|]

prologExecuteTestRuntimeErrorHtml :: RuntimeError -> CC CCP Handler Html
prologExecuteTestRuntimeErrorHtml err =
  lift $ defaultLayout $ [whamlet| #{show err}|]

prologExecuteCCMain :: CCState -> Text -> Text -> CC CCP Handler CCContentType
prologExecuteCCMain st progCode goalCode = do
   result <- evalPrologT $ do
        eprog <- consultString (T.unpack progCode)
        case eprog of
          Left  err  -> return $ Left $ "syntax error in program:" ++ show err
          Right prog -> do egoal <- parseQuery (T.unpack goalCode)
                           case egoal of
                             Left  err -> return $ Left $ "syntax error in query:" ++ show err
                             Right goal -> do Right <$>  resolveToTerms st prog goal

   case result of
    Left  err         ->  (CCTypeHtml <$> prologExecuteTestRuntimeErrorHtml err) >>= inquireFinish
    Right (Left err)  ->  (CCTypeHtml <$> prologExecuteTestSyntaxErrorHtml err) >>= inquireFinish
    Right (Right tss) ->  (CCTypeHtml <$> prologExecuteTestFinishHtml tss) >>= inquireFinish

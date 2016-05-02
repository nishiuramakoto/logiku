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
import Language.Prolog2(resolveToTerms,consultString,parseQuery,evalPrologT,RuntimeError, ParseError)
import Language.Prolog2.Syntax
import Language.Prolog2.Types

import Control.Monad.Trans.Either
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

executePrologProgram :: CCState -> ModuleName -> Text -> Text -> Handler Html
executePrologProgram st mod progCode goalCode = do
  (Right (CCContentHtml html) , _) <- runWithBuiltins $ prologExecuteCCMain st mod progCode goalCode
  return html

  -- case (consultString (T.unpack progCode), parseQuery (T.unpack goalCode)) of
  -- (Right clauses, Right terms)   -> run $ prologExecuteCCMain clauses terms

  -- (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in the program #{show err}|]
  -- (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in the goals   #{show err}|]


postPrologExecuteTestR :: Handler Html
postPrologExecuteTestR = do
  ((result, _widget), _enctype) <- runFormPost prologTestForm
  st <- startState "プロログ実行スタート"

  let mod = "main"
  case result of
    FormSuccess (PrologTestForm (Textarea program) (Textarea goal)) ->
      executePrologProgram st mod program goal
    FormMissing     -> defaultLayout $ [whamlet| Form Missing|]
    FormFailure err -> defaultLayout $ [whamlet| Form failure   #{show err}|]

getPrologExecuteTestContR :: CCNode -> Handler Html
getPrologExecuteTestContR node = do
  (a, binding) <- resume =<< getFormMissingState node
  case a of
    Right (CCContentHtml html) -> return html
    Right (CCContentJson value) -> do
      $logInfo $ T.pack $ show value
      notFound
    Left err -> do
      $logInfo $ T.pack $ show err
      notFound

categoryTree :: Widget
categoryTree =  toWidget $(widgetFile "css-tree")


prologExecuteTestFinishHtml :: [[Term]] -> CCPrologHandler Html
prologExecuteTestFinishHtml unifiers =
  lift $ defaultLayout $ [whamlet| Finished with #{show unifiers}|]

prologExecuteTestSyntaxErrorHtml :: ParseError -> CCPrologHandler Html
prologExecuteTestSyntaxErrorHtml err =
  lift $ defaultLayout $ [whamlet| Syntax error: #{show err}|]

prologExecuteTestRuntimeErrorHtml :: RuntimeError -> CCPrologHandler Html
prologExecuteTestRuntimeErrorHtml err =
  lift $ defaultLayout $ [whamlet| Runtime error: #{show err}|]

prologExecuteCCMain :: CCState -> ModuleName -> Text -> Text -> CCPrologHandler CCContentType
prologExecuteCCMain st mod progCode goalCode = do
   result <- runEitherT $ do
     prog <- EitherT $ liftProlog $ consultString (T.unpack progCode)
     goal <- EitherT $ liftProlog $ parseQuery (T.unpack goalCode)
     lift $ resolveToTerms st mod prog goal

   case result of
     Left  err ->  (CCContentHtml <$> prologExecuteTestSyntaxErrorHtml err) >>= inquireFinish
     Right tss ->  (CCContentHtml <$> prologExecuteTestFinishHtml tss) >>= inquireFinish

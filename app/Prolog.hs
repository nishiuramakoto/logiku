module Prolog (
  eitherNotFound,
  programCheck,
  goalCheck,
  prologExecuteCcMain
  ) where

import             Import hiding (parseQuery,readFile)
import             Language.Prolog2
import             Control.Monad.CC.CCCxe
import             Control.Monad.Trans.Either
import qualified   Data.Text as T
import             DBFS
import             CCGraph


type ProgramError = Either RuntimeError ParseError

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


eitherNotFound :: EitherT DbfsError Handler a -> Handler a
eitherNotFound body = do e <- runEitherT body
                         case e of
                           Right html -> return html
                           Left err   -> do setMessage $ toHtml (T.pack $ show err)
                                            notFound


prologExecuteTestFinishHtml :: [[Term]] -> CC (PS Html) Handler Html
prologExecuteTestFinishHtml unifiers =
  lift $ defaultLayout $ [whamlet| #{show unifiers}|]

prologExecuteTestSyntaxErrorHtml :: ParseError -> CC (PS Html) Handler Html
prologExecuteTestSyntaxErrorHtml err =
  lift $ defaultLayout $ [whamlet| #{show err}|]

prologExecuteTestRuntimeErrorHtml :: RuntimeError -> CC (PS Html) Handler Html
prologExecuteTestRuntimeErrorHtml err =
  lift $ defaultLayout $ [whamlet| #{show err}|]



prologExecuteCcMain :: Text -> Text -> CC (PS Html) Handler Html
prologExecuteCcMain progCode goalCode = do
   result <- evalPrologT $ runEitherT $ do
        prog <- EitherT $ consultString (T.unpack progCode)
        goal <- EitherT $ parseQuery (T.unpack goalCode)
        lift $ resolveToTerms prog goal

   case result of
    Left  err          ->  prologExecuteTestRuntimeErrorHtml err >>= inquireFinish
    Right (Left err)   ->  prologExecuteTestSyntaxErrorHtml err >>= inquireFinish
    Right (Right tss)  ->  prologExecuteTestFinishHtml tss >>= inquireFinish

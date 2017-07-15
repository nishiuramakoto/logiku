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

programCheck' :: Text -> Either RuntimeError (Either ParseError Database)
programCheck' text = database $ runIdentity $ evalPrologT $ consultText text

programCheck :: Text -> Either ProgramError Database
programCheck text = either3iso $ programCheck' text

goalCheck' :: Text -> Either RuntimeError (Either ParseError [Goal])
goalCheck' text = runIdentity $ evalPrologT $ parseQuery  text

goalCheck :: Text -> Either ProgramError [Goal]
goalCheck text = either3iso $ goalCheck' text


eitherNotFound :: EitherT DbfsError Handler a -> Handler a
eitherNotFound body = do e <- runEitherT body
                         case e of
                           Right html -> return html
                           Left err   -> do setMessage $ toHtml (T.pack $ show err)
                                            notFound


prologExecuteTestFinishHtml :: [[Term]] -> CCPrologHandler  Html
prologExecuteTestFinishHtml unifiers =
  lift $ defaultLayout $ [whamlet| #{show unifiers}|]

prologExecuteTestSyntaxErrorHtml :: ParseError -> CCPrologHandler  Html
prologExecuteTestSyntaxErrorHtml err =
  lift $ defaultLayout $ [whamlet| #{show err}|]

prologExecuteTestRuntimeErrorHtml :: RuntimeError -> CCPrologHandler  Html
prologExecuteTestRuntimeErrorHtml err =
  lift $ defaultLayout $ [whamlet| #{show err}|]



prologExecuteCcMain :: CCState -> ModuleName -> Text -> Text -> CCPrologHandler  CCContentType
prologExecuteCcMain st mod progCode goalCode = do
  result <- runEitherT $ do
    db   <- EitherT $ liftProlog $ consultText  progCode
    goal <- EitherT $ liftProlog $ parseQuery   goalCode
    sysdb <- lift $ liftProlog $ createSysDB
    lift $ resolveToTerms st mod  goal db sysdb

  case result of
    (Left err)   ->  (CCContentHtml <$> prologExecuteTestSyntaxErrorHtml err) >>= inquireFinish
    (Right tss)  ->  (CCContentHtml <$> prologExecuteTestFinishHtml tss) >>= inquireFinish

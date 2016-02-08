module Handler.PrologResolver (
  getPrologResolverR,
  getPrologResolverContR
  ) where

import Import
import Control.Monad.CC.CCCxe
import Prolog.Interpreter(resolve)

------------------------------ Handlers ------------------------------
getPrologResolverR :: Handler Html
getPrologResolverR = do
  inquireMain

getPrologResolverContR :: Int -> Handler Html
getPrologResolverContR klabel = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html

------------------------  Application logic --------------------------


inquireMain :: Program -> [Goal] -> CC (PS Html) Handler Html
inquireMain  program goals = do
  loopInquire program goal

loopInquire :: Program -> [Goal] -> CC (PS Html) Handler Html
loopInquire program goals = do
  (klabel, form) <- resolve program goals
  inquireFinish $ defaultLayout [whamlet|Finished|]

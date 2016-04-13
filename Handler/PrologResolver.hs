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
  resume klabel

------------------------  Application logic --------------------------


inquireMain :: Program -> [Goal] -> CC (PS Html) Handler Html
inquireMain  program goals = do
  loopInquire program goal

loopInquire :: Program -> [Goal] -> CC (PS Html) Handler Html
loopInquire program goals = do
  (klabel, form) <- resolve program goals
  inquireFinish $ defaultLayout [whamlet|Finished|]

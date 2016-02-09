module Prolog.Inquire (
  PrologInquireBoolForm(..),
  inquirePrologBool
  )
  where

import Import
import ContMap
import Control.Monad.CC.CCCxe
import Prolog.Syntax

-------------------------- Primitive forms  --------------------------

data PrologInquireBoolForm = PrologInquireBoolForm Bool
                             deriving Show

prologInquireBoolForm :: Term -> Html -> MForm Handler (FormResult PrologInquireBoolForm, Widget)
prologInquireBoolForm t = renderDivs $ PrologInquireBoolForm
                         <$> areq boolField (fromString $ show t) Nothing

prologInquireBoolWidget ::  ContId -> Widget -> Enctype -> Widget
prologInquireBoolWidget klabel formWidget _enctype = do
  [whamlet| <form action=@{PrologExecuteTestContR klabel} method="GET">
              ^{formWidget}
              <button type="submit" value="Submit">Submit</button>
  |]


prologInquireBoolHtml :: Term -> CC (PS Html) Handler (ContId, Html)
prologInquireBoolHtml t = do
  (klabel, formWidget, enctype) <- lift $ generateCcFormGet $ prologInquireBoolForm t
  html <- lift $ defaultLayout $ prologInquireBoolWidget klabel formWidget enctype
  return (klabel, html)

inquirePrologBool :: Term -> CC (PS Html) Handler (ContId, FormResult (PrologInquireBoolForm))
inquirePrologBool t = do
  (klabel, html) <- prologInquireBoolHtml  t
  answer         <- inquireGet klabel html (prologInquireBoolForm t)
  return (klabel, answer)

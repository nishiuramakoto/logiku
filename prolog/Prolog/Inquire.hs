module Prolog.Inquire (
  PrologInquireBoolForm(..),
  inquirePrologBool
  )
  where

import Import
import ContMap
import Control.Monad.CC.CCCxe


-------------------------- Primitive forms  --------------------------

data PrologInquireBoolForm = PrologInquireBoolForm Bool
                             deriving Show

prologInquireBoolForm :: Html -> MForm Handler (FormResult PrologInquireBoolForm, Widget)
prologInquireBoolForm  = renderDivs $ PrologInquireBoolForm
                         <$> areq boolField "Yes or No: " Nothing

prologInquireBoolWidget :: ContId -> Widget -> Enctype -> Widget
prologInquireBoolWidget klabel formWidget _enctype = do
  [whamlet| <form action=@{PrologExecuteTestContR klabel} method="GET">
              ^{formWidget}
              <button type="submit" value="Submit">Submit</button>
  |]


prologInquireBoolHtml :: CC (PS Html) Handler (ContId, Html)
prologInquireBoolHtml = do
  (klabel, formWidget, enctype) <- lift $ generateCcFormGet $ prologInquireBoolForm
  html <- lift $ defaultLayout $ prologInquireBoolWidget klabel formWidget enctype
  return (klabel, html)

inquirePrologBool :: CC (PS Html) Handler (ContId, FormResult (PrologInquireBoolForm))
inquirePrologBool = do
  (klabel, html) <- prologInquireBoolHtml
  answer         <- inquireGet klabel html prologInquireBoolForm
  return (klabel, answer)

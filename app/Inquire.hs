module Inquire (
  PrologInquireBoolForm(..),
  inquirePrologBool
  )
  where

import Import
import CCGraph
import Form
import Control.Monad.CC.CCCxe
import Language.Prolog2.Syntax


import qualified Data.Text as T


prologInquireBoolForm :: Term -> Html -> MForm (HandlerT site IO) (FormResult PrologInquireBoolForm, Widget)
prologInquireBoolForm t = renderDivs $ PrologInquireBoolForm
                          <$> areq boolField (fromString $ showU t) Nothing
--                         <$> areq boolField (fromString $ show t) Nothing

prologInquireBoolWidget ::  CCData -> Widget -> Enctype -> Widget
prologInquireBoolWidget ccdata formWidget _enctype = do
  let klabel = ccLabel ccdata
  [whamlet| <form action=@{PrologExecuteTestContR klabel} method="GET">
              ^{formWidget}
              <button type="submit" value="Submit">Submit</button>
  |]


prologInquireBoolHtml :: CCData -> Term -> CC (PS Html) (HandlerT site IO) (CCData, Html)
prologInquireBoolHtml ccdata t = do
  (ccdata', formWidget, enctype) <- lift $ generateCCFormGet ccdata (prologInquireBoolForm t)
  html <- lift $ defaultLayout $ prologInquireBoolWidget ccdata' formWidget enctype
  return (ccdata', html)

inquirePrologBool :: CCData -> Term
                     -> CC (PS Html) (HandlerT site IO) (CCData, FormResult (PrologInquireBoolForm))
inquirePrologBool  ccdata t = do
  (ccdata', html) <- prologInquireBoolHtml  t ccdata
  answer          <- inquireGet ccdata' html (prologInquireBoolForm t)
  return (update ccdata' answer , answer)



showU :: Term -> String
showU (UTerm (TStruct a [])) = T.unpack a
showU t =  show t

module Inquire (
  PrologInquireBoolForm(..),
  inquirePrologBool
  )
  where

import Import hiding(Form)
import CCGraph
import Form
import Control.Monad.CC.CCCxe
import Language.Prolog2.Syntax
import Authentication

import qualified Data.Text as T


breadcrumbWidget :: CCState -> Widget
breadcrumbWidget st@(CCState node _) = do
  path' <- handlerToWidget $ spine node
  uid   <- handlerToWidget $ getUserAccountId

  case path' of
    [] -> [whamlet||]
    (root,_,_):_ ->
      [whamlet| <nav class="breadcrumb">
                   <a href="@{PrologExecuteTestContR root}">
                      #{root}
                   $forall (node',node,la) <- path'
                       &gt;
                       <a href="@{PrologExecuteTestContR node}">
                           #{node}
  |]



data PrologInquireBoolForm = PrologInquireBoolForm Bool
                             deriving (Eq,Ord,Show, Typeable)

prologInquireBoolForm :: Term -> Html
                         -> MForm Handler (FormResult PrologInquireBoolForm, Widget)
prologInquireBoolForm t = renderDivs $ PrologInquireBoolForm
                          <$> areq boolField (fromString $ showU t) Nothing
--                         <$> areq boolField (fromString $ show t) Nothing

prologInquireBoolWidget ::  CCState -> CCNode -> Widget -> Enctype -> Widget
prologInquireBoolWidget st node formWidget _enctype = do
  uid <- handlerToWidget $ getUserAccountId

  [whamlet| ^{breadcrumbWidget st}
            <form action=@{PrologExecuteTestContR node} method="GET">
              ^{formWidget}
              <button type="submit" value="Submit">Submit</button>
  |]


prologInquireBoolHtml :: CCState ->  Term -> CCContentTypeM App
prologInquireBoolHtml st t node = do
  (formWidget, enctype) <- lift $ generateCCFormGet (prologInquireBoolForm t)
  CCContentHtml <$> (lift $ defaultLayout $ prologInquireBoolWidget st node formWidget enctype)

inquirePrologBool :: CCState -> Term -> CC CCP Handler CCState
inquirePrologBool  st t = do
  inquireGet st (prologInquireBoolHtml st t) (prologInquireBoolForm t)


showU :: Term -> String
showU (UTerm (TStruct a [])) = T.unpack a
showU t =  show t

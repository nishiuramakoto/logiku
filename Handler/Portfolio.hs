module Handler.Portfolio where

import  Import
import  Control.Monad.CC.CCCxe
import  ContMap
import  LogicT.SRReifT


getPortfolio01R :: Handler Html
getPortfolio01R = do

  defaultLayout $ do
    addStylesheet $ StaticR  css_normalize_css
    setTitle "水廻りの疑問"
    toWidget $(widgetFile "01")

getLeakInBathroomR :: Handler Html
getLeakInBathroomR = run leak_in_bathroom'

postLeakInBathroomContR  :: Int -> Handler Html
postLeakInBathroomContR klabel = defaultResume klabel

defaultResume  :: Int -> Handler Html
defaultResume klabel = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html


leak_in_bathroom' :: CC (PS Html) Handler Html
leak_in_bathroom' = do
  b <- observe leak_in_bathroom
  lift $ defaultLayout $ toWidget $
    if b
    then  $(widgetFile "leak-in-bathroom-yes")
    else  $(widgetFile "leak-in-bathroom-no")


getLeakInKitchenR :: Handler Html
getLeakInKitchenR = run leak_in_kitchen'

postLeakInKitchenContR  :: Int -> Handler Html
postLeakInKitchenContR klabel = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html

leak_in_kitchen' :: CC (PS Html) Handler Html
leak_in_kitchen' = do
  b <- observe leak_in_kitchen
  lift $ defaultLayout $ toWidget $
    if b
    then  $(widgetFile "leak-in-kitchen-yes")
    else  $(widgetFile "leak-in-kitchen-no")


------------------------------  Domain Logic --------------------------------
type Logic a = SG (CC (PS Html) Handler) a
leak_in_bathroom :: Logic Bool
leak_in_bathroom = do
  (hall_wet >>  kitchen_dry) `mplus` return False

problem_in_kitchen :: Logic Bool
problem_in_kitchen = do
  (hall_wet >>  bathroom_dry ) `mplus` return False

no_water_from_outside :: Logic Bool
no_water_from_outside = do
  window_closed `mplus` no_rain `mplus` return False

leak_in_kitchen :: Logic Bool
leak_in_kitchen = do
  (problem_in_kitchen >>  no_water_from_outside) `mplus` return False
-------------------------- Data marshalling --------------------------
data BoolForm = BoolForm
                   { bool :: Bool
                   } deriving Show
boolWidget :: ContId -> Widget -> Enctype -> Widget
boolWidget klabel bool_widget enctype = $(widgetFile "bool")

boolForm :: Html -> MForm Handler (FormResult BoolForm, Widget)
boolForm = renderDivs $ BoolForm
              <$> areq boolField "Yes or no:" Nothing

boolHtml :: Text -> Logic (ContId, Html)
boolHtml query_string = do
  (klabel, widget, enctype) <- lift $ lift $ generateCcFormPost $ boolForm
  html <- lift $ lift $ defaultLayout $ [whamlet|#{query_string}|] >> boolWidget klabel widget enctype
  return (klabel, html)

inquireBool :: Text -> Logic (ContId, BoolForm)
inquireBool query_string = do
  (klabel, html) <- boolHtml query_string
  bool <- lift $ inquirePostUntil klabel html boolForm
  return (klabel, bool)
----------------------------------------------------------------------

hall_wet :: Logic Bool
hall_wet = do
  (_klabel, BoolForm bool ) <- inquireBool "hall wet?"
  if bool
    then return True
    else mzero

kitchen_dry :: Logic Bool
kitchen_dry = do
  (_klabel, BoolForm  bool) <- inquireBool "kitchen dry?"
  if bool
    then return True
    else mzero

bathroom_dry :: Logic Bool
bathroom_dry = do
  (_klabel, BoolForm bool) <- inquireBool "bathroom dry?"
  if bool
    then return True
    else mzero

window_closed :: Logic Bool
window_closed = do
  (_klabel, BoolForm bool) <- inquireBool "window closed?"
  if bool
    then return True
    else mzero

no_rain :: Logic Bool
no_rain = do
  (_klabel, BoolForm bool) <- inquireBool "no rain?"
  if bool
    then return True
    else mzero

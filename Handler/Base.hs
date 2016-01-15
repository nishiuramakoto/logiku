module Handler.Base where

import Import

getBaseR :: Handler Html
getBaseR = defaultLayout $(widgetFile "base")

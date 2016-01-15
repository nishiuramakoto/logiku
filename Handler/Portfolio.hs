module Handler.Portfolio where

import Import

getPortfolio01R :: Handler Html
getPortfolio01R = defaultLayout $ do
  addStylesheet $ StaticR  css_normalize_css
  setTitle "水廻りの疑問"

  toWidget $(widgetFile "01")

{-# LANGUAGE OverloadedStrings  #-}

module Handler.Home where

import  Import hiding (FileInfo)
import Prelude ((!!))
import  DBFS
import  qualified Data.Text as T

import Handler.Command



getCommandEditR :: FileId -> Handler Html
getCommandEditR file = do
  return $ toHtml $ (show file)

-- getCommandRunR :: FileId -> Handler Html
-- getCommandRunR file = do
--   return $ toHtml $ (show file)


getHomeR :: Handler Html
getHomeR = redirect CommandR

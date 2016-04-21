{-# LANGUAGE OverloadedStrings  #-}

module Handler.Home where

import  Import hiding (FileInfo)
import Prelude ((!!))
import  DBFS
import  qualified Data.Text as T

import Handler.Goal



getGoalEditR :: FileId -> Handler Html
getGoalEditR file = do
  return $ toHtml $ (show file)

-- getGoalRunR :: FileId -> Handler Html
-- getGoalRunR file = do
--   return $ toHtml $ (show file)


getHomeR :: Handler Html
getHomeR = redirect GoalR

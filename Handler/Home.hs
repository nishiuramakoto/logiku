{-# LANGUAGE OverloadedStrings  #-}

module Handler.Home where

import  Import hiding (FileInfo)
import Prelude ((!!))
import  DBFS
import  qualified Data.Text as T
import  Data.Time.LocalTime
import  Data.Time.Calendar.WeekDate
import  Text.Printf
import  Constructors

translateWeekJp :: Int -> Char
translateWeekJp n = ("日月火水木金土" ++ (repeat '?')) !! n

showTime :: TimeZone -> UTCTime -> String
showTime tz time = let lctime    = utcToLocalTime tz time
                       lday       = localDay lctime
                       (year,month,doy) = toGregorian lday
                       (_ , _, dow) = toWeekDate lday
                       dowjp = translateWeekJp dow
                       TimeOfDay hour minuit _sec  = localTimeOfDay lctime
                   in
                     printf "%04d/%02d/%02d(%c) %02d:%02d"  year month doy dowjp hour minuit

showPermDirectory :: DirectoryInfo -> String
showPermDirectory info = [r,w,x]
  where
    r = if directoryInfoR info then 'r' else '-'
    w = if directoryInfoW info then 'w' else '-'
    x = if directoryInfoX info then 'x' else '-'

showPermFile :: FileInfo -> String
showPermFile info = [or',ow',ox',ar',aw',ax']
  where
    or' = if fileInfoOwnerR info then 'r' else '-'
    ow' = if fileInfoOwnerW info then 'w' else '-'
    ox' = if fileInfoOwnerX info then 'x' else '-'
    ar' = if fileInfoEveryoneR info then 'r' else '-'
    aw' = if fileInfoEveryoneW info then 'w' else '-'
    ax' = if fileInfoEveryoneX info then 'x' else '-'


showGroupPermFile :: (Text,Perm) -> String
showGroupPermFile (grp, Perm r w x) = show grp ++ [gr,gw,gx]
  where
    gr = if r then 'r' else '-'
    gw = if w then 'w' else '-'
    gx = if x then 'x' else '-'


getCommandEditR :: FileId -> Handler Html
getCommandEditR file = do
  return $ toHtml $ (show file)

-- getCommandRunR :: FileId -> Handler Html
-- getCommandRunR file = do
--   return $ toHtml $ (show file)


getHomeR :: Handler Html
getHomeR = redirect CommandR

getCommandR :: Handler Html
getCommandR = do
  uid <- getCurrentUser

  einfos <- runDB $ findFile uid 0 10
  tz <- liftIO $ getCurrentTimeZone
  case einfos of
     Right infos ->   defaultLayout $ do
       addStylesheet $ StaticR  css_normalize_css
       setTitle "コマンドリスト"
       toWidget $(widgetFile "top_command")
     Left err -> do
       setMessage $ toHtml $ T.pack $ show err
       return $ toHtml $ ("Error" :: T.Text)


getBotR :: Handler Html
getBotR = do
  uid <- getCurrentUser

  edirs <- runDB $ findDirectory uid 0 10
  tz <- liftIO $ getCurrentTimeZone
  case edirs of
     Right directories ->  defaultLayout $ do
       addStylesheet $ StaticR  css_normalize_css
       setTitle "ボットリスト"
       toWidget $(widgetFile "top_bot")
     Left err -> do
       setMessage $ toHtml $ T.pack $ show err
       return $ toHtml $ ("Error" :: T.Text)

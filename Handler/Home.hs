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
    w = if directoryInfoR info then 'w' else '-'
    x = if directoryInfoR info then 'x' else '-'

showPermFile :: FileInfo -> String
showPermFile info = [r,w,x]
  where
    r = if fileInfoR info then 'r' else '-'
    w = if fileInfoR info then 'w' else '-'
    x = if fileInfoR info then 'x' else '-'

getHomeR :: Handler Html
getHomeR = redirect CommandR

getCommandR :: Handler Html
getCommandR = do
  uid <- getCurrentUser

  efiles <- runDB $ findFile uid 0 10
  tz <- liftIO $ getCurrentTimeZone
  case efiles of
     Right files ->   defaultLayout $ do
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

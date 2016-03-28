module Handler.Home where

import  Import
import  DBFS
import  qualified Data.Text as T
import  Data.Time.LocalTime
import  Text.Printf

showTime :: TimeZone -> UTCTime -> String
showTime tz time = let lctime    = utcToLocalTime tz time
                       localday  = localDay lctime
                       (year,month,day) = toGregorian localday
                       TimeOfDay hour min sec  = localTimeOfDay lctime
                   in
                     printf "%04d年%02d月%02d日%02d時%02d分"  year month day hour min


getHomeR :: Handler Html
getHomeR = do
  uid <- getCurrentUser
  efiles <- runDB $ do eids <- findExecutableFile uid 0 10
                       case eids of
                         Right ids -> llFile ids
                         Left err  -> return $ Left err
  tz <- liftIO $ getCurrentTimeZone
  case efiles of
    Right files ->   defaultLayout $ do
      addStylesheet $ StaticR  css_normalize_css
      setTitle "BitBotBed トップ"
      toWidget $(widgetFile "home")
    Left err -> do
      setMessage $ toHtml $ T.pack $ show err
      return $ toHtml $ ("Error" :: T.Text)


getHomeBotR :: Handler Html
getHomeBotR = do
  uid <- getCurrentUser
  efiles <- runDB $ do eids <- findExecutableFile uid 0 10
                       case eids of
                         Right ids -> llFile ids
                         Left err  -> return $ Left err
  tz <- liftIO $ getCurrentTimeZone

  case efiles of
    Right files ->   defaultLayout $ do
      addStylesheet $ StaticR  css_normalize_css
      setTitle "BitBotBed トップ"
      toWidget $(widgetFile "home")
    Left err -> do
      setMessage $ toHtml $ T.pack $ show err
      return $ toHtml $ ("Error" :: T.Text)

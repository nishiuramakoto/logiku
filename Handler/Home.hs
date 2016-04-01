{-# LANGUAGE OverloadedStrings  #-}

module Handler.Home where

import  Import
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


getHomeR :: Handler Html
getHomeR = do
  uid <- getCurrentUser

  efiles <- runDB $ findFile uid 0 10
  tz <- liftIO $ getCurrentTimeZone
  case efiles of
     Right files ->   defaultLayout $ do
       addStylesheet $ StaticR  css_normalize_css
       setTitle "BotsNest トップ"
       toWidget $(widgetFile "home")
     Left err -> do
       setMessage $ toHtml $ T.pack $ show err
       return $ toHtml $ ("Error" :: T.Text)


getHomeBotR :: Handler Html
getHomeBotR = redirect HomeR

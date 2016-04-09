module Handler.Bot (
  getBotR,
  getBotEditR
  ) where

import             Import hiding (parseQuery,readFile)
import             CCGraph
import             Control.Monad.Trans.Either
import             DBFS
import             Prolog
import             Data.Time.LocalTime
import             Constructors
import             Show
import qualified   Data.Text as T

getBotR :: Handler Html
getBotR = do
  uid <- getUserAccountId

  einfos <- runDB $ findDirectory uid 0 10
  tz <- liftIO $ getCurrentTimeZone
  case einfos of
     Right infos ->  defaultLayout $ do
       addStylesheet $ StaticR  css_normalize_css
       setTitle "ボットリスト"
       toWidget $(widgetFile "top_bot")
     Left err -> do
       setMessage $ toHtml $ T.pack $ show err
       return $ toHtml $ ("Error" :: T.Text)


getBotEditR :: DirectoryId -> Handler Html
getBotEditR dir = do
  redirect BotR

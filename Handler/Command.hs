module Handler.Command (
  getCommandR,
  getCommandRunR
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

getCommandR :: Handler Html
getCommandR = do
  uid <- getUserAccountId

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


getCommandRunR :: FileId -> Handler Html
getCommandRunR file = eitherNotFound $ do
  uid      <- lift $ getUserAccountId
  fileData <- EitherT $ runDB $ uid `readFile` file
  let dir = fileDirectoryId fileData
  dirData  <- EitherT $ runDB $ uid `readDirectory` dir
  let botCode = directoryCode dirData
      commandCode = fileCode  fileData

  st <- lift startState
  lift $ runCommand st botCode commandCode
  -- lift $ defaultLayout $ [whamlet|#{show (progCode,goalCode)}|]

runCommand :: CCState -> Text -> Text -> Handler Html
runCommand st botCode commandCode =
  case (programCheck  botCode , goalCheck commandCode) of
  (Right clauses, Right terms)   -> run $ prologExecuteCcMain st  botCode commandCode

  (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in bot code #{show err}|]
  (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in command code  #{show err}|]

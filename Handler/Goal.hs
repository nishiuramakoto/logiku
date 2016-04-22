module Handler.Goal (
  getGoalR,
  getGoalRunR
  ) where

import             Import hiding (parseQuery,readFile, FileInfo)
import             CCGraph
import             Control.Monad.Trans.Either
import             DBFS
import             Prolog
import             Data.Time.LocalTime
import             Constructors
import             Show
import qualified   Data.Text as T

getGoalR :: Handler Html
getGoalR = do
  uid <- getUserAccountId

  einfos <- runDB $ findFile uid 0 10
  tz <- liftIO $ getCurrentTimeZone
  case einfos of
     Right infos ->   defaultLayout $ do
       addStylesheet $ StaticR  css_normalize_css
       setTitle "ゴールリスト"
       toWidget $(widgetFile "goal_list")
     Left err -> do
       setMessage $ toHtml $ T.pack $ show err
       return $ toHtml $ ("Error" :: T.Text)



getGoalRunR :: FileId -> Handler Html
getGoalRunR file = eitherNotFound $ do
  uid      <- lift $ getUserAccountId
  fileData <- EitherT $ runDB $ uid `readFile` file
  let dir = fileDirectoryId fileData
  dirData  <- EitherT $ runDB $ uid `readDirectory` dir
  let progCode = directoryCode dirData
      goalCode = fileCode  fileData

  st <- lift startState
  lift $ runGoal st progCode goalCode
  -- lift $ defaultLayout $ [whamlet|#{show (progCode,goalCode)}|]

runGoal :: CCState -> Text -> Text -> Handler Html
runGoal st progCode goalCode =
  case (programCheck  progCode , goalCheck goalCode) of
  (Right clauses, Right terms)   -> do CCContentHtml html <- run $ prologExecuteCcMain st  progCode goalCode
                                       return html

  (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in program code #{show err}|]
  (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in goal code  #{show err}|]

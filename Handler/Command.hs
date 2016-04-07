module Handler.Command (
  getCommandRunR
  ) where

import             Import hiding (parseQuery,readFile)
import             CCGraph
import             Control.Monad.Trans.Either
import             DBFS
import             Prolog

sessionUserIdent :: Text
sessionUserIdent   = "userIdent"

getUserId :: Handler UserAccountId
getUserId = do mu <- lookupSession sessionUserIdent
               runDB $ getByUserIdent mu




getCommandRunR :: FileId -> Handler Html
getCommandRunR file = eitherNotFound $ do
  uid      <- lift $ getUserId
  fileData <- EitherT $ runDB $ uid `readFile` file
  let dir = fileDirectoryId fileData
  dirData  <- EitherT $ runDB $ uid `readDirectory` dir
  let botCode = directoryCode dirData
      commandCode = fileCode  fileData

  lift $ runCommand botCode commandCode
  -- lift $ defaultLayout $ [whamlet|#{show (progCode,goalCode)}|]

runCommand :: Text -> Text -> Handler Html
runCommand botCode commandCode =
  case (programCheck  botCode , goalCheck commandCode) of
  (Right clauses, Right terms)   -> run $ prologExecuteCcMain botCode commandCode

  (Left  err, _ ) ->  defaultLayout $ [whamlet|Parse error in bot code #{show err}|]
  (_ , Left  err) ->  defaultLayout $ [whamlet|Parse error in command code  #{show err}|]

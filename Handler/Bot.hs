module Handler.Bot (
  getBotR,
  getBotEditR,
  getBotEditNewR,
  postBotSaveR
  ) where

import             Import hiding (parseQuery,readFile)
import             Control.Monad.Trans.Either
import             Control.Monad.CC.CCCxe
import             Data.Time.LocalTime
import             DBFS
import             Prolog
import             CCGraph
import             Constructors
import             Form
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
  st <- startState
  uid <- getUserAccountId
  CCTypeHtml html <- run $ editMain st uid dir
  return html

editMain :: CCState -> UserAccountId -> DirectoryId -> CC CCP Handler CCContentType
editMain st uid dir = do
  result <- runEitherT $ do
    dirData <- EitherT $ lift $ runDB $ readDirectory uid dir
    lift $ inquireEdit st uid (editHtml st uid (Entity dir dirData))
    return ()

  case result of
    Right _   -> return ()
    Left  err -> lift $ setMessage $ toHtml $ T.pack $ show err

  (CCTypeHtml <$> editFinishHtml) >>= inquireFinish



editWidget :: CCState -> UserAccountId -> Entity Directory -> CCNode -> Widget
editWidget st uid (Entity key dir) node = do
  setTitle "Bot editor"
  addScript $ StaticR css_ace_src_noconflict_ace_js
  -- addStylesheet $ StaticR css_bootstrap_css

  let name = directoryName dir
      expl = directoryExplanation dir
      code = directoryCode dir
  $(widgetFile "bot_editor")


editHtml :: CCState -> UserAccountId -> Entity Directory -> CCContentTypeM App
editHtml st uid dir node = do
  CCTypeHtml <$> (lift $ defaultLayout $ editWidget st uid dir node)

inquireEdit :: CCState -> UserAccountId -> CCContentTypeM App -> CC CCP Handler CCState
inquireEdit st uid html = do
  (newNode, content) <- inquire st html
  return (CCState newNode (FormSuccess ()))

editFinishHtml :: CC CCP Handler Html
editFinishHtml = lift $ redirect HomeR



getEditData  (FormSuccess (DirectoryForm name
                        (Textarea expl)
                        (Textarea code)))
  = Just (name,expl,code)
getEditData _ = Nothing


postBotSaveR :: CCNode -> DirectoryId ->  Handler Value
postBotSaveR node dir = do
  eval  <- runEitherT $ trySaveBot dir
  notFoundValue <- returnJson (DirectoryEditResponseJson False (Just $ "Not Found"))
  let notFoundM node = return $ CCTypeValue $ notFoundValue
  case eval of
    Right val ->  do jsonval <-  returnJson val
                     CCTypeValue val <- resume (node, notFoundM) (CCTypeValue jsonval)
                     return val

    Left  err ->  do jsonval <- returnJson ( DirectoryEditResponseJson False (Just $ T.pack $ show err))
                     CCTypeValue val <- resume (node, notFoundM) (CCTypeValue jsonval)
                     return val



postBotSaveR' :: CCNode -> DirectoryId -> Handler Value
postBotSaveR'  node key = do
  eval <- runEitherT $ trySaveBot key
  case eval of
    Right val -> returnJson val
    Left  err -> returnJson (DirectoryEditResponseJson False (Just $ T.pack $ show err))

trySaveBot :: DirectoryId -> EitherT DbfsError Handler DirectoryEditResponseJson
trySaveBot key = do
  uid <- lift getUserAccountId
  jsonVal@(DirectoryEditRequestJson name expl code )
          <- lift $ (requireJsonBody :: Handler DirectoryEditRequestJson)
  dir <- EitherT $ runDB $ uid `readDirectory` key
  let dir' = dir { directoryName        = name
                 , directoryExplanation = expl
                 , directoryCode        = code }
  EitherT $ runDB $ uid `writeDirectory` Entity key dir'

  return $ DirectoryEditResponseJson True Nothing


-- runEitherTSetMessage :: Show err => EitherT err (CC CCP Handler) Html -> CC CCP Handler ()
-- runEitherTSetMessage m = do
--   e <- runEitherT m
--   case e of
--     Right x  -> return x
--     Left err -> lift $ setMessage $ toHtml $ T.pack $ show err


getBotEditNewR :: CCNode -> Handler Html
getBotEditNewR ccnode = redirect HomeR

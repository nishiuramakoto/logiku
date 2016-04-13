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
import             Data.Typeable
import             DBFS
import             Prolog
import             CCGraph
import             Constructors
import             Form
import             Show
import qualified   Data.Text as T

data Action = EditNew
            | Save DirectoryEditResponseJson
              deriving (Show,Eq,Typeable)


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
  CCContentHtml html <- run $ editMain st uid dir
  return html

editMain :: CCState -> UserAccountId -> DirectoryId -> CC CCP Handler CCContentType
editMain st uid dir = do
  result <- runEitherT $ do
    dirData <- EitherT $ lift $ runDB $ readDirectory uid dir
    st'@(CCState _ (Just (CCFormResult result))) <- lift $ inquire st (editHtml st uid (Entity dir dirData))

    case (cast result) of
      Just EditNew -> do
        lift $ lift $ $logInfo $ "edit new"
        lift $ editMain st' uid dir

      Just (Save editResult) -> do
        st'' <- lift $ inquireSave st' editResult
        lift $ editMain st'' uid dir
      Nothing -> do
        lift $ lift $ $logInfo $ T.pack $ show "no known response" ++ show result
        lift $ (CCContentHtml <$> editFinishHtml) >>= inquireFinish

  case result of
    Right content   -> return content
    Left  err -> do
      lift $ $logInfo $ T.pack $ show st
      lift $ setMessage $ toHtml $ T.pack $ show err
      (CCContentHtml <$> editFinishHtml) >>= inquireFinish



inquireSave :: CCState -> DirectoryEditResponseJson -> CC CCP Handler CCState
inquireSave st res = do
  lift $ $logInfo $ T.pack $ show "save:" ++ show res
  json <- returnJson res
  inquire st (const $ return $ CCContentJson json)



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
  CCContentHtml <$> (lift $ defaultLayout $ editWidget st uid dir node)

inquireEdit :: CCState -> UserAccountId -> CCContentTypeM App -> CC CCP Handler CCState
inquireEdit st uid html = do
  st' <- inquire st html
  lift $ $logInfo $ T.pack $ "inquireEdit:" ++ show st'
  return st'

editFinishHtml :: CC CCP Handler Html
editFinishHtml = lift $ redirect HomeR


getEditData  (FormSuccess (DirectoryForm name
                        (Textarea expl)
                        (Textarea code)))
  = Just (name,expl,code)
getEditData _ = Nothing


postBotSaveR :: CCNode -> DirectoryId ->  Handler Value
postBotSaveR node dir = do
  eres  <- runEitherT $ trySaveBot dir
  case eres of
    Right res ->  do CCContentJson val <- resume (CCState node (Just $ CCFormResult (FormSuccess res)))
                     return val

    Left  err ->  do CCContentJson val <- resume (CCState node
                                                  (Just $ CCFormResult
                                                   (FormFailure [T.pack $ show err] :: FormResult DirectoryEditResponseJson)))
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
getBotEditNewR node = do
  content <- resume (CCState node (Just (CCFormResult EditNew )))
  case content of
    CCContentHtml html -> return html
    _ -> error "Content type mismatch"

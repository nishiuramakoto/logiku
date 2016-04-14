module Handler.Bot (
  getBotR,
  getBotEditR,
  getBotEditNewR,
  postBotSaveR,
  postBotSaveNewR
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

-- Just an idea:
-- resume :: YesodCC site
--           => CCState Action -> HandlerT site IO CCContentType
-- resume = CCGraph.resume
-- inquire :: YesodCC site
--            => CCState Action -> CCContentTypeM site -> CC CCP (HandlerT site IO) (CCState Action)
-- inquire = CCGraph.inquire


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
  CCContentHtml html <- run $ editMain st uid (Just dir)
  return html

editMain :: CCState -> UserAccountId -> Maybe DirectoryId -> CC CCP Handler CCContentType
editMain st uid mdir = do
  result <- runEitherT $ do
    st'@(CCState _ mresult) <- case mdir of
      Just dir -> do
        dirData <- EitherT $ lift $ runDB $ readDirectory uid dir
        lift $ inquire st (editHtml st uid (Just $ Entity dir dirData))
      Nothing  -> do
        lift $ inquire st (editHtml st uid Nothing)

    case mresult of
      Just (CCFormResult result) -> do
        case (cast result) of
          Just (FormSuccess EditNew) -> do
            lift $ lift $ $logInfo $ "edit new"
            lift $ editMain st' uid Nothing

          Just (FormSuccess (Save editResult)) -> do
            lift $ lift $ $logInfo $ T.pack $ show "save:" ++ show editResult
            st'' <- lift $ inquireSave st' editResult
            lift $ editMain st'' uid mdir

          Just (FormFailure errs) -> do
            lift $ lift $ $logInfo $ T.pack $ show "form failure:" ++ show errs
            lift $ lift $ setMessage $ toHtml $ T.pack $ show errs
            lift $ (CCContentHtml <$> editFinishHtml) >>= inquireFinish

          Nothing -> do
            lift $ lift $ $logInfo $ T.pack $ show "no known response" ++ show result
            lift $ (CCContentHtml <$> editFinishHtml) >>= inquireFinish
      Nothing -> do
        lift $ lift $ $logInfo $ T.pack $ show "no known response:Nothing"
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



directoryUserDisplayName (Entity key dirData) = do
  let diruid = directoryUserId dirData
  mdiru <- get diruid
  case mdiru of
    Just diru -> return $ userAccountDisplayName diru
    Nothing   -> return Nothing


editWidget :: CCState -> UserAccountId -> Maybe (Entity Directory) -> CCNode -> Widget
editWidget st uid mdir node = do
  setTitle "Bot editor"
  addScript $ StaticR js_autosize_js
  addScript $ StaticR css_ace_src_noconflict_ace_js
  -- addStylesheet $ StaticR css_bootstrap_css

  minfo <- case mdir of
    Just (Entity dir _) ->  eitherToMaybe <$> (handlerToWidget $ runDB $ llDirectory uid dir)
    Nothing             ->  return Nothing

  muserDisplayName <- case mdir of
    Just dir ->  handlerToWidget $ runDB $ directoryUserDisplayName dir
    Nothing  ->  return Nothing

  Just u     <- handlerToWidget $ runDB $ get uid
  tz         <- liftIO $ getCurrentTimeZone

  case mdir of
    Just (Entity key dir) -> do let name = directoryName dir
                                    expl = directoryExplanation dir
                                    code = directoryCode dir
                                toWidget [julius|botSaveUrl='@{BotSaveR node key}'|]
                                $(widgetFile "bot_editor")

    Nothing -> do let name = "" :: Text
                      expl = "" :: Text
                      code = "" :: Text
                  toWidget [julius|botSaveUrl='@{BotSaveNewR node}'|]
                  $(widgetFile "bot_editor")

editHtml :: CCState -> UserAccountId -> Maybe (Entity Directory) -> CCContentTypeM App
editHtml st uid mdir node = do
  CCContentHtml <$> (lift $ defaultLayout $ editWidget st uid mdir node)

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
  eres  <- runEitherT $ trySaveBot (Just dir)
  botSave node eres

botSave :: CCNode -> Either DbfsError DirectoryEditResponseJson -> Handler Value
botSave node eres = do
  $logInfo $ T.pack $ show "botSave"

  content <- case eres of
    Right res ->  do resume (CCState node (Just $ CCFormResult (FormSuccess (Save res))))

    Left  err ->  do let res = DirectoryEditResponseJson False (Just $ T.pack $ show err)
                     resume (CCState node (Just $ CCFormResult (FormSuccess (Save res))))

  case content of
    CCContentHtml _ -> do let res = DirectoryEditResponseJson False (Just $ T.pack $ show "Html was returned")
                          returnJson res
    CCContentJson val -> do   $logInfo $ T.pack $ show "botSave"
                              return val




postBotSaveNewR :: CCNode ->  Handler Value
postBotSaveNewR node = do
  eres  <- runEitherT $ trySaveBot Nothing
  botSave node eres


-- postBotSaveR' :: CCNode -> DirectoryId -> Handler Value
-- postBotSaveR'  node key = do
--   eval <- runEitherT $ trySaveBot key
--   case eval of
--     Right val -> returnJson val
--     Left  err -> returnJson (DirectoryEditResponseJson False (Just $ T.pack $ show err))

trySaveBot :: Maybe DirectoryId -> EitherT DbfsError Handler DirectoryEditResponseJson
trySaveBot mkey = do
  lift $ $logInfo $ T.pack $ show "trySaveBot"
  uid <- lift getUserAccountId
  jsonVal@(DirectoryEditRequestJson name expl code )
          <- lift $ (requireJsonBody :: Handler DirectoryEditRequestJson)
  key' <- case mkey of
    Just key -> return key
    Nothing  -> EitherT $ runDB $ uid `mkdir` name

  dir <- EitherT $ runDB $ uid `readDirectory` key'
  let dir' = dir { directoryName        = name
                 , directoryExplanation = expl
                 , directoryCode        = code }
  EitherT $ runDB $ uid `writeDirectory` Entity key' dir'

  return $ DirectoryEditResponseJson True Nothing


-- runEitherTSetMessage :: Show err => EitherT err (CC CCP Handler) Html -> CC CCP Handler ()
-- runEitherTSetMessage m = do
--   e <- runEitherT m
--   case e of
--     Right x  -> return x
--     Left err -> lift $ setMessage $ toHtml $ T.pack $ show err



getBotEditNewR :: CCNode -> Handler Html
getBotEditNewR node = do
  content <- resume (CCState node (Just (CCFormResult (FormSuccess EditNew) )))
  case content of
    CCContentHtml html -> return html
    _ -> error "Content type mismatch"

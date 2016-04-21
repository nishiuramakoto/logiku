{-# LANGUAGE OverloadedStrings #-}

module Handler.Program (
  getProgramR,
  getProgramEditR,
  getProgramEditNewR,
  postProgramSaveR,
  postProgramSaveNewR,
  postProgramEditorGoalEditorAjaxR
  ) where


import             Import hiding (parseQuery,readFile, FileInfo)
import             Control.Monad.Trans.Either
import             Control.Monad.CC.CCCxe
import             Data.Time.LocalTime
import             Data.Typeable
import             Data.Aeson.Types
import             Data.Either
import             DBFS
import             Prolog
import             CCGraph
import             Constructors
import             Form
import             Show
import qualified   Data.Text as T

data Action = EditNew
            | Save DirectoryEditResponseJson
            | SaveGoal FileEditResponseJson
              deriving (Show,Eq,Typeable)

data FileEditRequest = FileEditRequest
                        { token :: Text
                        , name  :: Text
                        , explanation :: Text
                        , code  :: Text
                        } deriving (Show,Eq)

instance FromJSON FileEditRequest where
  parseJSON (Object v) = FileEditRequest <$>
                         v .: "_token" <*>
                         v .: "name" <*>
                         v .: "explanation" <*>
                         v .: "code"

  parseJSON invalid    = error $  "type error mismatch:FileEditRequest" ++ show invalid

--  parseJSON invalid    = error "type error mismatch:FileEditRequest"
  -- parseJSON invalid    = typeMismatch "FileEditRequest" invalid

instance ToJSON FileEditRequest where
  toJSON FileEditRequest {..} = object
                                [ defaultCsrfParamName .= token
                                , "name"   .= name
                                , "explanation" .= explanation
                                , "code"   .= code
                                ]


setErrorMessage :: Either DbfsError a  -> Handler ()
setErrorMessage (Left err) = setMessage $ toHtml $ T.pack (show err)
setErrorMessage (Right _)  = return ()


-- Just an idea:
-- resume :: YesodCC site
--           => CCState Action -> HandlerT site IO CCContentType
-- resume = CCGraph.resume
-- inquire :: YesodCC site
--            => CCState Action -> CCContentTypeM site -> CC CCP (HandlerT site IO) (CCState Action)
-- inquire = CCGraph.inquire


getProgramR :: Handler Html
getProgramR = do
  uid <- getUserAccountId

  einfos <- runDB $ findDirectory uid 0 10
  tz <- liftIO $ getCurrentTimeZone
  case einfos of
     Right infos ->  defaultLayout $ do
       addStylesheet $ StaticR  css_normalize_css
       setTitle "プログラムリスト"
       toWidget $(widgetFile "program_list")
     Left err -> do
       setMessage $ toHtml $ T.pack $ show err
       return $ toHtml $ ("Error" :: T.Text)


getProgramEditR :: DirectoryId -> Handler Html
getProgramEditR dir = do
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

          Just (FormSuccess (SaveGoal editResult)) -> do
            lift $ lift $ $logInfo $ T.pack $ show "save goal:" ++ show editResult
            st'' <- lift $ inquireSaveGoal st' editResult
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

inquireSaveGoal :: CCState -> FileEditResponseJson -> CC CCP Handler CCState
inquireSaveGoal st res = do
  lift $ $logInfo $ T.pack $ show "save goal:" ++ show res
  json <- returnJson res
  inquire st (const $ return $ CCContentJson json)



directoryUserDisplayName (Entity key dirData) = do
  let diruid = directoryUserId dirData
  mdiru <- get diruid
  case mdiru of
    Just diru -> return $ userAccountDisplayName diru
    Nothing   -> return Nothing


emptyForm :: Html -> MForm Handler (FormResult () , Widget)
emptyForm = renderDivs $ pure ()

editWidget :: CCState -> UserAccountId -> Maybe (Entity Directory) -> CCNode -> Widget
editWidget st uid (Just dir@(Entity key val)) node = do
  setTitle "Program editor"
  addScript $ StaticR js_autosize_js
  addScript $ StaticR css_ace_src_noconflict_ace_js
  -- addStylesheet $ StaticR css_bootstrap_css

  minfo <- eitherToMaybe <$> (handlerToWidget $ runDB $ llDirectory uid key)
  muserDisplayName <-  handlerToWidget $ runDB $ directoryUserDisplayName dir

  Just u     <- handlerToWidget $ runDB $ get uid
  tz         <- liftIO $ getCurrentTimeZone

  efileinfos <- handlerToWidget $ runDB $ llFile uid key 0 10
  handlerToWidget $ setErrorMessage efileinfos
  let mfileinfos = eitherToMaybe efileinfos :: Maybe [FileInfo]

  fileData <- case mfileinfos of
    Just fileinfos -> do handlerToWidget $ forM fileinfos $ \info -> do
                           file <- runDB $ uid `readFile` (fileInfoFileId info)
                           return (info, file)
    Nothing -> return []

  (goalEditorWidget, enctype) <- handlerToWidget $ generateFormPost emptyForm

  let name = directoryName val
      expl = directoryExplanation val
      code = directoryCode val
  toWidget [julius|programSaveUrl='@{ProgramSaveR node key}'|]
  $(widgetFile "program_editor")


editWidget st uid Nothing node = do
  setTitle "Program editor"
  addScript $ StaticR js_autosize_js
  addScript $ StaticR css_ace_src_noconflict_ace_js
  -- addStylesheet $ StaticR css_bootstrap_css
  let minfo = Nothing
      muserDisplayName = Nothing :: Maybe Text
      fileData = [] ::  [(FileInfo, Either DbfsError File)]

  Just u     <- handlerToWidget $ runDB $ get uid
  tz         <- liftIO $ getCurrentTimeZone

  (goalEditorWidget, enctype) <- handlerToWidget $ generateFormPost emptyForm

  let name = "" :: Text
      expl = "" :: Text
      code = "" :: Text
  toWidget [julius|programSaveUrl='@{ProgramSaveNewR node}'|]
  $(widgetFile "program_editor")

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


postProgramSaveR :: CCNode -> DirectoryId ->  Handler Value
postProgramSaveR node dir = do
  eres  <- runEitherT $ trySaveProgram (Just dir)
  programSave node eres

programSave :: CCNode -> Either DbfsError DirectoryEditResponseJson -> Handler Value
programSave node eres = do
  $logInfo $ T.pack $ show "programSave"

  content <- case eres of
    Right res ->  do resume (CCState node (Just $ CCFormResult (FormSuccess (Save res))))

    Left  err ->  do let res = DirectoryEditResponseJson False (Just $ T.pack $ show err)
                     resume (CCState node (Just $ CCFormResult (FormSuccess (Save res))))

  case content of
    CCContentHtml _ -> do let res = DirectoryEditResponseJson False (Just $ T.pack $ show "Html was returned")
                          returnJson res
    CCContentJson val -> do   $logInfo $ T.pack $ show "programSave"
                              return val




postProgramSaveNewR :: CCNode ->  Handler Value
postProgramSaveNewR node = do
  eres  <- runEitherT $ trySaveProgram Nothing
  programSave node eres


-- postProgramSaveR' :: CCNode -> DirectoryId -> Handler Value
-- postProgramSaveR'  node key = do
--   eval <- runEitherT $ trySaveProgram key
--   case eval of
--     Right val -> returnJson val
--     Left  err -> returnJson (DirectoryEditResponseJson False (Just $ T.pack $ show err))

trySaveProgram :: Maybe DirectoryId -> EitherT DbfsError Handler DirectoryEditResponseJson
trySaveProgram mkey = do
  lift $ $logInfo $ T.pack $ show "trySaveProgram"
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



getProgramEditNewR :: CCNode -> Handler Html
getProgramEditNewR node = do
  content <- resume (CCState node (Just (CCFormResult (FormSuccess EditNew) )))
  case content of
    CCContentHtml html -> return html
    _ -> error "Content type mismatch"

postProgramEditorGoalEditorAjaxR :: CCNode -> FileId -> Handler Value
postProgramEditorGoalEditorAjaxR node file = do
  $logInfo $ T.pack $ "postProgramEditorGoalEditorAjaxR"
  jsonVal@(FileEditRequest  token name explanation code )
                <-  requireJsonBody :: Handler FileEditRequest
  $logInfo $ T.pack $ "postProgramEditorGoalEditorAjaxR" ++ show jsonVal
  goalSave node $ Right (FileEditResponseJson False (Just "not implemented"))

goalSave :: CCNode -> Either DbfsError FileEditResponseJson -> Handler Value
goalSave node eres = do
  $logInfo $ T.pack $ show "goalSave"

  content <- case eres of
    Right res ->  do resume (CCState node (Just $ CCFormResult (FormSuccess (SaveGoal res))))

    Left  err ->  do let res = FileEditResponseJson False (Just $ T.pack $ show err)
                     resume (CCState node (Just $ CCFormResult (FormSuccess (SaveGoal res))))

  case content of
    CCContentHtml _ -> do let res = FileEditResponseJson False (Just $ T.pack $ show "Html was returned")
                          returnJson res
    CCContentJson val -> do   $logInfo $ T.pack $ show "goalSave"
                              return val

{-# LANGUAGE DeriveGeneric #-}

module Handler.Program (
  getProgramR,
  getProgramEditR,
  getProgramEditNewR,
  postProgramSaveR,
  postProgramSaveNewR,
  postProgramEditorGoalEditorR,
  postProgramEditorGoalEditorNewGoalR
  ) where


import             Import hiding (parseQuery,readFile, writeFile, FileInfo)
import             GHC.Generics
import             Control.Monad.Trans.Either
import             Control.Monad.CC.CCCxe
import             Data.Time.LocalTime
import             Data.Typeable
import             Data.Either
import             DBFS
import             Prolog
import             CCGraph
import             Constructors
import             Form
import             Show
import qualified   Data.Text as T
import             Handler.Goal


data Action = EditNew
            | Save DirectoryEditResponseJson
            | SaveGoal FileEditResponseJson
              deriving (Show,Eq,Typeable)



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
  st <- startState "プログラム編集スタート"
  uid <- getUserAccountId
  (Right (CCContentHtml html) , _) <- runWithBuiltins $ editMain st uid (Just dir)
  return html

editMain :: CCState -> UserAccountId -> Maybe DirectoryId -> CCPrologHandler CCContentType
editMain st uid mdir = do
  result <- runEitherT $ do
    st' <- case mdir of
      Just dir -> do
        dirData <- EitherT $ lift $ runDB $ readDirectory uid dir
        lift $ inquire st "プログラム編集" (editHtml st uid (Just $ Entity dir dirData))
      Nothing  -> do
        lift $ inquire st "新規プログラム" (editHtml st uid Nothing)

    case ccsCurrentForm st' of
      (CCFormResult result) -> do
        case (cast result) of
          Just (FormSuccess EditNew) -> do
            lift $ $logInfo $ "edit new"
            lift $ editMain st' uid Nothing

          Just (FormSuccess (Save editResult)) -> do
            lift $ $logInfo $ T.pack $ show "save:" ++ show editResult
            st'' <- lift $ inquireSave st' editResult
            lift $ editMain st'' uid mdir

          Just (FormSuccess (SaveGoal editResult)) -> do
            lift $ $logInfo $ T.pack $ show "save goal:" ++ show editResult
            st'' <- lift $ inquireSaveGoal st' editResult
            lift $ editMain st'' uid mdir

          Just (FormFailure errs) -> do
            lift $ $logInfo $ T.pack $ show "form failure:" ++ show errs
            lift $ lift $ setMessage $ toHtml $ T.pack $ show errs
            lift $ (CCContentHtml <$> editFinishHtml) >>= inquireFinish

          Nothing -> do
            lift $ $logInfo $ T.pack $ show "no known response" ++ show result
            lift $ editMain st' uid mdir
            --lift $ (CCContentHtml <$> editFinishHtml) >>= inquireFinish

  case result of
    Right content   -> return content
    Left  err -> do
      $logInfo $ T.pack $ show st
      lift $ setMessage $ toHtml $ T.pack $ show err
      (CCContentHtml <$> editFinishHtml) >>= inquireFinish



inquireSave :: CCState -> DirectoryEditResponseJson -> CCPrologHandler CCState
inquireSave st res = do
  lift $ $logInfo $ T.pack $ show "save:" ++ show res
  json <- returnJson res
  inquire st "セーブ" (const $ return $ CCContentJson json)

inquireSaveGoal :: CCState -> FileEditResponseJson -> CCPrologHandler CCState
inquireSaveGoal st res = do
  lift $ $logInfo $ T.pack $ show "save goal:" ++ show res
  json <- returnJson res
  inquire st "セーブゴール" (const $ return $ CCContentJson json)



-- directoryUserDisplayName (Entity key dirData) = do
--   let diruid = directoryUserId dirData
--   mdiru <- get diruid
--   case mdiru of
--     Just diru -> return $ userAccountDisplayName diru
--     Nothing   -> return Nothing


emptyForm :: Html -> MForm Handler (FormResult () , Widget)
emptyForm = renderDivs $ pure ()

editWidget :: CCState -> UserAccountId -> Maybe (Entity Directory) -> CCNode -> Widget
editWidget st uid (Just dir@(Entity key val)) node = do
  setTitle "Program editor"
  addScript $ StaticR js_autosize_js
  addScript $ StaticR css_ace_src_noconflict_ace_js
  -- addStylesheet $ StaticR css_bootstrap_css

  einfo <- Just <$> (handlerToWidget $ runDB $ llDirectory uid key)
  -- muserDisplayName <-  handlerToWidget $ runDB $ directoryUserDisplayName dir

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
      mdir = Just key
  toWidget [julius|programSaveUrl='@{ProgramSaveR node key}'|]
  $(widgetFile "program_editor")


editWidget st uid Nothing node = do
  setTitle "Program editor"
  addScript $ StaticR js_autosize_js
  addScript $ StaticR css_ace_src_noconflict_ace_js
  -- addStylesheet $ StaticR css_bootstrap_css
  let einfo = Nothing :: Maybe (Either DbfsError DirectoryInfo)
      muserDisplayName = Nothing :: Maybe Text
      fileData = [] ::  [(FileInfo, Either DbfsError File)]

  Just u     <- handlerToWidget $ runDB $ get uid
  tz         <- liftIO $ getCurrentTimeZone

  (goalEditorWidget, enctype) <- handlerToWidget $ generateFormPost emptyForm

  let name = "" :: Text
      expl = "" :: Text
      code = "" :: Text
      mdir = Nothing
  toWidget [julius|programSaveUrl='@{ProgramSaveNewR node}'|]
  $(widgetFile "program_editor")

editHtml :: CCState -> UserAccountId -> Maybe (Entity Directory) -> CCContentTypeM App
editHtml st uid mdir node = lift $ do
  CCContentHtml <$> (defaultLayout $ editWidget st uid mdir node)

inquireEdit :: CCState -> UserAccountId -> CCContentTypeM App -> CCPrologHandler CCState
inquireEdit st uid html = do
  st' <- inquire st "編集" html
  lift $ $logInfo $ T.pack $ "inquireEdit:" ++ show st'
  return st'

editFinishHtml :: CCPrologHandler Html
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

  (Right content, _) <- case eres of
    Right res ->  do resume =<< getFormSuccessState node (Save res)

    Left  err ->  do let res = DirectoryEditResponseJson False (Just $ T.pack $ show err)
                     resume =<< getFormSuccessState node (Save res)

  case content of
    CCContentHtml _ -> do let res = DirectoryEditResponseJson False (Just $ T.pack $ show "Html was returned")
                          returnJson res
    CCContentJson val -> do   $logInfo $ T.pack $ show "programSave"
                              return val




postProgramSaveNewR :: CCNode ->  Handler TypedContent
postProgramSaveNewR node = selectRep $ do
  provideRep $ do  eres  <- runEitherT $ trySaveProgram Nothing
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
  (Right content, _) <- resume =<< getFormSuccessState node EditNew
  case content of
    CCContentHtml html -> return html
    _ -> error "Content type mismatch"

---------------------------- Goal Editor  ----------------------------

postProgramEditorGoalEditorR :: CCNode -> DirectoryId -> FileId -> Handler TypedContent
postProgramEditorGoalEditorR node dir file = selectRep $ do
  provideRep $ do eval <- runEitherT $ tryEditGoal dir (Just file)
                  goalResponse node eval

  provideRep $ goalEditorRunGoal node dir

postProgramEditorGoalEditorNewGoalR :: CCNode -> DirectoryId -> Handler TypedContent
postProgramEditorGoalEditorNewGoalR node dir = selectRep $ do
  provideRep $ do  eval <- runEitherT $ tryEditGoal dir Nothing
                   goalResponse node eval

  provideRep $ do goalEditorRunGoal node dir

data GoalEditor = GoalEditor { geName :: Maybe Textarea
                             , geExpl :: Maybe Textarea
                             , geCode :: Textarea
                             , geAction :: Text }

goalEditorForm = GoalEditor
                  <$> iopt textareaField "name"
                  <*> iopt textareaField "explanation"
                  <*> ireq textareaField "code"
                  <*> ireq textField "action"

goalEditorRunGoal :: CCNode -> DirectoryId -> Handler Html
goalEditorRunGoal node dir = do
  $logInfo "goalEditorRunGoal"
  uid <- getUserAccountId
  ge <- runInputPost goalEditorForm
  edir <- runDB $ uid `readDirectory` dir
  case edir of
       Left  err ->  permissionDenied $ T.pack $ show err
       Right dirContent -> do
         let progCode = directoryCode dirContent
             goalCode = unTextarea $ geCode ge

         -- st <- getFormMissingState node
         st <- startState "ゴールスタート"
         runGoal st progCode goalCode


goalResponse :: CCNode -> Either DbfsError FileEditResponseJson -> Handler Value
goalResponse node eres = do
  $logInfo $ T.pack $ show "goalResponse"

  (Right content, _) <- case eres of
    Right res ->  do resume =<< getFormSuccessState node (SaveGoal res)

    Left  err ->  do let res = FileEditResponseJson False (Just $ T.pack $ show err) "" "" ""
                     resume =<< getFormSuccessState node (SaveGoal res)

  case content of
    CCContentHtml _ -> do let res = FileEditResponseJson False (Just $ T.pack $ show "Html was returned")
                                    "" "" ""
                          returnJson res
    CCContentJson val -> do   $logInfo $ T.pack $ show "goalSave"
                              return val

tryEditGoal :: DirectoryId -> Maybe FileId -> EitherT DbfsError Handler FileEditResponseJson
tryEditGoal dir mkey = do
  lift $ $logInfo $ T.pack $ show "trySaveGoal"
  uid <- lift getUserAccountId
  jsonVal@(FileEditRequestJson _ name expl code action  )
                 <- lift $ (requireJsonBody :: Handler FileEditRequestJson)
  case (action, mkey) of
    ("save", Just key) -> do file <- EitherT $ runDB $ uid `readFile` key
                             let file' = file { fileName        = name
                                              , fileExplanation = expl
                                              , fileCode        = code }
                             EitherT $ runDB $ uid `writeFile` Entity key file'

                             return $ FileEditResponseJson True Nothing name expl code

    ("saveNew", Nothing) -> do  key <- EitherT $ runDB $ uid `touchAt` dir $ name

                                file <- EitherT $ runDB $ uid `readFile` key
                                let file' = file { fileName        = name
                                                 , fileExplanation = expl
                                                 , fileCode        = code }
                                EitherT $ runDB $ uid `writeFile` Entity key file'

                                return $ FileEditResponseJson True Nothing name expl code

    ("reload" , Just key) -> do  file <- EitherT $ runDB $ uid `readFile` key
                                 return $ FileEditResponseJson True Nothing
                                   (fileName file) (fileExplanation file) (fileCode file)

    ("delete" , Just key) -> do EitherT $ runDB $ uid `rm` key
                                return $ FileEditResponseJson True Nothing "" "" ""


    (action , _) -> return $ FileEditResponseJson False
                    (Just $ T.concat [ "unknown action:" , action ] ) "" "" ""

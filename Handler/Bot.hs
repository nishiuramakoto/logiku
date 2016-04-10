module Handler.Bot (
  getBotR,
  getBotEditR,
  postBotEditContR,
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
import             Form hiding(directoryName,directoryExplanation)
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
  run $ editMain st uid dir

postBotEditContR :: Int -> Handler Html
postBotEditContR node = do
  not_found_html <- defaultLayout [whamlet|postBotEditContR: Not Found|]
  resume node not_found_html

editWidget :: CCState -> UserAccountId -> Entity Directory -> CCNode -> Widget
editWidget st uid (Entity key dir) node = do
  setTitle "Bot editor"
  addScript $ StaticR css_ace_src_noconflict_ace_js
  -- addStylesheet $ StaticR css_bootstrap_css

  let name = directoryName dir
      expl = directoryExplanation dir
      code = directoryCode dir
  $(widgetFile "bot_editor")


editHtml :: CCState -> UserAccountId -> Entity Directory -> CCNode -> CC CCP Handler Html
editHtml st uid dir node = do
  lift $ defaultLayout $ editWidget st uid dir node

inquireEdit :: CCState -> UserAccountId -> Entity Directory -> CC CCP Handler CCState
inquireEdit st uid dir = do
  newNode <- inquire st (editHtml st uid dir)
  return (newNode , formInj (FormSuccess ()))

editFinishHtml :: CC CCP Handler Html
editFinishHtml = lift $ redirect HomeR

editMain :: CCState -> UserAccountId -> DirectoryId -> CC CCP Handler Html
editMain st uid dir =  do
   edata <- lift $ runDB $ readDirectory uid dir
   case edata of
     Right dirData -> inquireEdit st uid (Entity dir dirData) >> return ()
     Left  err     -> lift $ setMessage $ toHtml $ T.pack $ show err

   editFinishHtml >>= inquireFinish

-- loopEdit :: CCState -> UserAccountId -> DirectoryId -> Bool -> CC CCP Handler ()
-- loopEdit st uid dir forceSave = do
--   edata <- lift $ runDB $ readDirectory uid dir
--   case edata of
--     Right dirData -> loopEdit' dirData
--     Left  err    -> do
--       setMessage $ toHtml $ T.pack $ show err
--       return ()

--   where
--     loopEdit' :: Directory -> CC CCP Handler ()
--     loopEdit' dirData = do
--       let uid' = directoryUserId dirData
--           name = directoryName   dirData
--           expl = directoryExplanation dirData
--           code = directoryCode   dirData

--       (st', mact) <- inquireDirectory st name expl code forceSave

--       case (mact, getEditData st') of
--         (Just Save, Just (newName,newExpl,newCode)) -> do
--           write st' uid dirData { directoryName = newName
--                                 , directoryExplanation = newExpl
--                                 , directoryCode = newCode
--                                 }
--             forceSave
--         _ -> loopEdit st' uid dir forceSave

--     write st uid dirData forceSave = do
--       if forceSave || programOK newCode
--         then do runDB $ writeDirectory uid dirData
--                 loopEdit st uid dirData forceSave
--           else
--   ((node, form), mact) <- inquireDirectory st  "" (Textarea "") (Textarea "") forceSave
--   let mdata = getEditData form
--   case (mact, mdata) of
--     (Just Save, Just (expl,code)) -> save st uid dir (expl,code) forceSave
--     _ loopEdit st uid dir forceSave



getEditData (FormDirectoryForm
          (FormSuccess (DirectoryForm name
                        (Textarea expl)
                        (Textarea code))))
  = Just (name,expl,code)
getEditData _ = Nothing


postBotSaveR :: DirectoryId -> Handler Value
postBotSaveR  key = do
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

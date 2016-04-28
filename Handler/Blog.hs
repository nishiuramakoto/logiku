
module Handler.Blog  where

import             Import hiding(breadcrumb, Form)
import             Control.Monad.CC.CCCxe
import qualified   Data.Text as T
import             Data.Typeable
import             CCGraph
import             Constructors
import             Form
import             Authentication
import             Breadcrumb

-- getHomeR :: Handler Html
-- getHomeR = redirect BlogR

-------------------------------------------------------------------------------------------

getBlogR :: Handler Html
getBlogR = do
  st <- startState "ブログスタート"
  (Right (CCContentHtml html), _) <- runWithBuiltins $ blog_main st
  return html

postBlogContR  :: UserAccountId -> CCNode -> Handler Html
postBlogContR uid node = do
  (Right (CCContentHtml html) , _) <- resume =<< getFormMissingState node
  return html

getBlogContR  :: UserAccountId -> CCNode -> Handler Html
getBlogContR uid node = do
  (Right (CCContentHtml html) , _) <- resume =<< getFormMissingState node
  return html

------------------------------  Types --------------------------------
type Username = Text
data BlogAction = Cancel | Submit | Preview | Logout | New
                deriving (Eq,Show)


-----------------------------------------------------------------------------
data UserForm = UserForm
                { userformName     :: Text
                , userformPassword :: Text
                } deriving (Eq,Ord,Show,Typeable)

blogLoginWidget :: CCState -> CCNode -> Widget -> Enctype -> Widget
blogLoginWidget st node blog_login_widget enctype =  do
  let breadcrumb = breadcrumbWidget st
  uid   <- handlerToWidget $ getUserAccountId
  setTitle "Blog Login"
  $(widgetFile "blog_login")

blogLoginForm :: Html -> MForm Handler (FormResult UserForm, Widget)
blogLoginForm = identifyForm "userForm" $ renderDivs $ UserForm
                <$> areq textField      "Enter the user name:"   Nothing
                <*> areq passwordField  "Enter the password" Nothing

blogLoginHtml :: CCState -> CCContentTypeM App
blogLoginHtml st node = lift $ do
  (widget, enctype) <- generateCCFormPost blogLoginForm
  (CCContentHtml <$> (defaultLayout $ blogLoginWidget st node widget enctype))

inquireBlogLogin :: CCState -> CCPrologHandler CCState
inquireBlogLogin st = inquirePostUntil st "ブログログイン" (blogLoginHtml st) blogLoginForm

---------------------------------------------------------------------------------
blogLogoutWidget ::  CCState -> CCNode -> Username -> Widget
blogLogoutWidget st node username = do
  let breadcrumb = breadcrumbWidget st
  uid   <- handlerToWidget $ getUserAccountId
  setTitle "Blog Logout"
  $(widgetFile "blog_logout")

blogLogoutHtml :: CCState -> UserForm -> CCPrologHandler Html
blogLogoutHtml st user = lift $ do
  defaultLayout $ blogLogoutWidget st (ccsCurrentNode st) (userformName user)

-----------------------------------------------------------------------------------

data BlogForm = BlogForm { blogformTitle :: Text
                         , blogformBody  :: Textarea
                         } deriving (Eq,Ord,Show,Typeable)

blogNewForm :: Html -> MForm Handler (FormResult BlogForm, Widget)
blogNewForm = renderDivs $ BlogForm
              <$> areq textField "Subject:" Nothing
              <*> areq textareaField "Body:" Nothing

blogNewWidget :: CCState -> CCNode -> Widget -> Enctype -> Username -> Widget
blogNewWidget st node  blog_new_form enctype username = do
  let breadcrumb = breadcrumbWidget st
  uid   <- handlerToWidget $ getUserAccountId
  setTitle "Blog New"
  $(widgetFile "blog_new")

blogNewHtml :: CCState -> UserForm -> CCContentTypeM App
blogNewHtml st user node = lift $ do
  (widget, enctype) <- generateCCFormPost blogNewForm
  CCContentHtml <$> (defaultLayout $ blogNewWidget st node widget enctype (userformName user))

inquireBlogNew :: CCState -> UserForm -> CCPrologHandler (CCState, Maybe BlogAction)
inquireBlogNew st user = do
  inquirePostUntilButton  st "新しいブログ" (blogNewHtml st user) blogNewForm
    [ ("cancel", Cancel) , ("submit", Submit) , ("preview", Preview) ]

------------------------------------------------------------------------------------------

blogPreviewWidget :: CCState -> CCNode -> Username -> Textarea -> Widget -> Widget
blogPreviewWidget st node username blog_new_post blog_preview_widget = do
  let breadcrumb = breadcrumbWidget st
  uid   <- handlerToWidget $ getUserAccountId
  setTitle "Blog Preview"
  $(widgetFile "blog_preview")

emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()

blogPreviewHtml ::  CCState -> UserForm -> BlogForm -> CCContentTypeM App
blogPreviewHtml st user blog_new_post node = lift $ do
  (widget, _enctype) <- generateCCFormPost $ emptyForm
  CCContentHtml <$> (defaultLayout $
                     blogPreviewWidget st node (userformName user) (blogformBody blog_new_post) widget)

inquireBlogPreview :: CCState -> UserForm -> BlogForm
                      -> CCPrologHandler (CCState,  Maybe BlogAction)
inquireBlogPreview st user blog_new_post = do
  inquirePostUntilButton st "ブログプレビュー" (blogPreviewHtml st user blog_new_post) emptyForm
    [("cancel", Cancel),("submit", Submit)]

-------------------------------------------------------------------------------------------

blogViewWidget :: CCState -> CCNode -> Username -> Widget -> Widget -> Widget
blogViewWidget st node username blog_data blog_view_widget = do
  let breadcrumb = breadcrumbWidget st
  uid   <- handlerToWidget $ getUserAccountId
  setTitle "Blog View"
  $(widgetFile "blog_view")

blogViewHtml ::  CCState -> UserForm -> Widget -> CCContentTypeM App
blogViewHtml  st user blog_data node = lift $ do
  (widget, _enctype) <- generateCCFormPost emptyForm
  CCContentHtml <$> (defaultLayout $ blogViewWidget st node (userformName user) blog_data widget)

inquireBlogView :: CCState -> UserForm -> Widget
                   -> CCPrologHandler (CCState, Maybe BlogAction)
inquireBlogView st user blog_data = do
  inquirePostUntilButton  st "ブログビュー" (blogViewHtml st user blog_data) emptyForm
    [("logout", Logout), ("new", New) ]

------------------------  Database handling --------------------------
readBlogs :: CCPrologHandler Widget
readBlogs = lift $ do
  users <- runDB $ selectList [] [Asc UserAccountIdent]
  ws <- mapM printUserBlog users
  return $ concat_widgets ws
  where
    printUserBlog :: Entity UserAccount -> Handler Widget
    printUserBlog (Entity userId user) = do
      blogs  <- runDB $ selectList [BlogOwnerId ==. userId] []
      return [whamlet|
          <p> <b> #{userAccountIdent user}'s blog </b>
          $forall Entity _blogid blog <- blogs
            <p> [#{blogBlogTitle blog}]
            <p> #{blogBlogBody  blog}
      |]

    concat_widgets [] =  [whamlet||]
    concat_widgets (w:ws) = w >> concat_widgets ws

submitBlog ::  CCState -> UserForm -> BlogForm -> CCPrologHandler CCState
submitBlog st (UserForm name pass) (BlogForm title (Textarea body)) = lift $  do
  users  <- runDB $ selectList [UserAccountIdent ==. name ] []
  time <- liftIO $ getCurrentTime
  userid <- case users of
    []     -> runDB $ insert $ (makeUserAccount name time time time) { userAccountPassword = Just pass }
    (Entity userid _user: _us) -> return userid
  _blogid <- runDB $ insert $ Blog  userid title body
  return st

------------------------  Application logics  ------------------------

validateUser :: UserForm -> Bool
validateUser (UserForm user pass) = user == pass

blog_main :: CCState -> CCPrologHandler  CCContentType
blog_main state =  do
  lift $ $(logInfo) $ T.pack $ "inquireBlogLogin" ++ show state
  state' <- inquireBlogLogin state

  case ccsCurrentForm state' of
    (CCFormResult a) ->
      case cast a of
      Just (FormSuccess user) -> do
        $(logInfo) "validateUser"
        if validateUser user
          then authSuccess  state' user
          else authFail     state' user

  where
    authSuccess :: CCState -> UserForm -> CCPrologHandler CCContentType
    authSuccess state' user = do
      $(logInfo) "auth success"
      state'' <- loop_browse state' user
      logout_html <- blogLogoutHtml state'' user
      inquireFinish (CCContentHtml logout_html)

    authFail :: CCState -> UserForm -> CCPrologHandler CCContentType
    authFail state' _user = do
      $(logInfo) "auth fail"
      blog_main  state'

    loop_browse :: CCState ->  UserForm -> CCPrologHandler CCState
    loop_browse state' user = do
      blogs <- readBlogs
      (state'', maybe_action) <- inquireBlogView state' user blogs

      case maybe_action of
        Just New    -> do
          state''' <- edit state'' user
          loop_browse state''' user

        Just Logout -> return state''
        _           -> loop_browse state'' user

    edit state' user  = do
      (state'', maybe_action) <- inquireBlogNew state' user
      case ccsCurrentForm state'' of
        (CCFormResult res) ->
          case cast res of
          Just (FormSuccess (blog :: BlogForm)) -> do
            case maybe_action of
              Just Cancel  -> return state''
              Just Submit  -> submitBlog state'' user blog
              Just Preview -> preview state'' user blog
              _ -> return state''

    preview :: CCState -> UserForm -> BlogForm -> CCPrologHandler CCState
    preview state' user blog = do
      (state'', maybe_action) <- inquireBlogPreview state' user blog
      case maybe_action of
        Just Cancel -> return state''
        Just Submit -> submitBlog state'' user blog
        _ -> return state''

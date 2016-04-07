
module Handler.Blog  where

import             Import hiding(breadcrumb, Form)
import             Control.Monad.CC.CCCxe
import qualified   Data.Text as T
import             CCGraph
import             Constructors
import             Form


getHomeR :: Handler Html
getHomeR = redirect BlogR

-------------------------------------------------------------------------------------------

getBlogR :: Handler Html
getBlogR = do
  (root,_) <- insertCCRoot
  run $ blog_main (root,FormEmptyForm FormMissing)


postBlogContR  :: CCNode -> Handler Html
postBlogContR node = do
  notFoundHtml <- defaultLayout [whamlet|Not Found|]
  resume node notFoundHtml

getBlogReplayR  :: CCNode -> Handler Html
getBlogReplayR node = do
  notFoundHtml <- defaultLayout [whamlet|Not Found|]
  resume node notFoundHtml

------------------------------  Types --------------------------------
type Username = Text
data BlogAction = Cancel | Submit | Preview | Logout | New
                deriving (Eq,Show)

type State = (CCNode, Form)
currentNode :: State -> CCNode
currentNode  = fst
currentRes :: State -> Form
currentRes  = snd

------------------------------------------------------------------------------

breadcrumbWidget :: State -> Widget
breadcrumbWidget st@(node,_) = do
  path' <-  handlerToWidget $ spine node
  $(widgetFile "breadcrumb")

-----------------------------------------------------------------------------
blogLoginWidget :: State -> CCNode -> Widget -> Enctype -> Widget
blogLoginWidget st node blog_login_widget enctype =  do
  let breadcrumb = breadcrumbWidget st
  setTitle "Blog Login"
  $(widgetFile "blog_login")

blogLoginForm :: Html -> MForm Handler (FormResult UserForm, Widget)
blogLoginForm = renderDivs $ UserForm
                <$> areq textField      "Enter the user name:"   Nothing
                <*> areq passwordField  "Enter the password" Nothing

blogLoginHtml :: State -> CCNode -> CC CCP Handler Html
blogLoginHtml st node = do
  (widget, enctype) <- lift $ generateCCFormPost blogLoginForm
  lift $ defaultLayout $ blogLoginWidget st node widget enctype

inquireBlogLogin :: State -> CC CCP Handler (CCNode,Form)
inquireBlogLogin st = inquirePostUntil st (blogLoginHtml st) blogLoginForm

---------------------------------------------------------------------------------
blogLogoutWidget ::  State -> CCNode -> Username -> Widget
blogLogoutWidget st node username = do
  let breadcrumb = breadcrumbWidget st
  setTitle "Blog Logout"
  $(widgetFile "blog_logout")

blogLogoutHtml :: State -> UserForm -> CC CCP Handler Html
blogLogoutHtml st user = do
  lift $ defaultLayout $ blogLogoutWidget st (currentNode st) (userformName user)

-----------------------------------------------------------------------------------

blogNewForm :: Html -> MForm Handler (FormResult BlogForm, Widget)
blogNewForm = renderDivs $ BlogForm
              <$> areq textField "Subject:" Nothing
              <*> areq textareaField "" Nothing

blogNewWidget :: State -> CCNode -> Widget -> Enctype -> Username -> Widget
blogNewWidget st node  blog_new_form enctype username = do
  let breadcrumb = breadcrumbWidget st
  setTitle "Blog New"
  $(widgetFile "blog_new")

blogNewHtml :: State -> UserForm -> CCNode -> CC CCP Handler Html
blogNewHtml st user node = do
  (widget, enctype) <- lift $ generateCCFormPost blogNewForm
  lift $ defaultLayout $ blogNewWidget st node widget enctype (userformName user)

inquireBlogNew :: State -> UserForm -> CC CCP Handler ((CCNode,Form), Maybe BlogAction)
inquireBlogNew st user = do
  inquirePostUntilButton  st (blogNewHtml st user) blogNewForm
    [ ("cancel", Cancel) , ("submit", Submit) , ("preview", Preview) ]

------------------------------------------------------------------------------------------

blogPreviewWidget :: State -> CCNode -> Username -> Textarea -> Widget -> Widget
blogPreviewWidget st node username blog_new_post blog_preview_widget = do
  let breadcrumb = breadcrumbWidget st
  setTitle "Blog Preview"
  $(widgetFile "blog_preview")

emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()

blogPreviewHtml ::  State -> UserForm -> BlogForm -> CCNode -> CC CCP Handler Html
blogPreviewHtml st user blog_new_post node = do
  (widget, _enctype) <- lift $ generateCCFormPost $ emptyForm
  lift $ defaultLayout $
          blogPreviewWidget st node (userformName user) (blogformBody blog_new_post) widget

inquireBlogPreview :: State -> UserForm -> BlogForm
                      -> CC CCP Handler ((CCNode,Form),  Maybe BlogAction)
inquireBlogPreview st user blog_new_post = do
  inquirePostUntilButton st (blogPreviewHtml st user blog_new_post) emptyForm
    [("cancel", Cancel),("submit", Submit)]

-------------------------------------------------------------------------------------------

blogViewWidget :: State -> CCNode -> Username -> Widget -> Widget -> Widget
blogViewWidget st node username blog_data blog_view_widget = do
  let breadcrumb = breadcrumbWidget st
  setTitle "Blog View"
  $(widgetFile "blog_view")

blogViewHtml ::  State -> UserForm -> Widget
                -> CCNode -> CC CCP Handler  Html
blogViewHtml  st user blog_data node = do
  (widget, _enctype) <- lift $ generateCCFormPost emptyForm
  lift $ defaultLayout $ blogViewWidget st node (userformName user) blog_data widget

inquireBlogView :: State -> UserForm -> Widget
                   -> CC CCP Handler ((CCNode,Form), Maybe BlogAction)
inquireBlogView st user blog_data = do
  inquirePostUntilButton  st (blogViewHtml st user blog_data) emptyForm
    [("logout", Logout), ("new", New) ]

------------------------  Database handling --------------------------
readBlogs :: CC CCP Handler Widget
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

submitBlog ::  State -> UserForm -> BlogForm -> CC CCP Handler State
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

blog_main :: State -> CC CCP Handler Html
blog_main state =  do
  lift $ $(logInfo) $ T.pack $ "inquireBlogLogin" ++ show state
  state'@(_, FormUserForm (FormSuccess user)) <- inquireBlogLogin state

  lift $ $(logInfo) "validateUser"
  if validateUser user
    then authSuccess  state' user
    else authFail     state' user

  where
    authSuccess state' user = do
      lift $ $(logInfo) "auth success"
      state'' <- loop_browse state' user
      logout_html <- blogLogoutHtml state'' user
      inquireFinish logout_html

    authFail state' _user = do
      lift $ $(logInfo) "auth fail"
      blog_main  state'

    loop_browse :: State ->  UserForm -> CC CCP Handler State
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
      ((state''@(_, FormBlogForm (FormSuccess blog))), maybe_action) <- inquireBlogNew state' user
      case maybe_action of
        Just Cancel  -> return state''
        Just Submit  -> submitBlog state'' user blog
        Just Preview -> preview state'' user blog
        _ -> return state''

    preview :: State -> UserForm -> BlogForm -> CC CCP Handler State
    preview state' user blog = do
      (state'', maybe_action) <- inquireBlogPreview state' user blog
      case maybe_action of
        Just Cancel -> return state''
        Just Submit -> submitBlog state'' user blog
        _ -> return state''

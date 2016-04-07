
module Handler.Blog  where

import             Import
import             Control.Monad.CC.CCCxe
import             CCGraph
import             Constructors
import             Form


getHomeR :: Handler Html
getHomeR = redirect BlogR

-------------------------------------------------------------------------------------------

getBlogR :: Handler Html
getBlogR = do
  (node,_) <- insertCCRoot
  run $ blog_main node


postBlogContR  :: CCNode -> Handler Html
postBlogContR node = do
  notFoundHtml <- defaultLayout [whamlet|Not Found|]
  resume node notFoundHtml

------------------------------  Types --------------------------------
type Username = Text
data BlogAction = Cancel | Submit | Preview | Logout | New
                deriving (Eq,Show)

------------------------------------------------------------------------------

blogLoginWidget :: CCNode -> Widget -> Enctype -> Widget
blogLoginWidget node blog_login_widget enctype =  do
  setTitle "Blog Login"
  $(widgetFile "blog_login")

blogLoginForm :: Html -> MForm Handler (FormResult UserForm, Widget)
blogLoginForm = renderDivs $ UserForm
                <$> areq textField      "Enter the user name:"   Nothing
                <*> areq passwordField  "Enter the password" Nothing

blogLoginHtml :: CCNode -> CC CCP Handler Html
blogLoginHtml node = do
  (widget, enctype) <- lift $ generateCCFormPost blogLoginForm
  lift $ defaultLayout $ blogLoginWidget node widget enctype

inquireBlogLogin :: CCNode -> CC CCP Handler CCLEdge
inquireBlogLogin node = inquirePostUntil node blogLoginHtml blogLoginForm

---------------------------------------------------------------------------------
blogLogoutWidget :: Username -> Widget
blogLogoutWidget username                =   do
  setTitle "Blog Logout"
  $(widgetFile "blog_logout")

blogLogoutHtml :: UserForm -> CC CCP Handler Html
blogLogoutHtml user = do
  lift $ defaultLayout $ blogLogoutWidget (userformName user)

-----------------------------------------------------------------------------------

blogNewForm :: Html -> MForm Handler (FormResult BlogForm, Widget)
blogNewForm = renderDivs $ BlogForm
              <$> areq textField "Subject:" Nothing
              <*> areq textareaField "" Nothing

blogNewWidget :: CCNode -> Widget -> Enctype -> Username -> Widget
blogNewWidget node blog_new_form enctype username = do
  setTitle "Blog New"
  $(widgetFile "blog_new")

blogNewHtml :: UserForm -> CCNode -> CC CCP Handler Html
blogNewHtml user node = do
  (widget, enctype) <- lift $ generateCCFormPost blogNewForm
  lift $ defaultLayout $ blogNewWidget node widget enctype (userformName user)

inquireBlogNew :: CCNode -> UserForm -> CC CCP Handler (CCLEdge, Maybe BlogAction)
inquireBlogNew node user = do
  inquirePostUntilButton node (blogNewHtml user) blogNewForm
    [ ("cancel", Cancel) , ("submit", Submit) , ("preview", Preview) ]

------------------------------------------------------------------------------------------

blogPreviewWidget :: CCNode -> Username -> Textarea -> Widget -> Widget
blogPreviewWidget node username blog_new_post blog_preview_widget = do
  setTitle "Blog Preview"
  $(widgetFile "blog_preview")

emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()

blogPreviewHtml ::  UserForm -> BlogForm -> CCNode -> CC CCP Handler Html
blogPreviewHtml user blog_new_post node = do
  (widget, _enctype) <- lift $ generateCCFormPost $ emptyForm
  lift $ defaultLayout $
          blogPreviewWidget node (userformName user) (blogformBody blog_new_post) widget

inquireBlogPreview :: CCNode -> UserForm -> BlogForm
                      -> CC CCP Handler (CCLEdge,  Maybe BlogAction)
inquireBlogPreview node user blog_new_post = do
  inquirePostUntilButton node (blogPreviewHtml user blog_new_post) emptyForm
    [("cancel", Cancel),("submit", Submit)]

-------------------------------------------------------------------------------------------

blogViewWidget :: CCNode -> Username -> Widget -> Widget -> Widget
blogViewWidget node username blog_data blog_view_widget = do
  setTitle "Blog View"
  $(widgetFile "blog_view")

blogViewHtml ::  UserForm -> Widget
                -> CCNode -> CC CCP Handler  Html
blogViewHtml  user blog_data node = do
  (widget, _enctype) <- lift $ generateCCFormPost emptyForm
  lift $ defaultLayout $ blogViewWidget node (userformName user) blog_data widget

inquireBlogView :: CCNode -> UserForm -> Widget
                   -> CC CCP Handler (CCLEdge, Maybe BlogAction)
inquireBlogView node user blog_data = do
  inquirePostUntilButton node (blogViewHtml user blog_data) emptyForm  [("logout", Logout), ("new", New) ]

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

submitBlog ::  CCNode -> UserForm -> BlogForm -> CC CCP Handler CCNode
submitBlog node (UserForm name pass) (BlogForm title (Textarea body)) = lift $  do
  users  <- runDB $ selectList [UserAccountIdent ==. name ] []
  time <- liftIO $ getCurrentTime
  userid <- case users of
    []     -> runDB $ insert $ (makeUserAccount name time time time) { userAccountPassword = Just pass }
    (Entity userid _user: _us) -> return userid
  _blogid <- runDB $ insert $ Blog  userid title body
  return node

------------------------  Application logics  ------------------------

validateUser :: UserForm -> Bool
validateUser (UserForm user pass) = user == pass

blog_main :: CCNode -> CC CCP Handler Html
blog_main node =  do
  lift $ $(logInfo) "inquireBlogLogin"
  (_,node', CCEdgeLabel (FormUserForm (FormSuccess user))) <- inquireBlogLogin node
  lift $ $(logInfo) "validateUser"
  if validateUser user
    then authSuccess node' user
    else authFail node' user

  where
    authSuccess node' user = do
      lift $ $(logInfo) "auth success"
      _node'' <- loop_browse node' user
      logout_html <- blogLogoutHtml  user
      inquireFinish logout_html

    authFail node' _user = do
      lift $ $(logInfo) "auth fail"
      blog_main node'

    loop_browse :: CCNode -> UserForm -> CC CCP Handler CCNode
    loop_browse node' user = do
      blogs <- readBlogs
      ((_,node'',_), maybe_action) <- inquireBlogView node' user blogs
      case maybe_action of
        Just New    -> do
          node''' <- edit node'' user
          loop_browse node''' user

        Just Logout -> return node'
        _           -> loop_browse node' user

    edit node' user  = do
      ((_,node'',CCEdgeLabel (FormBlogForm (FormSuccess blog))), maybe_action) <- inquireBlogNew node' user
      case maybe_action of
        Just Cancel  -> return node''
        Just Submit  -> submitBlog node'' user blog
        Just Preview -> preview node'' user blog
        _ -> return node''

    preview :: CCNode -> UserForm -> BlogForm -> CC CCP Handler CCNode
    preview node' user blog = do
      ((_,node'',_), maybe_action) <- inquireBlogPreview node' user blog
      case maybe_action of
        Just Cancel -> return node''
        Just Submit -> submitBlog  node'' user blog
        _ -> return node''

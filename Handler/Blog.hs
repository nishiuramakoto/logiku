-- A temporary version, NOT THREAD SAFE

module Handler.Blog  where

import             Import
import             Control.Monad.CC.CCCxe
import             ContMap

-------------------------------------------------------------------------------------------

getBlogR :: Handler Html
getBlogR = run blog_main

postBlogContR  :: Int -> Handler Html
postBlogContR klabel = do
  cont_html <- defaultLayout [whamlet|Continue|]
  not_found_html <- defaultLayout [whamlet|Not Found|]
  resume klabel cont_html not_found_html


------------------------------  Types --------------------------------
type Username = Text
data BlogAction = Cancel | Submit | Preview | Logout | New
                deriving (Eq,Show)

------------------------------------------------------------------------------

data UserForm = UserForm
                { userformName     :: Text
                , userformPassword :: Text
                } deriving Show

blogLoginWidget :: ContId -> Widget -> Enctype -> Widget
blogLoginWidget klabel blog_login_widget enctype =  do
  setTitle "Blog Login"
  $(widgetFile "blog_login")

blogLoginForm :: Html -> MForm Handler (FormResult UserForm, Widget)
blogLoginForm = renderDivs $ UserForm
                <$> areq textField      "Enter the user name:"   Nothing
                <*> areq passwordField  "Enter the password" Nothing

blogLoginHtml :: CC (PS Html) Handler (ContId, Html)
blogLoginHtml = do
  (klabel, widget, enctype) <- lift $ generateCcFormPost $ blogLoginForm
  html <- lift $ defaultLayout $ blogLoginWidget klabel widget enctype
  return (klabel, html)

inquireBlogLogin :: CC (PS Html) Handler (ContId, UserForm)
inquireBlogLogin = do
  (klabel, html) <- blogLoginHtml
  user <- inquirePostUntil klabel html blogLoginForm
  return (klabel, user)

---------------------------------------------------------------------------------
blogLogoutWidget :: Username -> Widget
blogLogoutWidget username                =   do
  setTitle "Blog Logout"
  $(widgetFile "blog_logout")

blogLogoutHtml :: UserForm -> CC (PS Html) Handler Html
blogLogoutHtml user = do
  html <- lift $ defaultLayout $ blogLogoutWidget (userformName user)
  return  html

-----------------------------------------------------------------------------------
data BlogForm = BlogForm { blogformTitle :: Text
                         , blogformBody  :: Textarea
                         } deriving Show

blogNewForm :: Html -> MForm Handler (FormResult BlogForm, Widget)
blogNewForm = renderDivs $ BlogForm
              <$> areq textField "Subject:" Nothing
              <*> areq textareaField "" Nothing

blogNewWidget :: ContId -> Widget -> Enctype -> Username -> Widget
blogNewWidget klabel blog_new_form enctype username = do
  setTitle "Blog New"
  $(widgetFile "blog_new")


blogNewHtml :: UserForm -> CC (PS Html) Handler (ContId, Html)
blogNewHtml user = do
  (klabel, widget, enctype) <- lift $ generateCcFormPost $ blogNewForm
  html <- lift $ defaultLayout $ blogNewWidget klabel widget enctype (userformName user)
  return (klabel, html)

inquireBlogNew :: UserForm -> CC (PS Html) Handler (ContId, (BlogForm, Maybe BlogAction))
inquireBlogNew user = do
  (klabel, html)  <- blogNewHtml user
  (blog, action)  <- inquirePostUntilButton klabel html blogNewForm
                     [ ("cancel", Cancel) , ("submit", Submit) , ("preview", Preview) ]
  return (klabel, (blog, action))


------------------------------------------------------------------------------------------

blogPreviewWidget :: ContId -> Username -> Textarea -> Widget -> Widget
blogPreviewWidget klabel username blog_new_post blog_preview_widget = do
  setTitle "Blog Preview"
  $(widgetFile "blog_preview")

emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()

blogPreviewHtml :: UserForm -> BlogForm
                   -> CC (PS Html) Handler (ContId, Html)
blogPreviewHtml user blog_new_post = do
  (klabel, widget, _enctype) <- lift $ generateCcFormPost $ emptyForm
  html <- lift $ defaultLayout $
          blogPreviewWidget klabel (userformName user) (blogformBody blog_new_post) widget
  return (klabel, html)

inquireBlogPreview :: UserForm -> BlogForm
                      -> CC (PS Html) Handler (ContId,  Maybe BlogAction)
inquireBlogPreview user blog_new_post = do
  (klabel, html) <- blogPreviewHtml user blog_new_post
  (_answer, maybe_action) <- inquirePostUntilButton klabel html emptyForm
                            [("cancel", Cancel),("submit", Submit)]
  return (klabel, maybe_action)

-------------------------------------------------------------------------------------------


blogViewWidget :: ContId -> Username -> Widget -> Widget -> Widget
blogViewWidget klabel username blog_data blog_view_widget = do
  setTitle "Blog View"
  $(widgetFile "blog_view")

blogViewHtml :: UserForm -> Widget
                -> CC (PS Html) Handler (ContId, Html)
blogViewHtml user blog_data = do
  (klabel, widget, _enctype) <- lift $ generateCcFormPost emptyForm
  html   <- lift $ defaultLayout $ blogViewWidget klabel (userformName user) blog_data widget
  return (klabel, html)

inquireBlogView :: UserForm -> Widget
                   -> CC (PS Html) Handler (ContId, Maybe BlogAction)
inquireBlogView user blog_data = do
  (klabel, html) <- blogViewHtml user blog_data
  lift $ $(logInfo) "inquireBlogBUtton start"
  (_answer, maybe_action) <- inquirePostUntilButton klabel html emptyForm
                            [("logout", Logout), ("new", New) ]
  lift $ $(logInfo) "inquireBlogBUtton finish"
  return (klabel, maybe_action)




------------------------  Database handling --------------------------
readBlogs :: CC (PS Html) Handler Widget
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

submitBlog :: UserForm -> BlogForm -> CC (PS Html) Handler ()
submitBlog (UserForm name pass) (BlogForm title (Textarea body)) = lift $  do
  users  <- runDB $ selectList [UserAccountIdent ==. name ] []
  userid <- case users of
    []     -> runDB $ insert $ makeUser name (Just pass)
    (Entity userid _user: _us) -> return userid
  _blogid <- runDB $ insert $ Blog  userid title body
  return ()

------------------------  Application logics  ------------------------

validateUser :: UserForm -> Bool
validateUser (UserForm user pass) = user == pass

blog_main :: CC (PS Html) Handler Html
blog_main =  do
  lift $ $(logInfo) "inquireBlogLogin"
  (_klabel, user) <- inquireBlogLogin
  if validateUser user
    then authSuccess user
    else authFail user

  where
    authSuccess user = do
        lift $ $(logInfo) "loop_browse"
        loop_browse user
        lift $ $(logInfo) "loop_browse"
        logout_html <- blogLogoutHtml user
        inquireFinish logout_html

    authFail user = do
      blog_main

    loop_browse :: UserForm -> CC (PS Html) Handler ()
    loop_browse user = do
      blogs <- readBlogs
      (_klabel, maybe_action) <- inquireBlogView user blogs
      case maybe_action of
        Just New    -> do
          edit user
          loop_browse user
        Just Logout -> do
          return ()
        _           -> do
          loop_browse user

    edit user  = do
      (_klabel, (blog, maybe_action)) <- inquireBlogNew user
      case maybe_action of
        Just Cancel  -> return ()
        Just Submit  -> submitBlog user blog
        Just Preview -> preview user blog
        _ -> return ()

    preview :: UserForm -> BlogForm -> CC (PS Html) Handler ()
    preview user blog = do
      (_klabel, maybe_action) <- inquireBlogPreview user blog
      case maybe_action of
        Just Cancel -> return ()
        Just Submit -> submitBlog user blog
        _ -> return ()

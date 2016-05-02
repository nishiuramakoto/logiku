{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Foundation where

import Import.NoFoundation hiding (Unique)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
-- import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Auth.GoogleEmail2
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Control.Exception.Base

-- import Data.Time.LocalTime
import Authentication
import CCGraph
import SideMenu
import DBFS
import Constructors
import User
import Language.Prolog2.Database
import Language.Prolog2.Types

--------------------------------------------------------------------------

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appCCGraph     :: MVar (CCGraph App) -- ^ Pool of continuations
    , appUserStorage :: MVar (UserStorageMap App)
    , appMenuTree    :: MenuTree
    , appGuestId     :: MVar (Maybe UserAccountId)
    , appBuiltinDatabase :: CCDatabase App
    }

-- | Check database availability. In heroku, A database may be unavailable for a maximum of 4hr/month.
databaseAvailable ::  HandlerT App IO Bool
databaseAvailable = do
  mEntity <- runDB $ selectFirst [DirectoryName !=. ""] []
  case mEntity of
    Just _  ->  return True
    Nothing ->  return False

myErrorHandler ::  ErrorResponse -> HandlerT App IO TypedContent
myErrorHandler (InternalError e) = do
  $logErrorS "Foundations.hs" e
  -- mb <- databaseAvailable -- Turning this causes error handling not to work
  let mb = True
  selectRep $ do
      provideRep $ defaultLayout $ do
        setTitle "Internal Server Error"
        if mb
          then
          toWidget [hamlet|
                <h1> Internal Server Error (Database is under maintainance)
                <pre>#{e}
          |]
          else
          toWidget [hamlet|
                <h1>データベースエラー
                <pre>#{e}
          |]

      provideRep $ return $ object ["message" .= ("Internal Server Error" :: Text), "error" .= e]

myErrorHandler e = defaultErrorHandler e


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    -- makeSessionBackend _ = return Nothing

    -------------------- - Use this in production --------------------
    -- makeSessionBackend _ = sslOnlySessions $ Just <$> defaultClientSessionBackend
    -------------------------- For testing  --------------------------
    makeSessionBackend _ =  Just <$> defaultClientSessionBackend
                           (60*24)    -- timeout in minutes
                           "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- The defaultCsrfMiddleware:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.

    ------------------------  For production  ------------------------
    -- yesodMiddleware = sslOnlyMiddleware (60*24) . defaultCsrfMiddleware . defaultYesodMiddleware
    -------------------------- For testing  --------------------------
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

    errorHandler = myErrorHandler
    defaultLayout widget = do
      master <- getYesod
      mmsg <- getMessage

      mName <- maybeUserIdent
      mDName <- maybeUserDisplayName
      --maid  <- maybeAuthId
      sess  <- getSession
      -- let categoryTree =  $(widgetFile "css-tree")
        -- let categoryTree = [whamlet|tree|]


        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

      pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_searchbox_css
        $(widgetFile "default-layout")

      withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized route isWrite = myIsAuthorized route isWrite
    -- isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
      level /= LevelDebug
--        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserAccountId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    -- redirectToReferer _ = True
    redirectToReferer _ = False

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUserAccount $ credsIdent creds
        time <- liftIO $ getCurrentTime

        $(logInfo) $  T.concat [ "credsIdent" , (T.pack (show (credsIdent creds)))]
        setSession "credsIdent" (credsIdent creds)
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert (makeUserAccount (credsIdent creds) time time time)

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authBrowserId def
                    -- , authGoogleEmail clientId clientSecret
                    , authGoogleEmailSaveToken clientId clientSecret
                    ]

    authHttpManager = getHttpManager

    onLogin = do
      -- maybeUserIdent
      -- maybeDisplayName
      setMessage $ toHtml ("Logged In " :: Text )

    onLogout = do
      clearAuthSession
      setMessage $ toHtml ("Logged out" :: Text)

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

instance YesodCC App where
  getCCPool = appCCGraph

  readCCGraph = usCCGraph <$> readUserStorage

  modifyCCGraph f = modifyUserStorage (liftIO . f')
    where
      f' Nothing = do let UserStorage gr root seq = def
                      (gr',b) <- f gr
                      return (Just (UserStorage gr' root seq),b)
      f' (Just (UserStorage gr root seq)) = do (gr',b) <- f gr
                                               return (Just (UserStorage gr' root seq),b)

  getBuiltinDatabase =  appBuiltinDatabase <$> getYesod

instance YesodUserStorage App where
  getUserStorage = appUserStorage

deriving instance Typeable App


guestId :: Handler UserAccountId
guestId = do
  yesod <- getYesod
  let mv = appGuestId yesod
  mgid <- takeMVar mv

  gid <- case mgid of
    Just gid -> return gid
    Nothing  -> runDB suGuest

  putMVar mv (Just gid)
  return gid

getUserAccountId :: Handler UserAccountId
getUserAccountId = do
  -- let guest = UserAccountKey 5

  mentity <-  maybeAuth
  case mentity of
    Just (Entity uid _) -> return uid
    -- Nothing    -> guestId
    Nothing    -> runDB suGuest

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing

maybeUserDisplayName :: Handler (Maybe Text)
maybeUserDisplayName = do
  uid <- getUserAccountId
  join <$> eitherToMaybe <$> (runDB $ getUserDisplayName uid)


type CCPrologHandler a      = CCPrologT App Handler a

runWithBuiltins ::  Typeable a =>
                    CCPrologHandler a ->  Handler (Either RuntimeError a , IntBindingState T)
runWithBuiltins m = do
  $logInfo "runWithBuiltins"
  run m  =<< (appBuiltinDatabase <$> getYesod)

type CCState = CCStateT App
startState :: Text -> Handler CCState
startState title = do db <- appBuiltinDatabase <$> getYesod
                      mroute <- getCurrentRoute
                      case mroute of
                        Just route -> do
                          (root, la) <- insertCCRoot title (CCFormResult (FormSuccess ())) route db
                          return $ ccKArg la
                        Nothing -> do
                          notFound

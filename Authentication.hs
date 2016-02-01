module Authentication(
  clientId,
  clientSecret,
  maybeDisplayName,
  maybeUserIdent
  )  where

import Import.NoFoundation

import           Data.Default                (def)
import           Data.Text                   (Text)
import qualified Data.Text as T
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail2



-- Replace with Google client ID.
clientId :: Text
clientId = "197748900362-pj584nskcninquf5mmgse28fg2tv2c4a.apps.googleusercontent.com"

-- Replace with Google secret ID.
clientSecret :: Text
clientSecret = "SMbJxghU_ci-Fg2OzO1cwDkY"


maybeDisplayName :: YesodAuth master => HandlerT master IO (Maybe Text)
maybeDisplayName = maybeDisplayNameGoogle

maybeDisplayNameGoogle :: YesodAuth master => HandlerT master IO (Maybe Text)
maybeDisplayNameGoogle = do
  maybeToken <- getUserAccessToken
  case maybeToken  of
    Just token -> do
      app <-getYesod
      maybePerson <- getPerson (authHttpManager app) token
      return $ join $ fmap personDisplayName maybePerson
    Nothing -> return Nothing

maybeUserIdent :: YesodAuth master => HandlerT master IO (Maybe Text)
maybeUserIdent = do
  mtoken <- getUserAccessToken
  case mtoken of
    Nothing    ->  return Nothing
    Just token -> do app <- getYesod
                     mperson <- getPerson (authHttpManager app) token
                     case mperson of
                       Nothing -> return Nothing
                       Just person ->
                         case personEmails person of
                         (Yesod.Auth.GoogleEmail2.Email val _type : _ ) -> do
                           $(logInfo) $ T.pack $ "AuthId=" ++ show val
                           return $ Just val
                         _ -> return Nothing
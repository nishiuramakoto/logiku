{-# LANGUAGE TypeFamilies #-}

module Authentication(
  clientId,
  clientSecret,
  maybeDisplayName,
  maybeUserIdent,
  clearAuthSession,
  myIsAuthorized,
  maybeUserAccountId,
  getUserAccountId'
  )  where

import           Import.NoFoundation
import           Prelude(read)
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail2
import qualified Data.Text as T
import           DBFS


-- Replace with Google client ID.
clientId :: Text
clientId = "197748900362-pj584nskcninquf5mmgse28fg2tv2c4a.apps.googleusercontent.com"

-- Replace with Google secret ID.
clientSecret :: Text
clientSecret = "SMbJxghU_ci-Fg2OzO1cwDkY"

displayNameString :: Text
displayNameString =  "displayName"
userIdentString   :: Text
userIdentString   = "userIdent"

cacheSession :: MonadHandler m => Text -> m (Maybe Text) -> m (Maybe Text)
cacheSession key m = do
  mval <- lookupSession key
  case mval of
    Just val -> return $ Just val
    Nothing  -> do mval' <- m
                   case mval' of
                     Just val' -> do setSession key val'
                                     return $ Just val'
                     Nothing   -> return Nothing

clearAuthSession :: YesodAuth master => HandlerT master IO ()
clearAuthSession = do
  deleteSession displayNameString
  deleteSession userIdentString
  deleteSession "credsIdent"
  deleteSession "_GOOGLE_ACCESS_TOKEN"
  deleteSession "_GOOGLE_CSRF_TOKEN"
  -- clearCreds False

maybeDisplayName :: YesodAuth master => HandlerT master IO (Maybe Text)
maybeDisplayName = cacheSession displayNameString maybeDisplayNameGoogle

maybeDisplayNameGoogle :: YesodAuth master => HandlerT master IO (Maybe Text)
maybeDisplayNameGoogle = do
  maybeToken <- getUserAccessToken
  case maybeToken  of
    Just token -> do
      app <-getYesod
      maybePerson <- getPerson (authHttpManager app) token
      return $ join $ fmap personDisplayName maybePerson
    Nothing -> return Nothing


maybeUserIdent :: MonadHandler m => m (Maybe Text)
maybeUserIdent = lookupSession "credsIdent"



--  cacheSession userIdentString maybeUserIdentGoogle


-- maybeUserIdentGoogle :: YesodAuth master => HandlerT master IO (Maybe Text)
-- maybeUserIdentGoogle = do
--   mtoken <- getUserAccessToken
--   case mtoken of
--     Nothing    ->  return Nothing
--     Just token -> do app <- getYesod
--                      mperson <- getPerson (authHttpManager app) token
--                      case mperson of
--                        Nothing -> return Nothing
--                        Just person ->
--                          case personEmails person of
--                          (Yesod.Auth.GoogleEmail2.Email val _type : _ ) -> do
--                            $(logInfo) $ T.pack $ "AuthId=" ++ show val
--                            return $ Just val
--                          _ -> return Nothing

myIsAuthorized :: Route site -> Bool -> HandlerT site IO  AuthResult
myIsAuthorized _route _isWrite = return Authorized

maybeUserAccountId :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
               => HandlerT site IO (Maybe UserAccountId)
maybeUserAccountId = do

  root <- runDB $ su
  muident <- maybeUserIdent
  case muident of
    Just ident -> do euser <- runDB $ getUser ident
                     case euser of
                       Right uid -> return $ Just uid
                       Left  _   -> do euser' <- runDB $ root `useradd` ident
                                       case euser' of
                                         Right user' -> return $ Just user'
                                         Left  _     -> return $ Nothing

    Nothing   -> Just <$> (runDB $ suGuest)




getUserAccountId' :: (AuthId site ~ UserAccountId
                           , AuthEntity site ~ UserAccount
                           , YesodAuthPersist site
                           , YesodPersist site
                           , YesodPersistBackend site ~ SqlBackend)
                           =>  HandlerT site IO UserAccountId
getUserAccountId' = do
  -- let guest = UserAccountKey 5

  mentity <-  maybeAuth
  case mentity of
    Just (Entity uid _) -> do   putStrLn $ T.pack $ show uid
                                return uid

    Nothing    -> runDB $ suGuest


  -- muid <- maybeUserId

  -- case muid of
  --   Just uid -> return uid
  --   Nothing  -> runDB $ suGuest

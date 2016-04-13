module Handler.UserProfile (
  getUserProfileR,
  postUserProfileR

  ) where

import Import


data UserProfile = UserProfile
                   { userProfileDisplayName ::  Text
                   } deriving Show

userProfileForm :: Maybe Text -> Html -> MForm Handler (FormResult UserProfile, Widget)
userProfileForm mdname = renderDivs $ UserProfile
                         <$> areq textField "DisplayName" mdname


getUserProfileR :: Handler Html
getUserProfileR = do
  uid <- getUserAccountId
  muser <- runDB $ get uid

  case muser of
    Just user -> do
      let dname = userAccountDisplayName user
      (editorWidget, enctype) <- generateFormPost $ userProfileForm dname
      defaultLayout  $(widgetFile "user-profile")
    Nothing   -> do
      notFound

postUserProfileR :: Handler Html
postUserProfileR = do
  uid <- getUserAccountId
  muser <- runDB $ get uid
  case muser of
    Just user -> do
      let mdname = userAccountDisplayName user
      ((result,widget), enctype) <- runFormPost (userProfileForm mdname)

      case result of
        FormSuccess profile -> do
          updateUserProfile profile
          redirect UserProfileR

    _ ->  redirect UserProfileR


updateUserProfile :: UserProfile -> Handler ()
updateUserProfile (UserProfile dname) = do
  uid <- getUserAccountId
  runDB $ update uid [ UserAccountDisplayName =. Just dname]

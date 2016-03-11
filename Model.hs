{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Model where

import ClassyPrelude.Yesod hiding (fileName)
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


------------------------  Smart constructors  ------------------------

makeUser :: Text -> UserAccount
makeUser ident  =
  UserAccount
  { userAccountIdent = ident
  , userAccountPassword = Nothing
  , userAccountDisplayName = Nothing
                   -- Default umask=022
  , userAccountUmaskOwnerR = False
  , userAccountUmaskOwnerW = False
  , userAccountUmaskOwnerX = False
  , userAccountUmaskGroupR = False
  , userAccountUmaskGroupW = True
  , userAccountUmaskGroupX = False
  , userAccountUmaskEveryoneR = False
  , userAccountUmaskEveryoneW = True
  , userAccountUmaskEveryoneX = False
  }


makeGroup :: Text -> UserAccountId -> Group
makeGroup name ownerId =
  Group
  { groupName  = name
  , groupOwner = ownerId
  , groupExplanation = Nothing
  }

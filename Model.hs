{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import ClassyPrelude.Yesod hiding (fileName)
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


------------------------  Smart constructors and simple projections  ------------------------
-- TODO Rewrite this using lenses


data Perm = Perm { permR :: Bool
                 , permW :: Bool
                 , permX :: Bool
                 }
            deriving (Eq,Show,Ord)

data UMask = UMask
             { umaskOwnerR  :: Bool
             , umaskOwnerW  :: Bool
             , umaskOwnerX  :: Bool
             , umaskGroupR  :: Bool
             , umaskGroupW  :: Bool
             , umaskGroupX  :: Bool
             , umaskEveryoneR :: Bool
             , umaskEveryoneW :: Bool
             , umaskEveryoneX :: Bool
             }
           deriving (Eq,Show)


data Permission = Permission
       { permissionOwnerR :: Bool
       , permissionOwnerW :: Bool
       , permissionOwnerX :: Bool
       , permissionGroupR :: Bool
       , permissionGroupW :: Bool
       , permissionGroupX :: Bool
       , permissionEveryoneR :: Bool
       , permissionEveryoneW :: Bool
       , permissionEveryoneX :: Bool
       } deriving (Eq,Show)


data PublicUserAccount = PublicUserAccount
                         { publicUserAccountIdent       :: Text
                         , publicUserAccountDisplayName :: Maybe Text
                         , publicUserAccountCreated     :: UTCTime
                         } deriving (Eq,Show)

data PublicDirectory = PublicDirectory
                       { publicDirectoryUserId    :: UserAccountId
                       , publicDirectoryName      :: Text
                       , publicDirectoryCreated   :: UTCTime
                       } deriving (Eq,Show)

data PublicFile      = PublicFile
                       { publicFileUserId        :: UserAccountId
                       , publicFileDirectoryId   :: DirectoryId
                       , publicFileName          :: Text
                       , publicFileCreated       :: UTCTime
                       } deriving (Eq,Show)


makePublicUserAccount :: UserAccount -> PublicUserAccount
makePublicUserAccount _user@UserAccount { userAccountIdent = ident
                                        , userAccountDisplayName = displayName
                                        , userAccountCreated = created
                                        } =
  PublicUserAccount ident displayName created

makePublicDirectory :: Directory -> PublicDirectory
makePublicDirectory _dir@Directory { directoryUserId = uid
                                   , directoryName   = name
                                   , directoryCreated = created
                                   } =
  PublicDirectory uid name created

makePublicFile :: File -> PublicFile
makePublicFile _file@File { fileUserId = uid
                          , fileDirectoryId = dir
                          , fileName   = name
                          , fileCreated = created
                          } =
  PublicFile uid dir name created





-- not 666, since we file = goal and we want them to be executable by default
fileDefaultPermission :: Permission
fileDefaultPermission = Permission
                        { permissionOwnerR = True
                        , permissionOwnerW = True
                        , permissionOwnerX = True
                        , permissionGroupR = True
                        , permissionGroupW = True
                        , permissionGroupX = True
                        , permissionEveryoneR = True
                        , permissionEveryoneW = True
                        , permissionEveryoneX = True
                        }

directoryDefaultPermission :: Permission
directoryDefaultPermission = Permission
                             { permissionOwnerR = True
                             , permissionOwnerW = True
                             , permissionOwnerX = True
                             , permissionGroupR = True
                             , permissionGroupW = True
                             , permissionGroupX = True
                             , permissionEveryoneR = True
                             , permissionEveryoneW = True
                             , permissionEveryoneX = True
                             }


makeUserAccount :: Text -> UTCTime -> UserAccount
makeUserAccount ident created =
  UserAccount
  { userAccountIdent = ident
  , userAccountPrivileged = False
  , userAccountPassword = Nothing
  , userAccountDisplayName = Nothing
  , userAccountCreated     = created
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


makeUserAccountWithUmask :: Text -> UTCTime -> UMask -> UserAccount
makeUserAccountWithUmask ident created umask  =
  UserAccount
  { userAccountIdent = ident
  , userAccountPrivileged = False
  , userAccountPassword = Nothing
  , userAccountDisplayName = Nothing
  , userAccountCreated     = created
                             -- Default umask=022
  , userAccountUmaskOwnerR = umaskOwnerR umask
  , userAccountUmaskOwnerW = umaskOwnerW umask
  , userAccountUmaskOwnerX = umaskOwnerX umask
  , userAccountUmaskGroupR = umaskGroupR umask
  , userAccountUmaskGroupW = umaskGroupW umask
  , userAccountUmaskGroupX = umaskGroupX umask
  , userAccountUmaskEveryoneR = umaskEveryoneR umask
  , userAccountUmaskEveryoneW = umaskEveryoneW umask
  , userAccountUmaskEveryoneX = umaskEveryoneX umask
  }

umaskFromUserAccount :: UserAccount -> UMask
umaskFromUserAccount user = UMask
                            { umaskOwnerR  = userAccountUmaskOwnerR user
                            , umaskOwnerW  = userAccountUmaskOwnerW user
                            , umaskOwnerX  = userAccountUmaskOwnerX user
                            , umaskGroupR  = userAccountUmaskGroupR user
                            , umaskGroupW  = userAccountUmaskGroupW user
                            , umaskGroupX  = userAccountUmaskGroupX user
                            , umaskEveryoneR  = userAccountUmaskEveryoneR user
                            , umaskEveryoneW  = userAccountUmaskEveryoneW user
                            , umaskEveryoneX  = userAccountUmaskEveryoneX user
                            }




makeDirectory :: UserAccountId -> Text -> UTCTime -> UMask -> Directory
makeDirectory uid name created umask =
  Directory
  { directoryUserId = uid
  , directoryName   = name
  , directoryCode   = ""
  , directoryExplanation = ""
  , directoryCreated  = created

  , directoryOwnerR    = permissionOwnerR directoryDefaultPermission && not (umaskOwnerR umask)
  , directoryOwnerW    = permissionOwnerW directoryDefaultPermission && not (umaskOwnerW umask)
  , directoryOwnerX    = permissionOwnerX directoryDefaultPermission && not (umaskOwnerX umask)
  , directoryEveryoneR = permissionEveryoneR directoryDefaultPermission && not (umaskEveryoneR umask)
  , directoryEveryoneW = permissionEveryoneW directoryDefaultPermission && not (umaskEveryoneW umask)
  , directoryEveryoneX = permissionEveryoneX directoryDefaultPermission && not (umaskEveryoneX umask)
  }





makeFile :: UserAccountId -> DirectoryId -> Text -> UTCTime -> UMask -> File
makeFile uid dir name created umask =
  File
  { fileUserId = uid
  , fileDirectoryId = dir
  , fileName   = name
  , fileCode   = ""
  , fileExplanation = ""
  , fileCreated  = created

  , fileOwnerR    = permissionOwnerR fileDefaultPermission && not (umaskOwnerR umask)
  , fileOwnerW    = permissionOwnerW fileDefaultPermission && not (umaskOwnerW umask)
  , fileOwnerX    = permissionOwnerX fileDefaultPermission && not (umaskOwnerX umask)
  , fileEveryoneR = permissionEveryoneR fileDefaultPermission && not (umaskEveryoneR umask)
  , fileEveryoneW = permissionEveryoneW fileDefaultPermission && not (umaskEveryoneW umask)
  , fileEveryoneX = permissionEveryoneX fileDefaultPermission && not (umaskEveryoneX umask)
  }


makeGroup :: Text -> UserAccountId -> UTCTime -> Group
makeGroup name  ownerId created =
  Group
  { groupName  = name
  , groupOwner = ownerId
  , groupCreated = created
  , groupExplanation = ""
  }

makeGroupMember :: GroupId -> UserAccountId -> UTCTime -> GroupMember
makeGroupMember  gid uid created =  GroupMember gid uid created

makeDirectoryGroup :: DirectoryId -> GroupId -> UTCTime -> UMask -> DirectoryGroup
makeDirectoryGroup dir gid created umask =
  DirectoryGroup
  { directoryGroupDirectoryId = dir
  , directoryGroupGroupId     = gid
  , directoryGroupCreated     = created

  , directoryGroupGroupR      = permissionGroupR directoryDefaultPermission && not (umaskGroupR umask)
  , directoryGroupGroupW      = permissionGroupW directoryDefaultPermission && not (umaskGroupW umask)
  , directoryGroupGroupX      = permissionGroupX directoryDefaultPermission && not (umaskGroupX umask)
  }

makeDirectoryGroupWithPerm :: DirectoryId -> GroupId -> UTCTime -> Perm -> DirectoryGroup
makeDirectoryGroupWithPerm dir gid created perm =
  DirectoryGroup
  { directoryGroupDirectoryId = dir
  , directoryGroupGroupId     = gid
  , directoryGroupCreated     = created

  , directoryGroupGroupR      = permR perm
  , directoryGroupGroupW      = permW perm
  , directoryGroupGroupX      = permX perm
  }


makeFileGroup :: FileId -> GroupId -> UTCTime -> UMask -> FileGroup
makeFileGroup file gid created umask =
  FileGroup
  { fileGroupFileId      = file
  , fileGroupGroupId     = gid
  , fileGroupCreated     = created

  , fileGroupGroupR      = permissionGroupR fileDefaultPermission && not (umaskGroupR umask)
  , fileGroupGroupW      = permissionGroupW fileDefaultPermission && not (umaskGroupW umask)
  , fileGroupGroupX      = permissionGroupX fileDefaultPermission && not (umaskGroupX umask)
  }

makeFileGroupWithPerm :: FileId -> GroupId -> UTCTime -> Perm -> FileGroup
makeFileGroupWithPerm file gid created perm =
  FileGroup
  { fileGroupFileId      = file
  , fileGroupGroupId     = gid
  , fileGroupCreated     = created

  , fileGroupGroupR      = permR perm
  , fileGroupGroupW      = permW perm
  , fileGroupGroupX      = permX perm
  }


makeTag :: Text -> UTCTime -> Tag
makeTag tag created = Tag tag created

makeDirectoryTag :: DirectoryId -> TagId -> UTCTime -> DirectoryTag
makeDirectoryTag = DirectoryTag

makeFileTag :: FileId -> TagId -> UTCTime -> FileTag
makeFileTag = FileTag

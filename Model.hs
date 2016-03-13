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


makeUserAccount :: Text -> UserAccount
makeUserAccount ident  =
  UserAccount
  { userAccountIdent = ident
  , userAccountPrivileged = False
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


makeUserAccountWithUmask :: Text -> UMask -> UserAccount
makeUserAccountWithUmask ident umask  =
  UserAccount
  { userAccountIdent = ident
  , userAccountPrivileged = False
  , userAccountPassword = Nothing
  , userAccountDisplayName = Nothing
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




makeDirectory :: UserAccountId -> Text -> UMask -> Directory
makeDirectory uid name umask =
  Directory
  { directoryUserId = uid
  , directoryName   = name
  , directoryCode   = ""
  , directoryExplanation = ""

  , directoryOwnerR    = permissionOwnerR directoryDefaultPermission && not (umaskOwnerR umask)
  , directoryOwnerW    = permissionOwnerW directoryDefaultPermission && not (umaskOwnerW umask)
  , directoryOwnerX    = permissionOwnerX directoryDefaultPermission && not (umaskOwnerX umask)
  , directoryEveryoneR = permissionEveryoneR directoryDefaultPermission && not (umaskEveryoneR umask)
  , directoryEveryoneW = permissionEveryoneW directoryDefaultPermission && not (umaskEveryoneW umask)
  , directoryEveryoneX = permissionEveryoneX directoryDefaultPermission && not (umaskEveryoneX umask)
  }





makeFile :: UserAccountId -> DirectoryId -> Text -> UMask -> File
makeFile uid dir name umask =
  File
  { fileUserId = uid
  , fileDirectoryId = dir
  , fileName   = name
  , fileCode   = ""
  , fileExplanation = ""

  , fileOwnerR    = permissionOwnerR fileDefaultPermission && not (umaskOwnerR umask)
  , fileOwnerW    = permissionOwnerW fileDefaultPermission && not (umaskOwnerW umask)
  , fileOwnerX    = permissionOwnerX fileDefaultPermission && not (umaskOwnerX umask)
  , fileEveryoneR = permissionEveryoneR fileDefaultPermission && not (umaskEveryoneR umask)
  , fileEveryoneW = permissionEveryoneW fileDefaultPermission && not (umaskEveryoneW umask)
  , fileEveryoneX = permissionEveryoneX fileDefaultPermission && not (umaskEveryoneX umask)
  }


makeGroup :: Text -> UserAccountId -> Group
makeGroup name ownerId =
  Group
  { groupName  = name
  , groupOwner = ownerId
  , groupExplanation = ""
  }

makeGroupMembers :: GroupId -> UserAccountId -> GroupMembers
makeGroupMembers  gid uid =  GroupMembers gid uid

makeDirectoryGroups :: DirectoryId -> GroupId -> UMask -> DirectoryGroups
makeDirectoryGroups dir gid umask =
  DirectoryGroups
  { directoryGroupsDirectoryId = dir
  , directoryGroupsGroupId     = gid
  , directoryGroupsGroupR      = permissionGroupR directoryDefaultPermission && not (umaskGroupR umask)
  , directoryGroupsGroupW      = permissionGroupW directoryDefaultPermission && not (umaskGroupW umask)
  , directoryGroupsGroupX      = permissionGroupX directoryDefaultPermission && not (umaskGroupX umask)
  }

makeDirectoryGroupsWithPerm :: DirectoryId -> GroupId -> Perm -> DirectoryGroups
makeDirectoryGroupsWithPerm dir gid perm =
  DirectoryGroups
  { directoryGroupsDirectoryId = dir
  , directoryGroupsGroupId     = gid
  , directoryGroupsGroupR      = permR perm
  , directoryGroupsGroupW      = permW perm
  , directoryGroupsGroupX      = permX perm
  }


makeFileGroups :: FileId -> GroupId -> UMask -> FileGroups
makeFileGroups file gid umask =
  FileGroups
  { fileGroupsFileId      = file
  , fileGroupsGroupId     = gid
  , fileGroupsGroupR      = permissionGroupR fileDefaultPermission && not (umaskGroupR umask)
  , fileGroupsGroupW      = permissionGroupW fileDefaultPermission && not (umaskGroupW umask)
  , fileGroupsGroupX      = permissionGroupX fileDefaultPermission && not (umaskGroupX umask)
  }

makeFileGroupsWithPerm :: FileId -> GroupId -> Perm -> FileGroups
makeFileGroupsWithPerm file gid perm =
  FileGroups
  { fileGroupsFileId      = file
  , fileGroupsGroupId     = gid
  , fileGroupsGroupR      = permR perm
  , fileGroupsGroupW      = permW perm
  , fileGroupsGroupX      = permX perm
  }


makeTag :: Text -> Tag
makeTag tag = Tag tag

makeDirectoryTags :: DirectoryId -> TagId -> DirectoryTags
makeDirectoryTags = DirectoryTags

makeFileTags :: FileId -> TagId -> FileTags
makeFileTags = FileTags

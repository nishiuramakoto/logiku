{-# LANGUAGE OverloadedStrings #-}

module Constructors where

import Import.NoFoundation hiding (FileInfo)


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


data UserAccountInfo = UserAccountInfo
                       { userAccountInfoUserAccountId :: UserAccountId
                       , userAccountInfoIdent       :: Text
                       , userAccountInfoDisplayName :: Maybe Text
                       , userAccountInfoCreated     :: UTCTime
                       } deriving (Eq,Show)

data DirectoryInfo   = DirectoryInfo
                       { directoryInfoDirectoryId  :: DirectoryId
                       , directoryInfoUserId    :: UserAccountId
                       , directoryInfoName      :: Text
                       , directoryInfoCreated   :: UTCTime
                       , directoryInfoR         :: Bool
                       , directoryInfoW         :: Bool
                       , directoryInfoX         :: Bool
                       } deriving (Eq,Show)

data FileInfo        = FileInfo
                       { fileInfoFileId        :: FileId
                       , fileInfoUserId        :: UserAccountId
                       , fildInfoDirectoryId   :: DirectoryId
                       , fileInfoName          :: Text
                       , fileInfoCreated       :: UTCTime
                       , fileInfoModified      :: UTCTime
                       , fileInfoAccessed      :: UTCTime
                       , fileInfoR             :: Bool
                       , fileInfoW             :: Bool
                       , fileInfoX             :: Bool
                       } deriving (Eq,Show)

makeFileInfo :: FileId -> UserAccountId -> DirectoryId -> Text
                -> UTCTime -> UTCTime -> UTCTime -> Bool -> Bool -> Bool
                -> FileInfo
makeFileInfo = FileInfo

-- makePublicUserAccount :: UserAccount -> PublicUserAccount
-- makePublicUserAccount _user@UserAccount { userAccountIdent = ident
--                                         , userAccountDisplayName = displayName
--                                         , userAccountCreated = created
--                                         } =
--   PublicUserAccount ident displayName created

-- makePublicDirectory :: Directory -> PublicDirectory
-- makePublicDirectory _dir@Directory { directoryUserId = uid
--                                    , directoryName   = name
--                                    , directoryCreated = created
--                                    } =
--   PublicDirectory uid name created

-- makePublicFile :: File -> PublicFile
-- makePublicFile _file@File { fileUserId = uid
--                           , fileDirectoryId = dir
--                           , fileName   = name
--                           , fileCreated = created
--                           } =
--   PublicFile uid dir name created





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


makeUserAccount :: Text -> UTCTime -> UTCTime -> UTCTime -> UserAccount
makeUserAccount ident created modified accessed =
  UserAccount
  { userAccountIdent = ident
  , userAccountPrivileged = False
  , userAccountPassword = Nothing
  , userAccountDisplayName = Nothing
  , userAccountCreated     = created
  , userAccountModified    = modified
  , userAccountAccessed    = accessed
  , userAccountAccessCount = 0
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


makeUserAccountWithUmask :: Text -> UTCTime -> UTCTime -> UTCTime -> UMask -> UserAccount
makeUserAccountWithUmask ident created modified accessed umask  =
  UserAccount
  { userAccountIdent = ident
  , userAccountPrivileged = False
  , userAccountPassword = Nothing
  , userAccountDisplayName = Nothing
  , userAccountCreated     = created
  , userAccountModified    = modified
  , userAccountAccessed    = accessed
  , userAccountAccessCount = 0
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




makeDirectory :: UserAccountId -> Text -> UTCTime -> UTCTime -> UTCTime -> UMask -> Directory
makeDirectory uid name created modified accessed umask =
  Directory
  { directoryUserId = uid
  , directoryName   = name
  , directoryCode   = ""
  , directoryExplanation = ""
  , directoryCreated  = created
  , directoryModified = modified
  , directoryAccessed = accessed
  , directoryAccessCount = 0

  , directoryOwnerR    = permissionOwnerR directoryDefaultPermission && not (umaskOwnerR umask)
  , directoryOwnerW    = permissionOwnerW directoryDefaultPermission && not (umaskOwnerW umask)
  , directoryOwnerX    = permissionOwnerX directoryDefaultPermission && not (umaskOwnerX umask)
  , directoryEveryoneR = permissionEveryoneR directoryDefaultPermission && not (umaskEveryoneR umask)
  , directoryEveryoneW = permissionEveryoneW directoryDefaultPermission && not (umaskEveryoneW umask)
  , directoryEveryoneX = permissionEveryoneX directoryDefaultPermission && not (umaskEveryoneX umask)
  }



makeFile :: UserAccountId -> DirectoryId -> Text -> UTCTime -> UTCTime -> UTCTime -> UMask -> File
makeFile uid dir name created modified accessed umask =
  File
  { fileUserId = uid
  , fileDirectoryId = dir
  , fileName   = name
  , fileCode   = ""
  , fileExplanation = ""
  , fileCreated  = created
  , fileModified = modified
  , fileAccessed = accessed
  , fileAccessCount = 0

  , fileOwnerR    = permissionOwnerR fileDefaultPermission && not (umaskOwnerR umask)
  , fileOwnerW    = permissionOwnerW fileDefaultPermission && not (umaskOwnerW umask)
  , fileOwnerX    = permissionOwnerX fileDefaultPermission && not (umaskOwnerX umask)
  , fileEveryoneR = permissionEveryoneR fileDefaultPermission && not (umaskEveryoneR umask)
  , fileEveryoneW = permissionEveryoneW fileDefaultPermission && not (umaskEveryoneW umask)
  , fileEveryoneX = permissionEveryoneX fileDefaultPermission && not (umaskEveryoneX umask)
  }


makeGroup :: Text -> UserAccountId -> UTCTime -> UTCTime -> UTCTime -> Group
makeGroup name  ownerId created modified accessed =
  Group
  { groupName  = name
  , groupOwner = ownerId
  , groupCreated = created
  , groupModified = modified
  , groupAccessed = accessed
  , groupAccessCount = 0
  , groupExplanation = ""
  }

makeGroupMember :: GroupId -> UserAccountId ->  GroupMember
makeGroupMember  gid uid  =  GroupMember gid uid

makeDirectoryGroup :: DirectoryId -> GroupId -> UMask -> DirectoryGroup
makeDirectoryGroup dir gid  umask =
  DirectoryGroup
  { directoryGroupDirectoryId = dir
  , directoryGroupGroupId     = gid

  , directoryGroupGroupR      = permissionGroupR directoryDefaultPermission && not (umaskGroupR umask)
  , directoryGroupGroupW      = permissionGroupW directoryDefaultPermission && not (umaskGroupW umask)
  , directoryGroupGroupX      = permissionGroupX directoryDefaultPermission && not (umaskGroupX umask)
  }

makeDirectoryGroupWithPerm :: DirectoryId -> GroupId -> Perm -> DirectoryGroup
makeDirectoryGroupWithPerm dir gid  perm =
  DirectoryGroup
  { directoryGroupDirectoryId = dir
  , directoryGroupGroupId     = gid

  , directoryGroupGroupR      = permR perm
  , directoryGroupGroupW      = permW perm
  , directoryGroupGroupX      = permX perm
  }


makeFileGroup :: FileId -> GroupId -> UMask -> FileGroup
makeFileGroup file gid  umask =
  FileGroup
  { fileGroupFileId      = file
  , fileGroupGroupId     = gid

  , fileGroupGroupR      = permissionGroupR fileDefaultPermission && not (umaskGroupR umask)
  , fileGroupGroupW      = permissionGroupW fileDefaultPermission && not (umaskGroupW umask)
  , fileGroupGroupX      = permissionGroupX fileDefaultPermission && not (umaskGroupX umask)
  }

makeFileGroupWithPerm :: FileId -> GroupId ->  Perm -> FileGroup
makeFileGroupWithPerm file gid  perm =
  FileGroup
  { fileGroupFileId      = file
  , fileGroupGroupId     = gid

  , fileGroupGroupR      = permR perm
  , fileGroupGroupW      = permW perm
  , fileGroupGroupX      = permX perm
  }


makeTag :: Text -> Tag
makeTag tag  = Tag tag

makeDirectoryTag :: DirectoryId -> TagId -> DirectoryTag
makeDirectoryTag = DirectoryTag

makeFileTag :: FileId -> TagId ->  FileTag
makeFileTag = FileTag

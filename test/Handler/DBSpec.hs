{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -w #-}
module Handler.DBSpec (spec) where


import DBFS
import TestImport hiding((==.), on ,get, assert)
import qualified TestImport as I
import Database.Esqueleto

import Test.QuickCheck.Monadic
import Test.QuickCheck
import qualified Test.HUnit
import qualified Data.Text as T
import Data.Typeable

import Yesod.Persist.Core(YesodDB)
import Control.Monad.Logger(NoLoggingT)
import Control.Monad.Trans.Resource(ResourceT)
import Control.Monad.State hiding (get)
import GHC.Stack
import Network.Wai



sqlTest1 :: MonadIO m => SqlPersistT m [Entity UserAccount]
sqlTest1 = do people <- select $
                        from $ \user -> do
                          return user
              return people


sqlTest2 :: MonadIO m => SqlPersistT m [Entity UserAccount]
sqlTest2 = do people <- select $
                        from $ \user -> do
                          where_ (user ^. UserAccountUmaskEveryoneW ==. val True)
                          return user
              return people

sqlTest3 :: MonadIO m => SqlPersistT m [Entity Group]
sqlTest3 =  select $
            from $ \group -> do
              where_ (group ^. GroupExplanation ==. val "abc")
              return group


sqlTest4 :: MonadIO m => SqlPersistT m [(Entity Directory, Entity File)]
sqlTest4 = select $
           from $ \(dir,file) -> do
             where_ (dir^.DirectoryUserId ==. file^.FileUserId)
             orderBy [asc (dir^.DirectoryName)]
             return (dir,file)

sqlTest5 :: MonadIO m => SqlPersistT m [(Entity Directory, Maybe (Entity File))]
sqlTest5 = select $
           from $ \(dir `LeftOuterJoin` mfile) -> do
             on (just (dir^.DirectoryUserId) ==. mfile?.FileUserId)
             orderBy [asc (dir^.DirectoryName), asc (mfile?.FileName) ]
             return (dir,mfile)

sqlTest6 :: MonadIO m => SqlPersistT m [(Entity Directory , Entity File, Entity Group)]
sqlTest6 = select $
           from $ \(dir `InnerJoin` file `InnerJoin` group) -> do
             on (group^.GroupOwner ==. file^.FileUserId )
             on (dir^.DirectoryUserId ==. file^.FileUserId)
             return (dir,file,group)





setupTestDB1 :: YesodExample App ()
setupTestDB1 = do
  time <- liftIO $ getCurrentTime

  runDB $ do
      user1  <- insert $ makeUserAccount  "user1" time time time
      user2  <- insert $ makeUserAccount  "user2" time time time
      user3  <- insert $ makeUserAccount  "user3" time time time
      group1 <- insert $ (makeGroup "group1" user1 time time time) { groupExplanation = "abc" }
      Right group1   <- groupadd user1 "group2"
      Right dir1   <- mkdir user1 "dir1"
      Right dir2   <- mkdir user2 "dir2"
      Right dir3   <- mkdir user3 "dir3"
      Right dir1   <- mkdir user1 "dir4"
      Right dir2   <- mkdir user2 "dir5"
      Right dir3   <- mkdir user3 "dir6"
      Right file1  <- touch user1 dir1 "file1"
      Right file1  <- touch user1 dir1 "file2"
      Right file2  <- touch user2 dir2 "file3"
      Right file2  <- touch user2 dir2 "file4"

      Right u  <- usermod user1 user2 [ AddToGroup group1 ]
      Right u' <- usermod user2 user2 [ SetDisplayName "me" ]

      return ()


setupTestDB2 :: YesodExample App ()
setupTestDB2 =
  runDB $ do
    --root  <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
    root  <- su
    Right user1 <- root `useradd` "user1"
    Right user2 <- root `useradd` "user2"
    Right user3 <- root `useradd` "user3"

    Right group1 <- user1 `groupadd` "group1"
    Right group2 <- user2 `groupadd` "group2"
    Right group3 <- user3 `groupadd` "group3"

    Right u  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
    Right u' <- user2 `usermod` user2 $ [ SetDisplayName "me" ]
    return ()

sqlTest7 = do privileged <- select $
                            from $ \users -> do
                              where_ (users^.UserAccountPrivileged ==. val True)
                              return users
              return privileged


type TestDB a = SqlPersistM a

selectPrivilegedUsers =  select $
                         from $ \users -> do
                           where_ (users^.UserAccountPrivileged ==. val True)
                           return users

selectAll = do select $
                 from $ \groupMembers -> do
                   return groupMembers

selectUsers :: MonadIO m => SqlPersistT m [Entity UserAccount]
selectUsers = selectAll


deleteUser user = runDB $ do  Just root <- getBy $ UniqueUserAccount "root"
                              Just u    <- getBy $ UniqueUserAccount user
                              entityKey root `userdel` entityKey u

deleteGroup grp = runDB $ do  Just root <- getBy $ UniqueUserAccount "root"
                              Just g    <- getBy $ UniqueGroup grp
                              entityKey root `groupdel` entityKey g

deleteFromGroup user grp = runDB $ do Just root <- getBy $ UniqueUserAccount "root"
                                      Just u    <- getBy $ UniqueUserAccount  user
                                      Just g    <- getBy $ UniqueGroup grp
                                      entityKey root `usermod` entityKey u $ [DelFromGroup $ entityKey g]

selectUser user = runDB $ do Just u <- getBy $ UniqueUserAccount user
                             return $ entityVal u

dbIsEmpty :: SqlPersistM Bool
dbIsEmpty = do
  [] <- selectAll :: TestDB [Entity UserAccount]
  [] <- selectAll :: TestDB [Entity Directory]
  [] <- selectAll :: TestDB [Entity File]
  [] <- selectAll :: TestDB [Entity Group]
  [] <- selectAll :: TestDB [Entity GroupMember]
  [] <- selectAll :: TestDB [Entity DirectoryGroup]
  [] <- selectAll :: TestDB [Entity FileGroup]
  [] <- selectAll :: TestDB [Entity Tag]
  [] <- selectAll :: TestDB [Entity DirectoryTag]
  [] <- selectAll :: TestDB [Entity FileTag]
  return True


spec :: Spec
spec = withApp $ do

  -- TRUNCATE TABLE is Very slow. Consequently,
  -- I regreatably comment out all non-focused tests for each invocation.
  -- Using DELETE instead of TRUNCATE might be the cure of the problem.

  -- NOTE: DELETE is indeed faster but not fast enough.

  -- it "checks the type of a test" $ monadicYE $ do
  --   n1 <- pick (listOf (arbitrary :: Gen Char))
  --   pre (n1 /= "root")

  --   xs <- run $ do
  --       runDB $ do
  --         root  <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
  --         Right user1 <- root `useradd` (T.pack n1)
  --         return ()
  --       xs <- selectAll
  --       return (xs :: [Entity  UserAccount])

  --   assert (length xs == 2)

  it "leaves the user table empty" $ do
    users <- runDB $ selectList ([] :: [Filter UserAccount]) []
    assertEqual "user table empty" 0 $ length users

  it "retains entries after insertions" $ do
    setupTestDB1

    users <- runDB $ do
      selectList ([] :: [Filter UserAccount]) []
    length users `shouldBe` 3
    -- assertEqual "two users" 2 $ length users

  it "sets up a database with users" $ do
    setupTestDB1
    users <- runDB $ sqlTest1
    length users `shouldBe` 3

  it "contains users with default everyone write umask bit set to True" $ do
    setupTestDB1
    users <- runDB $ sqlTest2
    length users `shouldBe` 3


  it "contains a group with explanation=abc" $ do
    setupTestDB1
    groups <- runDB $ sqlTest3
    length groups `shouldBe` 1

  it "joins two tables based on the same user id" $ do
    setupTestDB1
    joined <- runDB $ sqlTest4
    length joined `shouldBe` 8

  it "left-outer-joins and gives three rows" $ do
    setupTestDB1
    joined <- runDB $ sqlTest5
    length joined `shouldBe` 10

  it "joins three tables in reverse order" $ do
    setupTestDB1
    joined <- runDB $ sqlTest6
    length joined `shouldBe` 8


  it "tests useradd and userdel" $ runDB $ do
    root  <- su
    Right user1 <- root `useradd` "user1"
    Right user2 <- root `useradd` "user2"
    Right user3 <- root `useradd` "user3"
    Left (UserAlreadyExists _)  <- root  `useradd` "user3"
    Left (PermissionError _  )  <- user1 `useradd` "user4"
    Left (PermissionError _  )  <- user1 `userdel` user2
    (length <$> selectPrivilegedUsers) `shouldReturn` 1

    Right _ <- root `userdel` user1
    (length <$> selectUsers) `shouldReturn` 3

    Right _ <- user2 `userdel` user2
    Right _ <- root  `userdel` user3

    Left (UserDoesNotExist _) <- root `userdel` user3

    Right _ <- root `userdel` root

    dbIsEmpty


  it "tests usermod" $ runDB $ do
    -- root  <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
    root  <- su
    Right user1 <- root `useradd` "user1"
    Right user2 <- root `useradd` "user2"
    Right user3 <- root `useradd` "user3"

    Right group1 <- user1 `groupadd` "group1"

    Right _  <- user1 `usermod` user1 $ [ AddToGroup group1 , SetDisplayName "name"]
    Right _  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
    Left (PermissionError _)  <- user1 `usermod` user2 $ [ SetDisplayName "new name" ]
    (getUserDisplayName user1) `shouldReturn` Right (Just "name")
    (getUserDisplayName user2) `shouldReturn` Right Nothing

    groupMembers <- selectAll :: TestDB [Entity GroupMember]
    length groupMembers `shouldBe` 2

    Left (NotAGroupMember _)   <- user1 `usermod` user2 $ [ DelFromGroup group1 , DelFromGroup group1]

    Right _ <- user1 `userdel` user1
    Right _ <- user2 `userdel` user2
    Right _ <- user3 `userdel` user3
    Right _ <- root  `userdel` root
    dbIsEmpty


  it "tests groupadd and groupdel" $ runDB $ do
    -- root  <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
    root  <- su
    Right user1 <- root `useradd` "user1"
    Right user2 <- root `useradd` "user2"
    Right user3 <- root `useradd` "user3"

    Right group1 <- user1 `groupadd` "group1"
    Right group2 <- user2 `groupadd` "group2"
    Right group3 <- user3 `groupadd` "group3"

    Right _  <- user1 `usermod` user1 $ [ AddToGroup group1 ]
    Right _  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
    Right _  <- user2 `usermod` user2 $ [ AddToGroup group2 ]
    Right _  <- user3 `usermod` user3 $ [ AddToGroup group3 ]

    groupMembers <- selectAll :: TestDB [Entity GroupMember]
    length groupMembers `shouldBe` 4

    groups <- selectAll :: TestDB [Entity Group]
    length groups `shouldBe` 3

    root `groupdel` group1

    groups <- selectAll :: TestDB [Entity Group]
    length groups `shouldBe` 2

    groupMembers <- selectAll :: TestDB [Entity GroupMember]
    length groupMembers `shouldBe` 2

    root `usermod` user3 $ [DelFromGroup group3]

    groupMembers <- selectAll :: TestDB [Entity GroupMember]
    length groupMembers `shouldBe` 1

    root `userdel` user1
    root `userdel` user2
    root `userdel` user3

    groups <- selectAll :: TestDB [Entity Group]
    length groups `shouldBe` 0

    groupMembers <- selectAll :: TestDB [Entity GroupMember]
    length groupMembers `shouldBe` 0

  it "creates directories with appropriate permissions" $ runDB $ do
    -- root  <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
    root  <- su
    Right user1 <- root `useradd` "user1"
    Right user2 <- root `useradd` "user2"
    Right user3 <- root `useradd` "user3"

    Right group1 <- user1 `groupadd` "group1"
    Right group2 <- user2 `groupadd` "group2"
    Right group3 <- user3 `groupadd` "group3"

    Right u  <- user1 `usermod` user1 $ [ AddToGroup group1 ]
    Right u  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
    Right u  <- user2 `usermod` user2 $ [ AddToGroup group2 ]
    Right u  <- user3 `usermod` user3 $ [ AddToGroup group3 ]

    Right dir0 <- root  `mkdir` "dir0"
    Right dir1 <- user1 `mkdir` "dir1"
    Right dir2 <- user2 `mkdir` "dir2"
    Right dir3 <- user3 `mkdir` "dir3"

    Right _ <- (user1 `chmodDirectory` dir1)
               [ ChmodOwner $ Perm True True True
               , ChmodGroup group2 $ Perm True True True
               , ChmodEveryone  $ Perm True True True
               ]

    Right _ <- (user2 `chmodDirectory`  dir2)
               [ ChmodOwner $ Perm True False False
               , ChmodGroup group1 $ Perm False True False
               , ChmodGroup group2 $ Perm False True False
               , ChmodGroup group3 $ Perm False True False
               , ChmodEveryone $ Perm False False True
               ]

    (dir0 `isDirectoryReadableBy` root) `shouldReturn` True

    -- Specify the reason for truth, and use catch-all for falsity

    -- test default umask (with permission bits 755) and access tests
    (dir0 `isDirectoryOwnerReadableBy`   root) `shouldReturn` True
    (dir0 `isDirectoryOwnerWritableBy`   root) `shouldReturn` True
    (dir0 `isDirectoryOwnerExecutableBy` root) `shouldReturn` True

    (dir0 `isDirectoryEveryoneReadableBy`   user1) `shouldReturn` True
    (dir0 `isDirectoryWritableBy`           user1) `shouldReturn` False
    (dir0 `isDirectoryEveryoneExecutableBy` user1) `shouldReturn` True

    -- test chmod and access tests
    (dir1 `isDirectoryOwnerReadableBy`   user1) `shouldReturn` True
    (dir1 `isDirectoryOwnerWritableBy`   user1) `shouldReturn` True
    (dir1 `isDirectoryOwnerExecutableBy` user1) `shouldReturn` True

    (dir1 `isDirectoryGroupReadableBy`   user2) `shouldReturn` True
    (dir1 `isDirectoryGroupWritableBy`   user2) `shouldReturn` True
    (dir1 `isDirectoryGroupExecutableBy` user2) `shouldReturn` True

    (dir1 `isDirectoryEveryoneReadableBy`   user3) `shouldReturn` True
    (dir1 `isDirectoryEveryoneWritableBy`   user3) `shouldReturn` True
    (dir1 `isDirectoryEveryoneExecutableBy` user3) `shouldReturn` True

    -- test chmod and access tests
    (dir2 `isDirectoryOwnerReadableBy`   user2) `shouldReturn` True
    (dir2 `isDirectoryOwnerWritableBy`   user2) `shouldReturn` False
    (dir2 `isDirectoryOwnerExecutableBy` user2) `shouldReturn` False
    (dir2 `isDirectoryGroupExecutableBy` user2) `shouldReturn` False
    (dir2 `isDirectoryEveryoneExecutableBy` user2) `shouldReturn` True

    (dir2 `isDirectoryReadableBy`   user3) `shouldReturn` False
    (dir2 `isDirectoryGroupWritableBy`   user3) `shouldReturn` True
    (dir2 `isDirectoryGroupExecutableBy` user3) `shouldReturn` False

    (dir2 `isDirectoryReadableBy`   user1) `shouldReturn` False
    (dir2 `isDirectoryGroupWritableBy`   user1) `shouldReturn` True
    (dir2 `isDirectoryEveryoneExecutableBy` user1) `shouldReturn` True


  it "creates files with appropriate permissions" $ do
    (root,user1,user2,user3,group1,group2,group3,dir0,dir1,dir2,dir3,file0,file1,file2,file3) <- runDB $ do

      root  <- su
      Right user1 <- root `useradd` "user1"
      Right user2 <- root `useradd` "user2"
      Right user3 <- root `useradd` "user3"

      Right group1 <- user1 `groupadd` "group1"
      Right group2 <- user2 `groupadd` "group2"
      Right group3 <- user3 `groupadd` "group3"

      Right u  <- user1 `usermod` user1 $ [ AddToGroup group1 ]
      Right u  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
      Right u  <- user2 `usermod` user2 $ [ AddToGroup group2 ]
      Right u  <- user3 `usermod` user3 $ [ AddToGroup group3 ]

      Right dir0 <- root  `mkdir` "dir0"
      Right dir1 <- user1 `mkdir` "dir1"
      Right dir2 <- user2 `mkdir` "dir2"
      Right dir3 <- user3 `mkdir` "dir3"

      Right file0 <- root  `touch` dir0 $ "file"
      Right file1 <- user1 `touch` dir1 $ "file"
      Right file2 <- user2 `touch` dir2 $ "file"
      Right file3 <- user3 `touch` dir3 $ "file"

      Right _ <- (user1 `chmodDirectory` dir1)
                 [ ChmodOwner $ Perm True True True
                 , ChmodGroup group1 $ Perm False False True
                 , ChmodEveryone $ Perm False False False ]

      Right _ <- (user1 `chmodFile` file1)
                 [ ChmodOwner $ Perm True True True
                 , ChmodGroup group2 $ Perm True True True
                 , ChmodEveryone $ Perm True True True ]

      Right _ <- (user2 `chmodFile`  file2)
                 [ ChmodOwner $  Perm True False False
                 , ChmodGroup group1 $ Perm False True False
                 , ChmodGroup group2 $ Perm False True False
                 , ChmodGroup group3 $ Perm False True False
                 , ChmodEveryone $ Perm False False True ]

      Right _ <- (user2 `chmodDirectory` dir2)
                 [ ChmodOwner $ Perm True True True
                 , ChmodGroup group2 $ Perm True True True
                 , ChmodEveryone $ Perm True True False ]

      return (root, user1,user2,user3,group1,group2,group3,dir0,dir1,dir2,dir3,file0,file1,file2,file3)

    Right _ <- runDB $ user1 `touch` dir1 $ "file"
    Left (PermissionError _  ) <- runDB $ user3 `touch` dir1 $ "file3"
    Left (PermissionError _  ) <- runDB $ user3 `touch` dir2 $ "file3"

    (runDB $ dir0 `isDirectoryReadableBy` root) `shouldReturn` True

    -- Specify the reason for truth, and use catch-all for falsity

    -- test default umask (with permission bits 755) and access tests
    (runDB $ file0 `isFileOwnerReadableBy`   root) `shouldReturn` True
    (runDB $ file0 `isFileOwnerWritableBy`   root) `shouldReturn` True
    (runDB $ file0 `isFileOwnerExecutableBy` root) `shouldReturn` True

    (runDB $ file0 `isFileEveryoneReadableBy`   user1) `shouldReturn` True
    (runDB $ file0 `isFileWritableBy`           user1) `shouldReturn` False
    (runDB $ file0 `isFileEveryoneExecutableBy` user1) `shouldReturn` True

    -- test chmod and access tests
    (runDB $ file1 `isFileOwnerReadableBy`   user1) `shouldReturn` True
    (runDB $ file1 `isFileOwnerWritableBy`   user1) `shouldReturn` True
    (runDB $ file1 `isFileOwnerExecutableBy` user1) `shouldReturn` True

    (runDB $ file1 `isFileGroupReadableBy`   user2) `shouldReturn` True
    (runDB $ file1 `isFileGroupWritableBy`   user2) `shouldReturn` True
    (runDB $ file1 `isFileGroupExecutableBy` user2) `shouldReturn` True

    (runDB $ file1 `isFileEveryoneReadableBy`   user3) `shouldReturn` True
    (runDB $ file1 `isFileEveryoneWritableBy`   user3) `shouldReturn` True
    (runDB $ file1 `isFileEveryoneExecutableBy` user3) `shouldReturn` True

    -- test chmod and access tests
    (runDB $ file2 `isFileOwnerReadableBy`   user2) `shouldReturn` True
    (runDB $ file2 `isFileOwnerWritableBy`   user2) `shouldReturn` False
    (runDB $ file2 `isFileOwnerExecutableBy` user2) `shouldReturn` False
    (runDB $ file2 `isFileGroupExecutableBy` user2) `shouldReturn` False
    (runDB $ file2 `isFileEveryoneExecutableBy` user2) `shouldReturn` True

    (runDB $ file2 `isFileReadableBy`        user3) `shouldReturn` False
    (runDB $ file2 `isFileGroupWritableBy`   user3) `shouldReturn` True
    (runDB $ file2 `isFileGroupExecutableBy` user3) `shouldReturn` False

    (runDB $ file2 `isFileReadableBy`           user1) `shouldReturn` False
    (runDB $ file2 `isFileGroupWritableBy`      user1) `shouldReturn` True
    (runDB $ file2 `isFileEveryoneExecutableBy` user1) `shouldReturn` True

  it "tests chown" $ do
    (root,user1,user2,user3,group1,group2,group3,dir0,dir1,dir2,dir3) <- runDB $ do
      root  <- su
      Right user1 <- root `useradd` "user1"
      Right user2 <- root `useradd` "user2"
      Right user3 <- root `useradd` "user3"

      Right group1 <- user1 `groupadd` "group1"
      Right group2 <- user2 `groupadd` "group2"
      Right group3 <- user3 `groupadd` "group3"

      Right u  <- user1 `usermod` user1 $ [ AddToGroup group1 ]
      Right u  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
      Right u  <- user2 `usermod` user2 $ [ AddToGroup group2 ]
      Right u  <- user3 `usermod` user3 $ [ AddToGroup group3 ]

      Right dir0 <- root  `mkdir` "dir0"
      Right dir1 <- user1 `mkdir` "dir1"
      Right dir2 <- user2 `mkdir` "dir2"
      Right dir3 <- user3 `mkdir` "dir3"

      return (root, user1,user2,user3,group1,group2,group3,dir0,dir1,dir2,dir3)

    (runDB $ getDirectoryUserId dir0) `shouldReturn` Right root
    (runDB $ getDirectoryUserId dir1) `shouldReturn` Right user1
    (runDB $ getDirectoryUserId dir2) `shouldReturn` Right user2
    (runDB $ getDirectoryUserId dir3) `shouldReturn` Right user3


--    Left (AlreadyOwner _)  <- runDB $ root `chownDirectory` dir1 $ [ ChownAddGroup group1 ]
    Left (AlreadyOwner _)  <- runDB $ root `chown`          dir1 $ [ ChownAddGroup group1 ]

    runDB $ do
      root `chown` dir1 $ [ ChownOwner user2
                          , ChownAddGroup group2
                          , ChownAddGroup group3
                          ]

    (runDB $ getDirectoryUserId dir1) `shouldReturn` Right user2
    Right gs <- runDB $ getDirectoryGroups dir1
    gs `shouldMatchList` [group1,group2,group3]


    (runDB $ dir1 `isDirectoryOwnerReadableBy` user3) `shouldReturn` False
    (runDB $ dir1 `isDirectoryOwnerWritableBy` user3) `shouldReturn` False
    (runDB $ dir1 `isDirectoryOwnerReadableBy` user3) `shouldReturn` False

    (runDB $ dir1 `isDirectoryGroupReadableBy` user3) `shouldReturn` True
    (runDB $ dir1 `isDirectoryGroupWritableBy` user3) `shouldReturn` False
    (runDB $ dir1 `isDirectoryGroupReadableBy` user3) `shouldReturn` True

    (runDB $ dir1 `isDirectoryEveryoneReadableBy` user3) `shouldReturn` True
    (runDB $ dir1 `isDirectoryEveryoneWritableBy` user3) `shouldReturn` False
    (runDB $ dir1 `isDirectoryEveryoneReadableBy` user3) `shouldReturn` True


    Left (PermissionError _) <- runDB $
      user1 `chown` dir2 $ [ ChownOwner user1 ]

    Right _ <- runDB $
      user2 `chown` dir2 $ [ ChownAddGroup group3 ]


    Right _ <- runDB $ do
      root `chown` dir1 $ [ ChownDelGroup group1
                          , ChownDelGroup group2
                          , ChownDelGroup group3
                          ]

    Right [] <- runDB $ getDirectoryGroups dir1

    return ()

  it "tests umask" $ do
    (root,user1,user2,user3,group1,group2,group3,dir0,dir1,dir2,dir3) <- runDB $ do
      root  <- su
      Right user1 <- root `useradd` "user1"
      Right user2 <- root `useradd` "user2"
      Right user3 <- root `useradd` "user3"

      Right group1 <- user1 `groupadd` "group1"
      Right group2 <- user2 `groupadd` "group2"
      Right group3 <- user3 `groupadd` "group3"

      Right _  <- user1 `usermod` user1 $ [ SetUmask $ UMask
                                            False False True
                                            False True  True
                                            True  True True ]

      Right u  <- user1 `usermod` user1 $ [ AddToGroup group1 ]
      Right u  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
      Right u  <- user2 `usermod` user2 $ [ AddToGroup group2 ]
      Right u  <- user3 `usermod` user3 $ [ AddToGroup group3 ]


      Right dir0 <- root  `mkdir` "dir0"
      Right dir1 <- user1 `mkdir` "dir1"
      Right dir2 <- user2 `mkdir` "dir2"
      Right dir3 <- user3 `mkdir` "dir3"

      return (root, user1,user2,user3,group1,group2,group3,dir0,dir1,dir2,dir3)


    runDB (get user1) >>= putStrLn . T.pack .  show
    (runDB $ dir1 `isDirectoryOwnerReadableBy` user1) `shouldReturn` True
    (runDB $ dir1 `isDirectoryOwnerWritableBy` user1) `shouldReturn` True
    (runDB $ dir1 `isDirectoryOwnerExecutableBy` user1) `shouldReturn` False

    (runDB $ dir1 `isDirectoryGroupReadableBy`  user2) `shouldReturn` True
    (runDB $ dir1 `isDirectoryGroupWritableBy`  user2) `shouldReturn` False
    (runDB $ dir1 `isDirectoryGroupExecutableBy` user2) `shouldReturn` False

    (runDB $ dir1 `isDirectoryEveryoneReadableBy` user3) `shouldReturn` False
    (runDB $ dir1 `isDirectoryEveryoneWritableBy` user3) `shouldReturn` False
    (runDB $ dir1 `isDirectoryEveryoneExecutableBy` user3) `shouldReturn` False

    return ()

  it "creates directories and lists them" $ do
    (root,user1,user2,user3,group1,group2,group3,dir0,dir1,dir2,dir3) <- runDB $ do
      root  <- su
      Right user1 <- root `useradd` "user1"
      Right user2 <- root `useradd` "user2"
      Right user3 <- root `useradd` "user3"

      Right group1 <- user1 `groupadd` "group1"
      Right group2 <- user2 `groupadd` "group2"
      Right group3 <- user3 `groupadd` "group3"

      Right u  <- user1 `usermod` user1 $ [ AddToGroup group1 ]
      Right u  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
      Right u  <- user2 `usermod` user2 $ [ AddToGroup group2 ]
      Right u  <- user3 `usermod` user3 $ [ AddToGroup group3 ]

      Right dir0 <- root  `mkdir` "dir0"
      Right dir1 <- user1 `mkdir` "dir1"
      Right dir2 <- user2 `mkdir` "dir2"
      Right dir3 <- user3 `mkdir` "dir3"

      Right _ <- (user1 `chmodDirectory` dir1)
                 [ ChmodOwner        $ Perm True  True True
                 , ChmodGroup group2 $ Perm False True True
                 , ChmodEveryone     $ Perm False True True
                 ]

      Right _ <- (user2 `chmodDirectory`  dir2)
                 [ ChmodOwner $ Perm True False False
                 , ChmodGroup group1 $ Perm False True False
                 , ChmodGroup group2 $ Perm False True False
                 , ChmodGroup group3 $ Perm False True False
                 , ChmodEveryone $ Perm False False True
                 ]

      Right _ <- (root `chmodDirectory`  dir3)
                 [ ChmodOwner        $ Perm False False False
                 , ChmodGroup group1 $ Perm False True False
                 , ChmodGroup group2 $ Perm False True False
                 , ChmodGroup group3 $ Perm False True False
                 , ChmodEveryone     $ Perm False False True
                 ]


      return (root,user1,user2,user3,group1,group2,group3,dir0,dir1,dir2,dir3)

    (runDB $ lsDirectory root  0 10) `shouldReturnRightMatchingList`  [dir0,dir1,dir2,dir3]
    (runDB $ lsDirectory user1 0 10) `shouldReturnRightMatchingList`  [dir0,dir1]
    (runDB $ lsDirectory user2 0 10) `shouldReturnRightMatchingList`  [dir0,dir1,dir2]
    (runDB $ lsDirectory user3 0 10) `shouldReturnRightMatchingList`  [dir0]


    return ()

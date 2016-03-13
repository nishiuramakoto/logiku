{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -w #-}
module Handler.DBSpec (spec) where


import DBFS
import TestImport hiding((==.), on , shouldBe , shouldReturn , get)
import qualified TestImport as I
import Database.Esqueleto

import qualified Data.Text as T

import Yesod.Persist.Core(YesodDB)
import Control.Monad.Logger(NoLoggingT)
import Control.Monad.Trans.Resource(ResourceT)
import Control.Monad.State hiding (get)
import GHC.Stack

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
setupTestDB1 =
    runDB $ do
      user1  <- insert $ makeUserAccount  "user1"
      user2  <- insert $ makeUserAccount  "user2"
      user3  <- insert $ makeUserAccount  "user3"
      group1 <- insert $ (makeGroup "group1" user1) { groupExplanation = "abc" }
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
    root  <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
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


type Test a = YesodExample App a

selectPrivilegedUsers = runDB  $ do  select $
                                       from $ \users -> do
                                         where_ (users^.UserAccountPrivileged ==. val True)
                                         return users

selectAll = runDB $ do select $
                         from $ \groupMembers -> do
                           return groupMembers


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

spec :: Spec
spec = withApp $ do

  -- This is a simple example of using a database access in a test.  The
  -- test will succeed for a fresh scaffolded site with an empty database,
  -- but will fail on an existing database with a non-empty user table.

  -- TRUNCATE TABLE is Very slow. Consequently,
  -- we regreatably comment out all non-focused tests for each invocation.
  -- Using DELETE instead of TRUNCATE might be the cure of the problem.

  -- it "leaves the user table empty" $ do
  --   users <- runDB $ selectList ([] :: [Filter UserAccount]) []
  --   assertEqual "user table empty" 0 $ length users

  -- it "retains entries after insertions" $ do
  --   setupTestDB1

  --   users <- runDB $ do
  --     selectList ([] :: [Filter UserAccount]) []
  --   length users `shouldBe` 3
  --   -- assertEqual "two users" 2 $ length users

  -- it "sets up a database with users" $ do
  --   setupTestDB1
  --   users <- runDB $ sqlTest1
  --   length users `shouldBe` 3

  -- it "contains users with default everyone write umask bit set to True" $ do
  --   setupTestDB1
  --   users <- runDB $ sqlTest2
  --   length users `shouldBe` 3


  -- it "contains a group with explanation=abc" $ do
  --   setupTestDB1
  --   groups <- runDB $ sqlTest3
  --   length groups `shouldBe` 1

  -- it "joins two tables based on the same user id" $ do
  --   setupTestDB1
  --   joined <- runDB $ sqlTest4
  --   length joined `shouldBe` 8

  -- it "left-outer-joins and gives three rows" $ do
  --   setupTestDB1
  --   joined <- runDB $ sqlTest5
  --   length joined `shouldBe` 10

  -- it "joins three tables in reverse order" $ do
  --   setupTestDB1
  --   joined <- runDB $ sqlTest6
  --   length joined `shouldBe` 8


  -- it "adds and deletes users appropriately" $ do
  --   -- setupTestDB2
  --   runDB $ do
  --     root  <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
  --     Right user1 <- root `useradd` "user1"
  --     Right user2 <- root `useradd` "user2"
  --     Right user3 <- root `useradd` "user3"

  --     Right group1 <- user1 `groupadd` "group1"
  --     Right group2 <- user2 `groupadd` "group2"
  --     Right group3 <- user3 `groupadd` "group3"

  --     Right u  <- user1 `usermod` user1 $ [ AddToGroup group1 ]
  --     Right u  <- user1 `usermod` user2 $ [ AddToGroup group1 ]
  --     Right u  <- user2 `usermod` user2 $ [ AddToGroup group2 ]
  --     Right u  <- user3 `usermod` user3 $ [ AddToGroup group3 ]
  --     Right u' <- user2 `usermod` user2 $ [ SetDisplayName "me" ]

  --     return ()

  --   user2 <- selectUser "user2"
  --   userAccountDisplayName user2 `shouldBe` Just "me"

  --   roots <- selectPrivilegedUsers
  --   length roots `shouldBe` 1

  --   groupMembers <- selectAll :: Test [Entity GroupMembers]
  --   length groupMembers `shouldBe` 4

  --   groups <- selectAll :: Test [Entity Group]
  --   length groups `shouldBe` 3

  --   deleteGroup "group1"

  --   groups <- selectAll :: Test [Entity Group]
  --   length groups `shouldBe` 2

  --   groupMembers <- selectAll :: Test [Entity GroupMembers]
  --   length groupMembers `shouldBe` 2

  --   deleteFromGroup "user3" "group3"

  --   groupMembers <- selectAll :: Test [Entity GroupMembers]
  --   length groupMembers `shouldBe` 1

  --   deleteUser "user1"
  --   users <- selectAll :: Test [Entity UserAccount]
  --   length users  `shouldBe` 3

  --   deleteUser "user2"
  --   deleteUser "user3"
  --   users <- selectAll :: Test [Entity UserAccount]
  --   length users  `shouldBe` 1

  --   groups <- selectAll :: Test [Entity Group]
  --   length groups `shouldBe` 0

  --   groupMembers <- selectAll :: Test [Entity GroupMembers]
  --   length groupMembers `shouldBe` 0


  it "creates directories with appropriate permissions" $ do
    (root,user1,user2,user3,dir0,dir1,dir2,dir3) <- runDB $ do
      root  <- insert $ (makeUserAccount  "root") { userAccountPrivileged = True }
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
        (Just $ Perm True True True)
        [(group2, Perm True True True)]
        (Just $ Perm True True True)

      Right _ <- (user2 `chmodDirectory`  dir2)
        (Just $ Perm True False False)
        [ (group1, Perm False True False)
        , (group2, Perm False True False)
        , (group3, Perm False True False) ]
        (Just $ Perm False False True)

      return (root,user1,user2,user3,dir0,dir1,dir2,dir3)

    (dir0 `isDirectoryReadableBy` root) `shouldReturn` True

    -- Specify the reason for truth, and use catch-all for falsity

    -- test default umask (with permission bits 755) and access tests
    -- (dir0 `isDirectoryOwnerReadableBy`   root) `shouldReturn` True
    -- (dir0 `isDirectoryOwnerWritableBy`   root) `shouldReturn` True
    -- (dir0 `isDirectoryOwnerExecutableBy` root) `shouldReturn` True

    -- (dir0 `isDirectoryEveryoneReadableBy`   user1) `shouldReturn` True
    -- (dir0 `isDirectoryWritableBy`           user1) `shouldReturn` False
    -- (dir0 `isDirectoryEveryoneExecutableBy` user1) `shouldReturn` True

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





shouldReturn :: (?loc :: CallStack) => (Show a, Eq a)
                  =>  SqlPersistM a -> a -> StateT (YesodExampleData App) IO ()
shouldReturn m a =
  do x <- runDB m
     liftIO $ x `I.shouldBe` a


shouldBe a b = liftIO $ a `I.shouldBe` b

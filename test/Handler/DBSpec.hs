module Handler.DBSpec (spec) where

import DBFS
import TestImport hiding((==.), on)
import Database.Esqueleto


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
              where_ (group ^. GroupExplanation ==. just (val "abc"))
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


setupTestDB :: YesodExample App ()
setupTestDB =
    runDB $ do
      user1  <- insert $ makeUser  "user1"
      user2  <- insert $ makeUser  "user2"
      group1 <- insert $ (makeGroup "group1" user1) { groupExplanation = Just "abc" }
      Just dir1   <- mkdir user1 "dir1"
      Just dir2   <- mkdir user2 "dir2"
      Just file1  <- touch user1 dir1 "file1"
      Just file2  <- touch user2 dir2 "file2"
      return ()

spec :: Spec
spec = withApp $ do

  -- This is a simple example of using a database access in a test.  The
  -- test will succeed for a fresh scaffolded site with an empty database,
  -- but will fail on an existing database with a non-empty user table.
  it "leaves the user table empty" $ do
    users <- runDB $ selectList ([] :: [Filter UserAccount]) []
    assertEqual "user table empty" 0 $ length users

  it "retains entries after insertions" $ do
    setupTestDB

    users <- runDB $ do
      selectList ([] :: [Filter UserAccount]) []
    assertEqual "two users" 2 $ length users

  it "sets up a database with users" $ do
    setupTestDB
    users <- runDB $ sqlTest1
    assertEqual "two users" 2 $ length users

  it "contains users with default everyone write umask bit set to True" $ do
    setupTestDB
    users <- runDB $ sqlTest2
    assertEqual "two users" 2 $ length users


  it "contains a group with explanation=abc" $ do
    setupTestDB
    groups <- runDB $ sqlTest3
    assertEqual "one group" 1 $ length groups

  it "joins two tables based on the same user id" $ do
    setupTestDB
    joined <- runDB $ sqlTest4
    assertEqual "two groups" 2 $ length joined

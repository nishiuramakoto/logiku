module Handler.DBSpec (spec) where

import DBFS
import TestImport

setupTestDB :: YesodExample App ()
setupTestDB =
    runDB $ do
      user1 <- insert $ makeUser  "user1"
      user2 <- insert $ makeUser  "user2"
      group1 <- insert $ (makeGroup "group1" user1) { groupExplanation = Just "abc" }
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

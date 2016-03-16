{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -w #-}
module Handler.DBQCSpec (spec) where


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




spec :: Spec
spec = withApp $ do

  it "creates directories and randomly associates permissions" $ monadicYESkip $ do
    (or,ow,ox) <- pick arbitrary
    (gr,gw,gx) <- pick arbitrary
    (ar,aw,ax) <- pick arbitrary

    (dir1,dir2,dir3,user1,user2,user3) <-  run $ runDB $ do
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
        (Just $ Perm or ow ox)
        [(group1, Perm gr gw gx)
        ,(group2, Perm gr gw gx)]
        (Just $ Perm ar aw ax)

      return (dir1,dir2,dir3,user1,user2,user3)

    (runDB $ dir1 `isDirectoryOwnerReadableBy` user1) `shouldBeM` or
    (runDB $ dir1 `isDirectoryOwnerWritableBy` user1) `shouldBeM` ow
    (runDB $ dir1 `isDirectoryOwnerExecutableBy` user1) `shouldBeM` ox
    (runDB $ dir1 `isDirectoryGroupReadableBy`   user2) `shouldBeM` gr
    (runDB $ dir1 `isDirectoryGroupWritableBy`   user2) `shouldBeM` gw
    (runDB $ dir1 `isDirectoryGroupExecutableBy`  user2) `shouldBeM` gx
    (runDB $ dir1 `isDirectoryEveryoneReadableBy` user3) `shouldBeM` ar
    (runDB $ dir1 `isDirectoryEveryoneWritableBy` user3) `shouldBeM` aw
    (runDB $ dir1 `isDirectoryEveryoneExecutableBy` user3) `shouldBeM` ax


  it "creates files and randomly associates permissions" $ monadicYESkip $ do
    (or,ow,ox) <- pick arbitrary
    (gr,gw,gx) <- pick arbitrary
    (ar,aw,ax) <- pick arbitrary

    (root,user1,user2,user3,dir0,dir1,dir2,dir3,file0,file1,file2,file3) <- run $ runDB $ do
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

      Right file0 <- root  `touchAt` dir0 $ "file"
      Right file1 <- user1 `touchAt` dir1 $ "file"
      Right file2 <- user2 `touchAt` dir2 $ "file"
      Right file3 <- user3 `touchAt` dir3 $ "file"

      Right _ <- (user1 `chmodDirectory` dir1)
        (Just $ Perm True True True)
        [(group2, Perm False False False)]
        (Just $ Perm False False False)

      Right _ <- (user1 `chmodFile` file1)
        (Just $ Perm or ow ox )
        [(group1, Perm gr gw gx)
        ,(group2, Perm gr gw gx)]
        (Just $ Perm ar aw ax)

      return (root,user1,user2,user3,dir0,dir1,dir2,dir3,file0,file1,file2,file3)


    (runDB $ file1 `isFileOwnerReadableBy` user1) `shouldBeM` or
    (runDB $ file1 `isFileOwnerWritableBy` user1) `shouldBeM` ow
    (runDB $ file1 `isFileOwnerExecutableBy` user1) `shouldBeM` ox
    (runDB $ file1 `isFileGroupReadableBy`   user2) `shouldBeM` gr
    (runDB $ file1 `isFileGroupWritableBy`   user2) `shouldBeM` gw
    (runDB $ file1 `isFileGroupExecutableBy`  user2) `shouldBeM` gx
    (runDB $ file1 `isFileEveryoneReadableBy` user3) `shouldBeM` ar
    (runDB $ file1 `isFileEveryoneWritableBy` user3) `shouldBeM` aw
    (runDB $ file1 `isFileEveryoneExecutableBy` user3) `shouldBeM` ax

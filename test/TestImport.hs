{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (trace)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.QuickCheck.Monadic as X hiding (assert)
import Test.QuickCheck  as X
import Test.Hspec            as X hiding(shouldBe,shouldReturn,shouldThrow, shouldMatchList)
-- import qualified Test.Hspec.Core.Spec  as Hspec
import qualified Test.Hspec  as H


import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X
import Yesod.Core.Dispatch
import Yesod.Core
import qualified Control.Monad.Trans.State as ST
import qualified Data.Text as T
import Data.Typeable
import Data.Time.LocalTime
import qualified Data.Map as M
import qualified Data.List as L
import GHC.Stack
import Network.Wai



runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


withApp :: SpecWith (TestApp App) -> Spec
withApp spec = beforeEachAndAll $ do
  settings <- loadAppSettings
              ["config/test-settings.yml", "config/settings.yml"]
              []
              ignoreEnv
  foundation <- makeFoundation settings
  wipeDB'  foundation
  logWare <- liftIO $ makeLogWare foundation
  return (foundation, logWare)
    where
      beforeEachAndAll eachIO = before eachIO (beforeAll_ initDB spec)


-- This gets run before all the specs exactly once
initDB :: IO ()
initDB = do
  return ()


-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
  tables <- getTables
  sqlBackend <- ask
  let escapedTables = map (connEscapeName sqlBackend . DBName) tables
      query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
  rawExecute query []


getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

-- Using DELETE FROM might be faster
wipeDB' :: App -> IO ()
wipeDB' app = runDBWithApp app $ do
  tables' <- getTables' -- manually topologically sorted
  tables  <- getTables
  sqlBackend <- ask
  let escapedTables = map (connEscapeName sqlBackend . DBName) (L.nub $ tables' ++ tables)
      queries = map (\t -> "DELETE FROM " ++ t) escapedTables

  forM_ queries (\query ->  rawExecute query [])

-- TODO: Should alter the tables to add CASCADE property for DELETEs to work flawlessly
--       without manual topological sorting. But that seems complex enough to need another
--       testing. For now I am reasonably content by the fact
--       that the breaking the order also breaks the execution and causes the test to fail.
getTables' :: MonadIO m => ReaderT SqlBackend m [Text]
getTables' = do
    return $ [ "blog"
             , "comment"
             , "email"
             , "file_json"
             , "file_tags"
             , "directory_tags"
             , "tag"
             , "file_groups"
             , "directory_groups"
             , "group_members"
             , "group"
             , "file"
             , "directory"
             , "user_account"
             ]

--------------  run monadic Quickcheck on Yesod sites ----------------

monadicYE :: forall site a. YesodDispatch site
             => PropertyM (ST.StateT (YesodExampleData site) IO) a
             -> (site,  Middleware)
             -> Property
monadicYE prop (site, middleware)  = monadic yesodProperty prop
  where
    yesodProperty ::  ST.StateT (YesodExampleData site) IO Property -> Property
    yesodProperty = ioProperty . runYE
    runYE :: ST.StateT (YesodExampleData site) IO Property -> IO Property
    runYE ex = do
      app <- toWaiAppPlain site
      ST.evalStateT ex YesodExampleData
        { yedApp  = middleware app
        , yedSite = site
        , yedCookies = M.empty
        , yedResponse = Nothing
        }

monadicYESkip :: forall site a. YesodDispatch site
             => PropertyM (ST.StateT (YesodExampleData site) IO) a
             -> (site,  Middleware)
             -> Property
monadicYESkip prop _  = monadic m prop
  where
    m = ioProperty . const (return True)


shouldThrow :: (?loc :: CallStack , Exception e , MonadIO m , MonadBaseControl IO m)
                =>  m a -> Selector e -> m ()
shouldThrow m catcher = do
  r <- try m
  case r of
    Right _ ->
      liftIO $ expectationFailure $
      "did not get expected exception: " ++ exceptionType
    Left e ->
      (\m' -> liftIO $ m' `expectTrue` catcher e) $
      "predicate failed on expected exception: " ++ exceptionType ++ " (" ++ show e ++ ")"
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) catcher
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "Test.Hspec.Expectations.shouldThrow: broken Typeable instance"

shouldReturn :: (?loc :: CallStack , Show a, Eq a, MonadIO m)
                  =>  m a -> a -> m ()
shouldReturn m a =
  do x <- m
     liftIO $ x `H.shouldBe` a


shouldReturnLeft :: (?loc :: CallStack , MonadIO m)
                    => m (Either a b) -> Something -> m  ()
shouldReturnLeft m _ = do
  e <- m
  case e of
    Left _ -> return ()
    Right _ -> liftIO $ False `H.shouldBe` True

data Something = Something
                 deriving (Eq,Show)


shouldMatchList :: (?loc :: CallStack , MonadIO m, Show a, Eq a)
                   => [a] -> [a] -> m ()
shouldMatchList as bs = liftIO $ as `H.shouldMatchList` bs

shouldReturnAndMatchList :: (?loc :: CallStack , MonadIO m, Show a, Eq a)
                            => m [a] -> [a] -> m ()
shouldReturnAndMatchList m bs = do
  as <- m
  liftIO $ as `H.shouldMatchList` bs

shouldReturnRightMatchingList :: (?loc :: CallStack , MonadIO m, Show a, Eq a)
                            => m (Either e [a]) -> [a] -> m ()
shouldReturnRightMatchingList m bs = do
  Right gs <- m
  liftIO $ gs `H.shouldMatchList` bs


shouldBe :: (?loc :: CallStack , Eq a, Show a, MonadIO m)
            => a -> a -> m()
shouldBe a b = liftIO $ a `H.shouldBe` b

expectTrue :: (?loc :: CallStack) =>  String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)


-- primitive tracing. Should use `(?loc ::  Callstack) =>'  instead.
trace :: MonadIO m => Text -> m ()
trace a = do
  ZonedTime localTime _zone  <- liftIO getZonedTime
  liftIO $ putStrLn $ T.concat [T.pack $ show localTime , ":" , a ]

--expectationFailure :: (?loc :: CallStack) =>  String -> Expectation
--expectationFailure = Test.HUnit.assertFailure

andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (m:ms) = do x <- m
                 y <- andM ms
                 return $ x && y

shouldBeM :: (?loc :: CallStack,  Monad m, Eq a, Show a) => m a -> a -> PropertyM m ()
shouldBeM m a = do
  x <- run m
  assert' (x == a)

assert' :: (?loc :: CallStack,  Monad m) => Bool -> PropertyM m ()
assert' True = return ()
assert' False = fail $ "Assertion falied at:" ++ showCallStack ?loc

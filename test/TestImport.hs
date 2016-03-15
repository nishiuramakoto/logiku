{-# LANGUAGE ImplicitParams #-}

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
import Test.Hspec            as X hiding(shouldBe,shouldReturn,shouldThrow)
import qualified Test.Hspec.Core.Spec  as Hspec
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
import GHC.Stack
import Network.Wai



runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  settings <- loadAppSettings
              ["config/test-settings.yml", "config/settings.yml"]
              []
              ignoreEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
  --trace "a"
  tables <- getTables
  --trace "b"
  sqlBackend <- ask
  --trace "c"
  let escapedTables = map (connEscapeName sqlBackend . DBName) tables
      query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
  rawExecute query []
  -- trace "d"

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables


-- run monadic Quickcheck on Yesod sites


newtype PropertyYesod site a =
  PropertyYesod {unPropertyYesod :: PropertyM (ST.StateT (YesodExampleData site) IO) a }
  deriving (Functor, Applicative, Monad)



runYesodProperty :: YesodDispatch site
                    => PropertyM (ST.StateT (YesodExampleData site) IO) a
                    -> (site,  Middleware)
                    -> Property
runYesodProperty prop (site, middleware)  = monadic yesodProperty prop
  where
--    yesodProperty :: YesodDispatch site => ST.StateT (YesodExampleData site) IO Property -> Property
    yesodProperty = (ioProperty :: IO Property -> Property) . run
--    run :: YesodDispatch site => ST.StateT (YesodExampleData site) IO Property -> IO Property
    run example = do
      app <- toWaiAppPlain site
      ST.evalStateT example YesodExampleData
        { yedApp  = middleware app
        , yedSite = site
        , yedCookies = M.empty
        , yedResponse = Nothing
        }


shouldThrow :: (?loc :: CallStack , Exception e)
                =>  SqlPersistM a -> Selector e -> ST.StateT (YesodExampleData App) IO ()
shouldThrow m catcher = do
  r <- try (runDB m)
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

shouldReturn :: (?loc :: CallStack , Show a, Eq a)
                  =>  SqlPersistM a -> a -> ST.StateT (YesodExampleData App) IO ()
shouldReturn m a =
  do x <- runDB m
     liftIO $ x `H.shouldBe` a

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

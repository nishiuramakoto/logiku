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
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X

import qualified Data.Text as T
import Data.Time.LocalTime

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  --trace "0"
  settings <- loadAppSettings
              ["config/test-settings.yml", "config/settings.yml"]
              []
              ignoreEnv
  --trace "1"
  foundation <- makeFoundation settings
  --trace "2"
  wipeDB foundation
  --trace "3"
  logWare <- liftIO $ makeLogWare foundation
  --trace "4"
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



trace a = do
  ZonedTime localTime zone  <- liftIO getZonedTime
  liftIO $ putStrLn $ T.pack $ show localTime ++ ":"  ++ a

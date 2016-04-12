module Handler.BuhiiSpec(spec) where

import TestImport
import qualified TestImport as I
import qualified Yesod.Test.TransversingCSS as CSS
import Yesod.Auth
import Test.QuickCheck.Monadic
import Test.QuickCheck
import qualified Test.HUnit
import Network.Wai.Test
import Network.HTTP.Types.Method

import qualified Data.Map as Map
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BL8
import Data.Time.LocalTime
import System.IO

import qualified Text.XML as XML
import qualified Text.XML.Cursor as XMLCursor


spec :: Spec
spec = withApp $ do
  it "tests the connection" $ do
    get ("http://kienizer.com/download/291519" :: String)
    statusIs 200


------------------------------------------------------------------

formActionValue query = do
  [form] <- htmlQuery query
  let cursor = parseHTML form
      XML.NodeElement (XML.Element name attr _nodes)   = XMLCursor.node cursor
      Just action = Map.lookup "action" attr
  return action

putTime = liftIO $ do
  withFile "/tmp/time.log" AppendMode $ \h -> do
    ZonedTime time tz <- getZonedTime
    I.hPutStrLn h $ show time

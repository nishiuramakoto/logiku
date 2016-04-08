module Handler.CCSpec(spec) where

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
    get ("http://localhost:3000" :: String)
    statusIs 303

  it "tests the form contents" $ do
    get ("http://localhost:3000/blog" :: String)
    statusIs 200
    htmlAllContain ".articleDetail .articleDetailHead .pageTitle"
      "Delimited continuations and nested transactions"

  it "tests the form structure" $ do
    get ("http://localhost:3000/blog" :: String)
    statusIs 200

    [form] <- htmlQuery "#blogLoginForm"

    let cursor = parseHTML form
        XML.NodeElement (XML.Element name attr _nodes)   = XMLCursor.node cursor
        Just action = Map.lookup "action" attr

    liftIO $ I.unpack action `shouldStartWith` "/blog/"

  it "tests the form submission" $ do
    get ("http://localhost:3000/blog" :: String)
    statusIs 200

    [form] <- htmlQuery "#blogLoginForm"

    let cursor = parseHTML form
        XML.NodeElement (XML.Element name attr _nodes)   = XMLCursor.node cursor
        Just action = Map.lookup "action" attr

    liftIO $ I.unpack action `shouldStartWith` "/blog/"

    let req = do setMethod methodPost
                 setUrl $ "http://localhost:3000" ++ I.unpack action
                 byLabel "Enter the user name:" "root"
                 byLabel "Enter the password" "root"
                 addToken
    I.request req
    statusIs 200

    bodyContains "You are logged in as root."


  it "tests the form submission" $ do
    login

  it "tests the form submission" $ do
    let i = 250
        i' = 2
    putTime
    forM [1..i'] $ \_ -> do
      login
      logout
      login
      newArticle
      cancel
      newArticle
      submit
      newArticle
      preview
      cancelPreview
      newArticle
      preview
      submitPreview

    putTime
----------------------------  Login Page  ----------------------------
login = do
    get ("http://localhost:3000/blog" :: String)
    statusIs 200

    action <- formActionValue "#blogLoginForm"
    liftIO $ I.unpack action `shouldStartWith` "/blog/"

    let req = do setMethod methodPost
                 setUrl $ "http://localhost:3000" ++ I.unpack action
                 byLabel "Enter the user name:" "root"
                 byLabel "Enter the password" "root"
                 addToken
    I.request req
    statusIs 200
    bodyContains "Blog View"
    bodyContains "You are logged in as root."

--------------------------  Blog view page  --------------------------
logout = do
  bodyContains "Blog View"
  bodyContains "You are logged in as root."

  action <- formActionValue ".articleDetailBody form"
  liftIO $ I.unpack action `shouldStartWith` "/blog/"

  let req = do setMethod methodPost
               setUrl $ "http://localhost:3000" ++ I.unpack action
               addPostParam "logout" "Logout"
               addToken
  I.request req
  statusIs 200
  bodyContains "Good bye"

newArticle = do
  bodyContains "Blog View"
  bodyContains "You are logged in as root."

  action <- formActionValue ".articleDetailBody form"
  liftIO $ I.unpack action `shouldStartWith` "/blog/"

  let req = do setMethod methodPost
               setUrl $ "http://localhost:3000" ++ I.unpack action
               addPostParam "new" "New Article"
               addToken
  I.request req
  statusIs 200
  bodyContains "Enter a new article"

-------------------------- New article page --------------------------
cancel = do
  bodyContains "Enter a new article"

  action <- formActionValue ".articleDetailBody form"
  liftIO $ I.unpack action `shouldStartWith` "/blog/"

  let req = do setMethod methodPost
               setUrl $ "http://localhost:3000" ++ I.unpack action
               byLabel "Subject:" "subject"
               byLabel "Body:" "body"
               addPostParam "cancel" "Cancel"
               addToken
  I.request req
  statusIs 200
  bodyContains "Blog View"
--  bodyContains "Enter a new article"
  bodyContains "You are logged in as root."

submit = do
  bodyContains "Enter a new article"

  action <- formActionValue ".articleDetailBody form"
  liftIO $ I.unpack action `shouldStartWith` "/blog/"

  let req = do setMethod methodPost
               setUrl $ "http://localhost:3000" ++ I.unpack action
               byLabel "Subject:" "subject"
               byLabel "Body:" "body"
               addPostParam "submit" "Submit"
               addToken
  I.request req
  statusIs 200
  bodyContains "Blog View"

preview = do
  bodyContains "Enter a new article"

  action <- formActionValue ".articleDetailBody form"
  liftIO $ I.unpack action `shouldStartWith` "/blog/"

  let req = do setMethod methodPost
               setUrl $ "http://localhost:3000" ++ I.unpack action
               byLabel "Subject:" "subject"
               byLabel "Body:" "body"
               addPostParam "preview" "Preview"
               addToken
  I.request req
  statusIs 200
  bodyContains "Preview your submission"

---------------------------- Preview page ----------------------------

cancelPreview = do
  bodyContains "Preview your submission"

  action <- formActionValue ".articleDetailBody form"
  liftIO $ I.unpack action `shouldStartWith` "/blog/"

  let req = do setMethod methodPost
               setUrl $ "http://localhost:3000" ++ I.unpack action
               addPostParam "cancel" "Cancel"
               addToken
  I.request req
  statusIs 200
  bodyContains "Blog View"
  bodyContains "You are logged in as root."

submitPreview = do
  bodyContains "Preview your submission"

  action <- formActionValue ".articleDetailBody form"
  liftIO $ I.unpack action `shouldStartWith` "/blog/"

  let req = do setMethod methodPost
               setUrl $ "http://localhost:3000" ++ I.unpack action
               addPostParam "submit" "Submit"
               addToken
  I.request req
  statusIs 200
  bodyContains "Blog View"
  bodyContains "You are logged in as root."


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

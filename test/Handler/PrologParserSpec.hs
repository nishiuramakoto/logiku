{-# OPTIONS_GHC -w #-}
module Handler.PrologParserSpec (spec) where

import Language.Prolog2.IO
import TestImport
import qualified TestImport as I

import Test.QuickCheck.Monadic
import Test.QuickCheck
import qualified Test.HUnit

prog1 = ""
goal1 = " X = 2*a + b*c , Y= +(*(2,a),*(b,c)) , X == Y"


resolve :: MonadIO m => Text -> Text -> m [[Term]]
resolve prog goal = do
  eterms <- evalPrologDatabaseT $ resolve' prog goal
  case eterms of
    Left err -> return []
    Right terms -> return terms


resolve' :: MonadLogger m => Text -> Text -> PrologDatabaseT m [[Term]]
resolve' prog goal = do
  sysdb <- liftProlog $ createSysDB
  est   <- liftProlog $ consultText prog
  tss <- case est of
    Right (Right st) -> do eterms <- liftProlog $ parseQuery st goal
                           case eterms of
                             Right (Right terms) -> resolveToTerms () "main" terms (database st) sysdb
                             _                   -> return []
    _  -> return []




spec :: Spec
spec = withApp $ do
  it "tests infix notation" $ do
    tss <- resolve prog1 goal1
    length tss `shouldBe` 1

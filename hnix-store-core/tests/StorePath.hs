{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module StorePath where

import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Text qualified

import System.Nix.StorePath (parseNameText, InvalidNameError(..))

spec_storePath :: Spec
spec_storePath = do
  describe "parseNameText" $ do
    it "parses valid name" $
      parseNameText "name-dev.dotok"
      `shouldBe`
      pure "name-dev.dotok"

    it "fails on empty" $
      parseNameText mempty
      `shouldBe`
      Left EmptyName

    it "fails on too long" $
      parseNameText (Data.Text.replicate 256 "n")
      `shouldBe`
      Left (NameTooLong 256)

    it "parses a leading dot" $
      parseNameText ".ab"
      `shouldBe`
      pure ".ab"

    it "fails on . or .. as the first dash-separated component" $
      fmap parseNameText [".", "..", ".-ab", "..-ab"]
      `shouldBe`
      replicate 4 (Left FirstComponentIsDotOrDotDot)

    it "fails on invalid characters" $
      parseNameText "ab!cd#@"
      `shouldBe`
      Left (InvalidCharacters "!#@")

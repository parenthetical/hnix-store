{-# LANGUAGE OverloadedStrings #-}

module StorePathSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)
import Test.QuickCheck ((===), (.&&.))

import System.Nix.Arbitrary ()
import System.Nix.StorePath

import Data.Attoparsec.Text qualified
import Data.Text qualified

spec :: Spec
spec = do
  describe "StorePath" $ do
    prop "roundtrips using parsePath . storePathToRawFilePath" $
      \storeDir ->
        roundtrips
          (storePathToRawFilePath storeDir)
          (parsePath storeDir)

    prop "roundtrips using parsePathFromText . storePathToText" $
      \storeDir ->
        roundtrips
          (storePathToText storeDir)
          (parsePathFromText storeDir)

    prop "roundtrips using pathParser . storePathToText" $
      \storeDir ->
        roundtrips
          (storePathToText storeDir)
          (Data.Attoparsec.Text.parseOnly $ pathParser storeDir)

    prop "roundtrips using parseBasePath . storePathBaseToRawFilePath" $
      roundtrips
        storePathBaseToRawFilePath
        parseBasePath

    prop "roundtrips using parseBasePathFromText . storePathBaseToText" $
      roundtrips
        storePathBaseToText
        parseBasePathFromText

  describe "parsePathAndSubpathFromText" $ do
    prop "splits off the path inside a store path" $
      \storeDir storePath segments ->
        let subpath = foldMap (("/" <>) . unStorePathName) (segments :: [StorePathName])
        in parsePathAndSubpathFromText storeDir (storePathToText storeDir storePath <> subpath)
             === Right (storePath, subpath)

    it "rejects a path outside the store" $
      parsePathAndSubpathFromText (StoreDir "/nix/store") "/usr/bin/ld"
        `shouldBe` Left (RootDirMismatch (StoreDir "/nix/store") (StoreDir "/usr/bin"))

  describe "sanitizeStorePathName" $ do
    prop "makes a valid name that fits in the length" $
      \maxLength string ->
        let name = sanitizeStorePathName maxLength (Data.Text.pack string)
        in mkStorePathName (unStorePathName name) === Right name
           .&&. Data.Text.length (unStorePathName name) <= max 1 (min storePathNameMaxLength maxLength)

    prop "keeps a valid name" $
      \name ->
        sanitizeStorePathName storePathNameMaxLength (unStorePathName name) === name

    it "replaces each run of invalid characters with a dash" $
      unStorePathName (sanitizeStorePathName storePathNameMaxLength "Foo'Bar Café 'é")
        `shouldBe` "Foo-Bar-Caf-"

    it "keeps the end of a long text and drops leading dots" $
      unStorePathName (sanitizeStorePathName 5 "abcd.efgh")
        `shouldBe` "efgh"

    it "makes a name of text without valid characters" $
      unStorePathName (sanitizeStorePathName storePathNameMaxLength "...")
        `shouldBe` "unknown"

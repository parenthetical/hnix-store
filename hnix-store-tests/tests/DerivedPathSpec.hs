{-# LANGUAGE OverloadedStrings #-}

module DerivedPathSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (forceRight, roundtrips)

import System.Nix.Arbitrary ()
import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..), SingleDerivedPath(..))
import System.Nix.OutputName (mkOutputName)
import System.Nix.StorePath (StoreDir(..), parseBasePathFromText)

import System.Nix.DerivedPath qualified

-- | The output @out@ of the derivation that the output @drv@ of
-- @foo.drv@ is
outputOfDynamicDerivation :: SingleDerivedPath
outputOfDynamicDerivation =
  SingleDerivedPath_Built
    (SingleDerivedPath_Built
      (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
      (forceRight $ mkOutputName "drv"))
    (forceRight $ mkOutputName "out")

spec :: Spec
spec = do
  describe "SingleDerivedPath" $ do
    prop "roundtrips" $ \sd ->
      roundtrips
        (System.Nix.DerivedPath.singleDerivedPathToText sd)
        (System.Nix.DerivedPath.parseSingleDerivedPath sd)

    prop "roundtrips in the legacy format" $ \sd ->
      roundtrips
        (System.Nix.DerivedPath.singleDerivedPathToTextLegacy sd)
        (System.Nix.DerivedPath.parseSingleDerivedPathLegacy sd)

    it "puts ^ before an output" $
      System.Nix.DerivedPath.singleDerivedPathToText (StoreDir "/nix/store") outputOfDynamicDerivation
        `shouldBe` "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv^drv^out"

    it "puts ! before an output in the legacy format" $
      System.Nix.DerivedPath.singleDerivedPathToTextLegacy (StoreDir "/nix/store") outputOfDynamicDerivation
        `shouldBe` "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv!drv!out"

  describe "DerivedPath" $ do
    prop "roundtrips" $ \sd ->
      roundtrips
        (System.Nix.DerivedPath.derivedPathToText sd)
        (System.Nix.DerivedPath.parseDerivedPath sd)

    prop "roundtrips in the legacy format" $ \sd ->
      roundtrips
        (System.Nix.DerivedPath.derivedPathToTextLegacy sd)
        (System.Nix.DerivedPath.parseDerivedPathLegacy sd)

    it "puts ^ before the outputs" $
      System.Nix.DerivedPath.derivedPathToText (StoreDir "/nix/store")
        (DerivedPath_Built outputOfDynamicDerivation OutputsSpec_All)
        `shouldBe` "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv^drv^out^*"

    it "puts ! before the outputs in the legacy format" $
      System.Nix.DerivedPath.derivedPathToTextLegacy (StoreDir "/nix/store")
        (DerivedPath_Built outputOfDynamicDerivation OutputsSpec_All)
        `shouldBe` "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv!drv!out!*"

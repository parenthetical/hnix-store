{-# LANGUAGE OverloadedStrings #-}

module StringContextSpec where

import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (forceRight, roundtrips)

import Data.Map qualified
import Data.Set qualified
import Data.Vector qualified

import System.Nix.Arbitrary ()
import System.Nix.Derivation (Derivation'(..), DerivationInputs(..), derivationInputsFromSingleDerivedPath)
import System.Nix.DerivedPath (SingleDerivedPath(..))
import System.Nix.OutputName (mkOutputName)
import System.Nix.StorePath (StoreDir(..), StorePath, mkStorePathName, parseBasePathFromText)
import System.Nix.StringContext

-- | The derivation in Nix's tests of string contexts
derivationX :: StorePath
derivationX = forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x.drv"

pathX :: StorePath
pathX = forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x"

built :: SingleDerivedPath -> Text -> SingleDerivedPath
built drvPath = SingleDerivedPath_Built drvPath . forceRight . mkOutputName

spec :: Spec
spec = do
  describe "StringContextElem" $ do
    -- The cases of Nix's src/libexpr-tests/value/context.cc
    it "reads and writes a store path" $ do
      parseStringContextElem "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x"
        `shouldBe` Right (StringContextElem_Path (SingleDerivedPath_Opaque pathX))
      stringContextElemToText (StringContextElem_Path (SingleDerivedPath_Opaque pathX))
        `shouldBe` "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x"

    it "reads and writes a derivation with its closure" $ do
      parseStringContextElem "=g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x.drv"
        `shouldBe` Right (StringContextElem_DrvDeep derivationX)
      stringContextElemToText (StringContextElem_DrvDeep derivationX)
        `shouldBe` "=g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x.drv"

    it "reads and writes an output" $ do
      let foo = StringContextElem_Path (built (SingleDerivedPath_Opaque derivationX) "foo")
      parseStringContextElem "!foo!g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x.drv" `shouldBe` Right foo
      stringContextElemToText foo `shouldBe` "!foo!g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x.drv"

    it "reads and writes an output of a dynamic derivation, outermost output first" $ do
      let fooOfBar = StringContextElem_Path (built (built (SingleDerivedPath_Opaque derivationX) "bar") "foo")
      parseStringContextElem "!foo!bar!g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x.drv" `shouldBe` Right fooOfBar
      stringContextElemToText fooOfBar `shouldBe` "!foo!bar!g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x.drv"

    it "rejects what Nix rejects" $
      fmap parseStringContextElem ["", "!", "!!/", "=/", "/", "foo!g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x.drv"]
        `shouldSatisfy` all isLeft

    prop "roundtrips" $
      roundtrips stringContextElemToText parseStringContextElem

  describe "ContextText" $ do
    it "joins the contexts of concatenated texts" $ do
      let storeDir = StoreDir "/nix/store"
          joined = storePathText storeDir pathX <> "/bin:" <> derivedPathText storeDir (built (SingleDerivedPath_Opaque derivationX) "out")
      textContext joined
        `shouldBe` Data.Set.fromList
          [ StringContextElem_Path (SingleDerivedPath_Opaque pathX)
          , StringContextElem_Path (built (SingleDerivedPath_Opaque derivationX) "out")
          ]

  describe "derivationFromContext" $
    it "takes the inputs from the context" $ do
      let storeDir = StoreDir "/nix/store"
          outputOfX = built (SingleDerivedPath_Opaque derivationX) "out"
          (derivation, closureDerivations) = derivationFromContext ContextDerivation
            { contextDerivationName = forceRight $ mkStorePathName "y"
            , contextDerivationOutputs = ()
            , contextDerivationPlatform = "x86_64-linux"
            , contextDerivationBuilder = storePathText storeDir pathX <> "/bin/sh"
            , contextDerivationArgs = Data.Vector.fromList ["-c", "cp -r \"$x\" $out"]
            , contextDerivationEnv = Data.Map.fromList
                [ ("x", derivedPathText storeDir outputOfX)
                , ("closure", ContextText "" (Data.Set.singleton (StringContextElem_DrvDeep derivationX)))
                ]
            }
      inputs derivation
        `shouldBe` DerivationInputs (Data.Set.singleton pathX) mempty
          <> derivationInputsFromSingleDerivedPath outputOfX
      builder derivation `shouldBe` "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-x/bin/sh"
      closureDerivations `shouldBe` Data.Set.singleton derivationX

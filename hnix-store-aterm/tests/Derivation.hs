{-# LANGUAGE OverloadedStrings #-}

module Derivation where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.QuickCheck (ioProperty, once, testProperty, (===))

import System.Nix.Derivation.ATerm (parseTraditionalDerivation, buildTraditionalDerivation)

import Data.Default.Class (Default(def))
import Data.Attoparsec.Text.Lazy qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.IO qualified
import Data.Text.Lazy.Builder qualified

processDerivation :: FilePath -> FilePath -> IO ()
processDerivation source dest = do
  contents <- Data.Text.Lazy.IO.readFile source
  either
    fail
    (Data.Text.Lazy.IO.writeFile dest
      . Data.Text.Lazy.Builder.toLazyText
      . buildTraditionalDerivation def
    )
    (Data.Attoparsec.Text.Lazy.parseOnly
      (parseTraditionalDerivation def)
      contents
    )

test_derivation :: TestTree
test_derivation =
  testGroup "golden" $ fmap mk [0 .. 3]
 where
  mk :: Int -> TestTree
  mk n =
    goldenVsFile
      ("derivation roundtrip of " <> drv)
      drv
      act
      (processDerivation drv act)
   where
    drv = fp <> show n <> ".drv"
    act = fp <> show n <> ".actual"
    fp  = "tests/example"

test_fixedOutputHashInBase32 :: TestTree
test_fixedOutputHashInBase32 =
  testProperty "reads a fixed output's hash in Nix's base 32 and writes it in base 16" $ once $ ioProperty $ do
    derivationWithHashInBase16 <- Data.Text.Lazy.IO.readFile "tests/example2.drv"
    let derivationWithHashInBase32 =
          Data.Text.Lazy.replace
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            "0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73"
            derivationWithHashInBase16
    pure $
      fmap (Data.Text.Lazy.Builder.toLazyText . buildTraditionalDerivation def)
        (Data.Attoparsec.Text.Lazy.parseOnly (parseTraditionalDerivation def) derivationWithHashInBase32)
      === Right derivationWithHashInBase16

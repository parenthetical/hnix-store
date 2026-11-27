{-# LANGUAGE OverloadedStrings #-}

module UpstreamDerivation where

import Data.Attoparsec.Text.Lazy qualified
import Data.Default.Class (Default(def))
import Data.Either (isLeft)
import Data.Map.Monoidal qualified
import Data.Set qualified
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder qualified
import Data.Text.Lazy.IO qualified
import Data.These (These(..))
import Test.Hspec.Nix (forceRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, counterexample, ioProperty, once, testProperty, (===))

import System.Nix.Derivation (ChildNode(..), DerivationInputs(..), DerivedPathMap(..), FreeformDerivationOutputs)
import System.Nix.Derivation.ATerm (buildDerivation, parseDerivation)
import System.Nix.Derivation.Traditional (TraditionalDerivation'(..))
import System.Nix.OutputName (mkOutputName)
import System.Nix.StorePath (parseBasePathFromText)

-- | Nix's own test data, from the submodule @upstream-nix@
upstreamDerivationFile :: FilePath -> FilePath
upstreamDerivationFile name = "upstream-libstore-data/derivation/" <> name

parse :: Text -> Either String (TraditionalDerivation' DerivationInputs FreeformDerivationOutputs)
parse = Data.Attoparsec.Text.Lazy.eitherResult
  . Data.Attoparsec.Text.Lazy.parse (parseDerivation def <* Data.Attoparsec.Text.Lazy.endOfInput)

render :: TraditionalDerivation' DerivationInputs FreeformDerivationOutputs -> Text
render = Data.Text.Lazy.Builder.toLazyText . buildDerivation def

rendersAsRead :: FilePath -> Property
rendersAsRead name = once $ ioProperty $ do
  text <- Data.Text.Lazy.IO.readFile (upstreamDerivationFile name)
  pure $ fmap render (parse text) === Right text

isRejected :: FilePath -> Property
isRejected name = once $ ioProperty $ do
  text <- Data.Text.Lazy.IO.readFile (upstreamDerivationFile name)
  let result = parse text
  pure $ counterexample (show result) $ isLeft result

-- | The inputs of @dyn-dep-derivation.drv@, as Nix's
-- @makeDynDepDerivation@ has them
dynDepDerivationInputs :: DerivationInputs
dynDepDerivationInputs = DerivationInputs
  { srcs = Data.Set.singleton $ forceRight $ parseBasePathFromText "c015dhfh5l0lp6wxyvdn7bmwhbbr6hr9-dep1"
  , drvs = DerivedPathMap $ Data.Map.Monoidal.singleton
      (forceRight $ parseBasePathFromText "c015dhfh5l0lp6wxyvdn7bmwhbbr6hr9-dep2.drv")
      (ChildNode $ These
        (Data.Set.fromList [outputName "cat", outputName "dog"])
        (Data.Map.Monoidal.fromList
          [ (outputName "cat", ChildNode $ This $ Data.Set.singleton $ outputName "kitten")
          , (outputName "goose", ChildNode $ This $ Data.Set.singleton $ outputName "gosling")
          ]))
  }
  where
    outputName = forceRight . mkOutputName

test_upstream :: TestTree
test_upstream = testGroup "upstream Nix test data"
  [ testProperty "renders simple-derivation.drv as read" $ rendersAsRead "simple-derivation.drv"
  , testProperty "renders dyn-dep-derivation.drv as read" $ rendersAsRead "dyn-dep-derivation.drv"
  , testProperty "reads the inputs of dyn-dep-derivation.drv" $ once $ ioProperty $ do
      text <- Data.Text.Lazy.IO.readFile (upstreamDerivationFile "dyn-dep-derivation.drv")
      pure $ fmap anonInputs (parse text) === Right dynDepDerivationInputs
  , testProperty "rejects dynamic inputs in the traditional version" $ isRejected "bad-old-version-dyn-deps.drv"
  , testProperty "rejects an unknown version" $ isRejected "bad-version.drv"
  ]

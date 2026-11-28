{-# LANGUAGE OverloadedStrings #-}

-- | Text that carries the store paths and derivation outputs it refers
-- to, as strings do in the Nix language. A derivation made of such text
-- gets its inputs from that context, as Nix's @derivation@ builtin does,
-- instead of from a separate list that has to agree with the text.
module System.Nix.StringContext
  ( StringContextElem(..)
  , stringContextElemToText
  , parseStringContextElem
  , StringContext
  , ContextText(..)
  , textWithoutContext
  , derivedPathText
  , storePathText
  , derivationInputsFromStringContext
  , ContextDerivation(..)
  , derivationFromContext
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Data.Set qualified
import Data.Text qualified

import System.Nix.Derivation
  ( Derivation'(..)
  , DerivationInputs
  , derivationInputsFromSingleDerivedPath
  )
import System.Nix.DerivedPath (SingleDerivedPath(..))
import System.Nix.OutputName (mkOutputName, outputNameToText)
import System.Nix.Placeholder (pathOrPlaceholderFromSingleDerivedPath)
import System.Nix.StorePath
  ( StoreDir
  , StorePath
  , StorePathName
  , parseBasePathFromText
  , storePathBaseToText
  )

-- | What a string refers to, as Nix's @NixStringContextElem@.
data StringContextElem
  = StringContextElem_Path SingleDerivedPath
  -- ^ A store path, or an output of a derivation, including an output of
  -- a derivation that is itself an output (a dynamic derivation)
  | StringContextElem_DrvDeep StorePath
  -- ^ A derivation with its whole build closure: every derivation in it
  -- with all of its outputs, as a derivation's @drvPath@ refers to it
  deriving (Eq, Generic, Ord, Show)

-- | Render as Nix does: a store path as its base name, a
-- 'StringContextElem_DrvDeep' as @=@ and the base name, and an output as
-- @!@, then each output name, outermost first, with a @!@ after it, then
-- the derivation's base name.
stringContextElemToText :: StringContextElem -> Text
stringContextElemToText = \case
  StringContextElem_Path (SingleDerivedPath_Opaque path) -> storePathBaseToText path
  StringContextElem_Path built -> "!" <> outputsAndDerivation built
  StringContextElem_DrvDeep drvPath -> "=" <> storePathBaseToText drvPath
 where
  outputsAndDerivation = \case
    SingleDerivedPath_Opaque path -> storePathBaseToText path
    SingleDerivedPath_Built drvPath outputName ->
      outputNameToText outputName <> "!" <> outputsAndDerivation drvPath

-- | Parse Nix's text form, as 'stringContextElemToText' renders it.
parseStringContextElem :: Text -> Either String StringContextElem
parseStringContextElem text = case Data.Text.uncons text of
  Nothing -> Left "a string context element can't be empty"
  Just ('!', outputsAndDerivation) -> case Data.Text.splitOn "!" outputsAndDerivation of
    components@(_ : _ : _) -> do
      derivation <- basePath (last components)
      outputNames <- traverse outputName (init components)
      pure
        $ StringContextElem_Path
        $ foldr
            (\outputName' drvPath -> SingleDerivedPath_Built drvPath outputName')
            (SingleDerivedPath_Opaque derivation)
            outputNames
    _ -> Left $ "an output in a string context needs an output name and a derivation: " <> show text
  Just ('=', drvPath) -> StringContextElem_DrvDeep <$> basePath drvPath
  Just _
    | "!" `Data.Text.isInfixOf` text -> Left $ "a store path in a string context can't have a !: " <> show text
    | otherwise -> StringContextElem_Path . SingleDerivedPath_Opaque <$> basePath text
 where
  basePath path = either (Left . show) Right $ parseBasePathFromText path
  outputName name = either (Left . show) Right $ mkOutputName name

type StringContext = Set StringContextElem

-- | Text with the context of the store paths and placeholders in it.
-- Concatenation joins the contexts.
data ContextText = ContextText
  { contextText :: Text
  , textContext :: StringContext
  }
  deriving (Eq, Generic, Ord, Show)

instance Semigroup ContextText where
  ContextText text context <> ContextText text' context' =
    ContextText (text <> text') (context <> context')

instance Monoid ContextText where
  mempty = ContextText mempty mempty

instance IsString ContextText where
  fromString = textWithoutContext . Data.Text.pack

textWithoutContext :: Text -> ContextText
textWithoutContext text = ContextText text mempty

-- | The store path, or the placeholder that Nix replaces with an output's
-- path, with the path as its context.
derivedPathText :: StoreDir -> SingleDerivedPath -> ContextText
derivedPathText storeDir path =
  ContextText
    (pathOrPlaceholderFromSingleDerivedPath storeDir path)
    (Data.Set.singleton (StringContextElem_Path path))

storePathText :: StoreDir -> StorePath -> ContextText
storePathText storeDir = derivedPathText storeDir . SingleDerivedPath_Opaque

-- | The inputs that a context gives a derivation, as Nix's
-- @derivationStrict@ computes them. The inputs of a
-- 'StringContextElem_DrvDeep' are every derivation in its closure, which
-- only a store knows, so those derivations come back on their own.
derivationInputsFromStringContext :: StringContext -> (DerivationInputs, Set StorePath)
derivationInputsFromStringContext = foldMap $ \case
  StringContextElem_Path path -> (derivationInputsFromSingleDerivedPath path, mempty)
  StringContextElem_DrvDeep drvPath -> (mempty, Data.Set.singleton drvPath)

-- | A derivation as the Nix language's @derivation@ builtin takes it: its
-- inputs are the context of its builder, arguments and environment.
data ContextDerivation outputs = ContextDerivation
  { contextDerivationName :: StorePathName
  , contextDerivationOutputs :: outputs
  , contextDerivationPlatform :: Text
  , contextDerivationBuilder :: ContextText
  , contextDerivationArgs :: Vector ContextText
  , contextDerivationEnv :: Map Text ContextText
  }
  deriving (Eq, Generic, Show)

-- | The derivation with the inputs of its context, and the derivations
-- whose closure the context refers to, as for
-- 'derivationInputsFromStringContext'.
derivationFromContext
  :: ContextDerivation outputs
  -> (Derivation' DerivationInputs outputs, Set StorePath)
derivationFromContext ContextDerivation{..} =
  ( Derivation
      { name = contextDerivationName
      , outputs = contextDerivationOutputs
      , inputs = derivationInputs
      , platform = contextDerivationPlatform
      , builder = contextText contextDerivationBuilder
      , args = contextText <$> contextDerivationArgs
      , env = contextText <$> contextDerivationEnv
      }
  , closureDerivations
  )
 where
  (derivationInputs, closureDerivations) =
    derivationInputsFromStringContext
      $ textContext contextDerivationBuilder
      <> foldMap textContext contextDerivationArgs
      <> foldMap textContext contextDerivationEnv

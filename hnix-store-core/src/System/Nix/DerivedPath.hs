{-# LANGUAGE OverloadedStrings #-}

module System.Nix.DerivedPath (
    OutputsSpec(..)
  , SingleDerivedPath(..)
  , parseSingleDerivedPath
  , singleDerivedPathToText
  , parseSingleDerivedPathLegacy
  , singleDerivedPathToTextLegacy
  , DerivedPath(..)
  , ParseOutputsError(..)
  , parseOutputsSpec
  , outputsSpecToText
  , parseDerivedPath
  , derivedPathToText
  , parseDerivedPathLegacy
  , derivedPathToTextLegacy
  ) where

import GHC.Generics (Generic)
import Data.Set (Set)
import Data.Text (Text)
import System.Nix.OutputName (OutputName, InvalidNameError)
import System.Nix.StorePath (StoreDir(..), StorePath, InvalidPathError)

import Data.Bifunctor qualified
import Data.ByteString.Char8 qualified
import Data.Set qualified
import Data.List qualified
import Data.Text qualified
import System.Nix.OutputName qualified
import System.Nix.StorePath qualified

data OutputsSpec =
    OutputsSpec_All
  -- ^ Wildcard spec (^*) meaning all outputs
  | OutputsSpec_Names (Set OutputName)
  -- ^ Set of specific outputs
  deriving (Eq, Generic, Ord, Show)

data SingleDerivedPath =
    SingleDerivedPath_Opaque StorePath
  -- ^ Fully evaluated store path that can't be built
  -- but can be fetched
  | SingleDerivedPath_Built SingleDerivedPath OutputName
  -- ^ Derivation path and the output built from it
  deriving (Eq, Generic, Ord, Show)

data DerivedPath =
    DerivedPath_Opaque StorePath
  -- ^ Fully evaluated store path that can't be built
  -- but can be fetched
  | DerivedPath_Built SingleDerivedPath OutputsSpec
  -- ^ Derivation path and the outputs built from it
  deriving (Eq, Generic, Ord, Show)

data ParseOutputsError =
    ParseOutputsError_InvalidPath InvalidPathError
  | ParseOutputsError_InvalidName InvalidNameError
  | ParseOutputsError_NoNames
  | ParseOutputsError_NoPrefix StoreDir Text
  deriving (Eq, Ord, Show)

parseOutputName :: Text -> Either ParseOutputsError OutputName
parseOutputName =
             ( Data.Bifunctor.first
                 ParseOutputsError_InvalidName
             . System.Nix.OutputName.mkOutputName
             )

parseOutputsSpec :: Text -> Either ParseOutputsError OutputsSpec
parseOutputsSpec t
  | t == "*" = Right OutputsSpec_All
  | otherwise = do
  names <- mapM
             parseOutputName
             (Data.Text.splitOn "," t)
  if null names
    then Left ParseOutputsError_NoNames
    else Right $ OutputsSpec_Names (Data.Set.fromList names)

outputsSpecToText :: OutputsSpec -> Text
outputsSpecToText = \case
  OutputsSpec_All -> "*"
  OutputsSpec_Names ns ->
    Data.Text.intercalate
      ","
      (fmap System.Nix.OutputName.outputNameToText
        (Data.Set.toList ns)
      )

-- | Parse a path with @^@ between a derivation and its output, as Nix
-- writes it on the command line
parseSingleDerivedPath
  :: StoreDir
  -> Text
  -> Either ParseOutputsError SingleDerivedPath
parseSingleDerivedPath = parseSingleDerivedPathWith "^"

-- | Parse a path with @!@ between a derivation and its output, as the
-- daemon protocol has it
parseSingleDerivedPathLegacy
  :: StoreDir
  -> Text
  -> Either ParseOutputsError SingleDerivedPath
parseSingleDerivedPathLegacy = parseSingleDerivedPathWith "!"

parseSingleDerivedPathWith
  :: Text
  -> StoreDir
  -> Text
  -> Either ParseOutputsError SingleDerivedPath
parseSingleDerivedPathWith separator root@(StoreDir sd) path =
  let -- We need to do a bit more legwork for case
      -- when StoreDir contains the separator
      -- between store path and output name(s).
      -- (The Arbitrary instance can generate such paths.)
    textRoot = Data.Text.pack
               $ Data.ByteString.Char8.unpack sd

  in case Data.Text.stripPrefix textRoot path of
    Nothing -> Left $ ParseOutputsError_NoPrefix root path
    Just woRoot ->
      case Data.Text.splitOn separator woRoot of
        [] -> error "internal error, this function should return NonEmpty"
        (pathNoPrefix : outputs) -> Data.List.foldl'
          (liftA2 SingleDerivedPath_Built)
          (SingleDerivedPath_Opaque
               <$> (convertError
                   $ System.Nix.StorePath.parsePathFromText
                      root
                      (textRoot <> pathNoPrefix)
                   ))
          (parseOutputName <$> outputs)

-- | Parse a path with @^@ between a derivation and its outputs, as Nix
-- writes it on the command line
parseDerivedPath
  :: StoreDir
  -> Text
  -> Either ParseOutputsError DerivedPath
parseDerivedPath = parseDerivedPathWith "^"

-- | Parse a path with @!@ between a derivation and its outputs, as the
-- daemon protocol has it
parseDerivedPathLegacy
  :: StoreDir
  -> Text
  -> Either ParseOutputsError DerivedPath
parseDerivedPathLegacy = parseDerivedPathWith "!"

parseDerivedPathWith
  :: Text
  -> StoreDir
  -> Text
  -> Either ParseOutputsError DerivedPath
parseDerivedPathWith separator root@(StoreDir sd) path =
  let -- We need to do a bit more legwork for case
      -- when StoreDir contains the separator
      -- between store path and output name(s).
      -- (The Arbitrary instance can generate such paths.)
    textRoot = Data.Text.pack
               $ Data.ByteString.Char8.unpack sd

  in case Data.Text.stripPrefix textRoot path of
    Nothing -> Left $ ParseOutputsError_NoPrefix root path
    Just woRoot ->
      case Data.Text.breakOnEnd separator woRoot of
        (r, suffix) ->
          if Data.Text.null r
          then DerivedPath_Opaque
               <$> (convertError
                   $ System.Nix.StorePath.parsePathFromText
                      root
                      path
                   )
          else DerivedPath_Built
               <$> parseSingleDerivedPathWith
                      separator
                      root
                      (textRoot <> Data.Text.dropEnd (Data.Text.length separator) r)
               <*> parseOutputsSpec suffix

convertError
  :: Either InvalidPathError a
  -> Either ParseOutputsError a
convertError = Data.Bifunctor.first ParseOutputsError_InvalidPath

-- | Render a path with @^@ between a derivation and its output, as Nix
-- writes it on the command line
singleDerivedPathToText :: StoreDir -> SingleDerivedPath -> Text
singleDerivedPathToText = singleDerivedPathToTextWith "^"

-- | Render a path with @!@ between a derivation and its output, as the
-- daemon protocol has it
singleDerivedPathToTextLegacy :: StoreDir -> SingleDerivedPath -> Text
singleDerivedPathToTextLegacy = singleDerivedPathToTextWith "!"

singleDerivedPathToTextWith :: Text -> StoreDir -> SingleDerivedPath -> Text
singleDerivedPathToTextWith separator root = \case
  SingleDerivedPath_Opaque p ->
    System.Nix.StorePath.storePathToText root p
  SingleDerivedPath_Built p o ->
    singleDerivedPathToTextWith separator root p
    <> separator
    <> System.Nix.OutputName.outputNameToText o

-- | Render a path with @^@ between a derivation and its outputs, as Nix
-- writes it on the command line
derivedPathToText :: StoreDir -> DerivedPath -> Text
derivedPathToText = derivedPathToTextWith "^"

-- | Render a path with @!@ between a derivation and its outputs, as the
-- daemon protocol has it
derivedPathToTextLegacy :: StoreDir -> DerivedPath -> Text
derivedPathToTextLegacy = derivedPathToTextWith "!"

derivedPathToTextWith :: Text -> StoreDir -> DerivedPath -> Text
derivedPathToTextWith separator root = \case
  DerivedPath_Opaque p ->
    System.Nix.StorePath.storePathToText root p
  DerivedPath_Built p os ->
    singleDerivedPathToTextWith separator root p
    <> separator
    <> outputsSpecToText os

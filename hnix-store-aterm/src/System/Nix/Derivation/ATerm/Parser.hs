{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Parsing logic

module System.Nix.Derivation.ATerm.Parser
    ( -- * Parser
      parseDerivation
    , parseDerivationWith
    , parseDerivationInputs
    , parseTraditionalDerivation
    , parseTraditionalDerivationWith
    , parseFreeformDerivationOutput
    , parseTraditionalDerivationInputs
    , textParser
    ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified
import Data.Attoparsec.Text.Lazy (Parser)
import Data.Attoparsec.Text.Lazy qualified
import Data.Map (Map)
import Data.Map qualified
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Set (Set)
import Data.Set qualified
import Data.Text (Text)
import Data.Text qualified
import Data.These (These(..))
import Data.Vector (Vector)
import Data.Vector qualified

import System.Nix.Derivation
    ( ChildNode(..)
    , DerivationInputs(..)
    , DerivedPathMap(..)
    , FreeformDerivationOutput(..)
    , FreeformDerivationOutputs
    )
import System.Nix.Derivation.Traditional
import System.Nix.StorePath
import System.Nix.OutputName

listOf :: Parser a -> Parser [a]
listOf element = do
    "["
    es <- Data.Attoparsec.Text.Lazy.sepBy element ","
    "]"
    pure es

-- | Parse a derivation in any version of the format
parseDerivation
  :: StoreDir
  -> Parser (TraditionalDerivation' DerivationInputs FreeformDerivationOutputs)
parseDerivation sd =
    parseDerivationWith
        (parseDerivationInputs sd)
        (\_ -> parseFreeformDerivationOutput sd)

-- | Parse a derivation in any version of the format using custom
-- parsers for derivation inputs/outputs. The parser of the inputs gets
-- the version from the derivation's header.
parseDerivationWith
    :: (DerivationATermVersion -> Parser drvInputs)
    -> (OutputName -> Parser drvOutput)
    -> Parser (TraditionalDerivation' drvInputs (Map OutputName drvOutput))
parseDerivationWith parseInputs parseOutput = do
    version <- traditionalHeader <|> versionedHeader
    parseDerivationFields (parseInputs version) parseOutput
  where
    traditionalHeader = do
        "Derive("
        pure DerivationATermVersion_Traditional

    versionedHeader = do
        "DrvWithVersion("
        versionName <- textParser
        version <- case versionName of
            "xp-dyn-drv" -> pure DerivationATermVersion_DynamicDerivations
            _ -> fail $ "unknown derivation ATerm format version " <> show versionName
        ","
        pure version

-- | Parse a derivation in the traditional version of the format
parseTraditionalDerivation
  :: StoreDir
  -> Parser (TraditionalDerivation' TraditionalDerivationInputs FreeformDerivationOutputs)
parseTraditionalDerivation sd =
    parseTraditionalDerivationWith
        (parseTraditionalDerivationInputs sd)
        (\_ -> parseFreeformDerivationOutput sd)

-- | Parse a derivation in the traditional version of the format using
-- custom parsers for derivation inputs/outputs
parseTraditionalDerivationWith
    :: Parser drvInputs
    -> (OutputName -> Parser drvOutput)
    -> Parser (TraditionalDerivation' drvInputs (Map OutputName drvOutput))
parseTraditionalDerivationWith parseInputs parseOutput = do
    "Derive("
    parseDerivationFields parseInputs parseOutput

-- | Parse the fields that follow the version header, and the closing
-- parenthesis
parseDerivationFields
    :: Parser drvInputs
    -> (OutputName -> Parser drvOutput)
    -> Parser (TraditionalDerivation' drvInputs (Map OutputName drvOutput))
parseDerivationFields parseInputs parseOutput = do
    let keyValue0 = do
            "("
            key <- outputNameParser
            ","
            drvOutput <- parseOutput key
            ")"
            return (key, drvOutput)
    anonOutputs <- mapOf keyValue0

    ","

    anonInputs <- parseInputs

    ","

    anonPlatform <- textParser

    ","

    anonBuilder <- textParser

    ","

    anonArgs <- vectorOf textParser

    ","

    let keyValue1 = do
            "("
            key <- textParser
            ","
            value <- textParser
            ")"
            pure (key, value)
    anonEnv <- mapOf keyValue1

    ")"

    pure TraditionalDerivation {..}

-- | Parse a derivation output
parseFreeformDerivationOutput :: StoreDir -> Parser FreeformDerivationOutput
parseFreeformDerivationOutput sd = do
    rawPath <- textParser
    ","
    rawMethodHashAlgo <- textParser
    ","
    rawHash <- textParser
    parseRawDerivationOutput sd $ RawDerivationOutput {..}

-- | Parse a derivation inputs
parseTraditionalDerivationInputs :: StoreDir -> Parser TraditionalDerivationInputs
parseTraditionalDerivationInputs sd = do
    traditionalDrvs <- mapOf $ do
         "("
         key <- storePathParser sd
         ","
         value <- setOf outputNameParser
         ")"
         pure (key, value)

    ","

    traditionalSrcs <- setOf $ storePathParser sd
    pure TraditionalDerivationInputs {..}

-- | Parse @DerivationInputs@ in the given version of the format. Only
-- the 'DerivationATermVersion_DynamicDerivations' version has inputs
-- that are outputs of dynamic derivations.
parseDerivationInputs :: StoreDir -> DerivationATermVersion -> Parser DerivationInputs
parseDerivationInputs sd version = do
    drvs <- fmap (DerivedPathMap . MonoidalMap) $ mapOf $ do
         "("
         key <- storePathParser sd
         ","
         node <- childNodeParser
         ")"
         pure (key, node)

    ","

    srcs <- setOf $ storePathParser sd
    pure DerivationInputs {..}
  where
    childNodeParser = case version of
        DerivationATermVersion_Traditional -> outputNamesNode
        DerivationATermVersion_DynamicDerivations -> outputNamesNode <|> nestedNode

    outputNamesNode = ChildNode . This <$> setOf outputNameParser

    nestedNode = do
        "("
        outputNames <- setOf outputNameParser
        ","
        children <- mapOf $ do
            "("
            outputName <- outputNameParser
            ","
            child <- childNodeParser
            ")"
            pure (outputName, child)
        ")"
        pure $ ChildNode $
            if | Data.Map.null children -> This outputNames
               | Data.Set.null outputNames -> That (MonoidalMap children)
               | otherwise -> These outputNames (MonoidalMap children)

textParser :: Parser Text
textParser = do
    "\""

    let predicate c = not (c == '"' || c == '\\')

    let loop = do
            text0 <- Data.Attoparsec.Text.takeWhile predicate

            char0 <- Data.Attoparsec.Text.anyChar

            case char0 of
                '"'  -> do
                    pure [ text0 ]

                _    -> do
                    char1 <- Data.Attoparsec.Text.anyChar

                    char2 <- case char1 of
                        'n' -> pure '\n'
                        'r' -> pure '\r'
                        't' -> pure '\t'
                        _   -> pure char1

                    textChunks <- loop

                    pure (text0 : Data.Text.singleton char2 : textChunks)

    Data.Text.concat <$> loop

outputNameParser :: Parser OutputName
outputNameParser = do
  n <- textParser
  case mkOutputName n of
    Left e -> fail $ show e -- TODO
    Right sp -> pure sp

storePathParser :: StoreDir -> Parser StorePath
storePathParser sd = do
  f <- textParser
  case System.Nix.StorePath.parsePathFromText sd f of
    Left e -> fail $ show e -- TODO
    Right sp -> pure sp

setOf :: Ord a => Parser a -> Parser (Set a)
setOf element = do
    es <- listOf element
    pure (Data.Set.fromList es)

vectorOf :: Parser a -> Parser (Vector a)
vectorOf element = do
    es <- listOf element
    pure (Data.Vector.fromList es)

mapOf :: Ord k => Parser (k, v) -> Parser (Map k v)
mapOf keyValue = do
    keyValues <- listOf keyValue
    pure (Data.Map.fromList keyValues)

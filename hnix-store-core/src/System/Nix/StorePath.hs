{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Description : Representation of Nix store paths.
-}

module System.Nix.StorePath
  ( -- * Basic store path types
    StoreDir(..)
  , HasStoreDir(..)
  , StorePath
  , storePathHash
  , storePathName
  , StorePathName
  , unStorePathName
  , StorePathHashPart
  , mkStorePathHashPart
  , unStorePathHashPart
    -- * Manipulating 'StorePathName'
  , InvalidNameError(..)
  , mkStorePathName
  , parseNameText
  , validStorePathNameChar
  , storePathNameMaxLength
  , sanitizeStorePathName
    -- * Reason why a path is not valid
  , InvalidPathError(..)
  , -- * Rendering out 'StorePath's
    storePathToFilePath
  , storePathToRawFilePath
  , storePathToText
  , storePathBaseToText
  , storePathBaseToRawFilePath
  , storePathToNarInfo
  , storePathHashPartToText
  , -- * Parsing 'StorePath's
    parseBasePath
  , parseBasePathFromText
  , parsePath
  , parsePathFromText
  , parsePathAndSubpathFromText
  , pathParser
    -- * Utilities for tests
  , unsafeMakeStorePath
  , unsafeMakeStorePathHashPart
  ) where

import Control.DeepSeq (NFData)
import Crypto.Hash (HashAlgorithm)
import Data.Attoparsec.Text.Lazy (Parser, (<?>))
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Default.Class (Default(def))
import Data.Hashable (Hashable(hashWithSalt))
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.Base (BaseEncoding(NixBase32))

import Data.Bifunctor qualified
import Data.ByteString.Char8 qualified
import Data.Char qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Attoparsec.Text.Lazy qualified
import System.FilePath qualified

import System.Nix.Base qualified
import System.Nix.Hash qualified
import System.Nix.Base32 qualified

-- | A path in a Nix store.
--
-- From the Nix thesis: A store path is the full path of a store
-- object. It has the following anatomy: storeDir/hashPart-name.
--
-- The store directory is *not* included, and must be known from the
-- context. This matches modern C++ Nix, and also represents the fact
-- that store paths for different store directories cannot be mixed.
data StorePath = StorePath
  { -- | The 160-bit hash digest reflecting the "address" of the name.
    -- Currently, this is a truncated SHA256 hash.
    storePathHash :: !StorePathHashPart
  , -- | The (typically human readable) name of the path. For packages
    -- this is typically the package name and version (e.g.
    -- hello-1.2.3).
    storePathName :: !StorePathName
  }
  deriving (Eq, Generic, Ord)

instance NFData StorePath

instance Hashable StorePath where
  hashWithSalt s StorePath{..} =
    s `hashWithSalt` storePathHash `hashWithSalt` storePathName

instance Show StorePath where
  show s =
    "StorePath"
    <> " "
    <> storePathToFilePath (StoreDir mempty) s

-- | The name portion of a Nix path.
--
-- 'unStorePathName' must only contain a-zA-Z0-9+._?=-, can't start
-- with a -, and must have at least one character (i.e. it must match
-- 'storePathNameRegex').
newtype StorePathName = StorePathName
  { -- | Extract the contents of the name.
    unStorePathName :: Text
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance NFData StorePathName

-- | The hash algorithm used for store path hashes.
newtype StorePathHashPart = StorePathHashPart
  { -- | Extract the contents of the hash.
    unStorePathHashPart :: ByteString
  }
  deriving (Eq, Generic, Hashable, Show)

-- | Implement the ordering used by Nix. In Nix's code base store paths are just
-- a newtype on Text, so its Ord is Text's. Nix's base-32 encoding prints the
-- digest's bytes from last to first, so we just flip our hash and compare.
instance Ord StorePathHashPart where
  compare (StorePathHashPart a) (StorePathHashPart b) =
    compare (Data.ByteString.reverse a) (Data.ByteString.reverse b)

instance NFData StorePathHashPart

-- | Make @StorePathHashPart@ from @ByteString@ (hash part of the @StorePath@)
-- using specific @HashAlgorithm@
mkStorePathHashPart
  :: forall hashAlgo
   . HashAlgorithm hashAlgo
  => ByteString
  -> StorePathHashPart
mkStorePathHashPart =
  StorePathHashPart
  . System.Nix.Hash.mkStorePathHash @hashAlgo

-- | Reason why a path name or output name is not valid
data InvalidNameError
  = EmptyName
  | NameTooLong Int
  | FirstComponentIsDotOrDotDot
  | InvalidCharacters Text
  deriving (Eq, Generic, Hashable, Ord, Show)

-- | Reason why a path is not valid
data InvalidPathError
  = PathNameInvalid InvalidNameError
  | HashDecodingFailure String
  | RootDirMismatch
      { rdMismatchExpected :: StoreDir
      , rdMismatchGot      :: StoreDir
      }
  deriving (Eq, Generic, Hashable, Ord, Show)

-- | Make @StorePathName@ from @Text@ (name part of the @StorePath@)
-- or fail with @InvalidNameError@ if it isn't valid
mkStorePathName :: Text -> Either InvalidNameError StorePathName
mkStorePathName = fmap StorePathName . parseNameText

-- | Parse name (either @StorePathName@ or @OutputName@)
parseNameText :: Text -> Either InvalidNameError Text
parseNameText n
  | n == ""
    = Left EmptyName
  | firstComponent == "." || firstComponent == ".."
    = Left FirstComponentIsDotOrDotDot
  | nLength > storePathNameMaxLength
    = Left $ NameTooLong nLength
  | not $ Data.Text.null invalidCharacters
    = Left $ InvalidCharacters invalidCharacters
  | otherwise = pure n
 where
  invalidCharacters = Data.Text.filter (not . validStorePathNameChar) n
  nLength = Data.Text.length n
  firstComponent = Data.Text.takeWhile (/= '-') n

-- | Whether a store path name or output name can have this character
validStorePathNameChar :: Char -> Bool
validStorePathNameChar c =
  any ($ c)
    [ Data.Char.isAsciiLower -- 'a'..'z', isAscii..er probably faster then putting it out
    , Data.Char.isAsciiUpper -- 'A'..'Z'
    , Data.Char.isDigit
    , (`elem` ("+-._?=" :: String))
    ]

storePathNameMaxLength :: Int
storePathNameMaxLength = 211

-- | Make a valid store path name from any text, as nixpkgs'
-- @lib.strings.sanitizeDerivationName@ does: replace each run of
-- characters that a name can't have with @-@, keep the end of the
-- text that fits in the given length, and drop leading dots. Text
-- that leaves nothing becomes @unknown@.
--
-- The length is at most 'storePathNameMaxLength'. A smaller one leaves
-- room for what Nix adds to a derivation's name: @.drv@, and an
-- output's name for the outputs other than @out@.
sanitizeStorePathName :: Int -> Text -> StorePathName
sanitizeStorePathName maxLength text =
  StorePathName
  $ if Data.Text.null name
    then Data.Text.take lengthCap "unknown"
    else name
 where
  lengthCap = max 1 (min storePathNameMaxLength maxLength)
  name =
    Data.Text.dropWhile (== '.')
    . Data.Text.takeEnd lengthCap
    . Data.Text.concat
    . fmap (\run -> if Data.Text.all validStorePathNameChar run then run else "-")
    . Data.Text.groupBy (\a b -> validStorePathNameChar a == validStorePathNameChar b)
    $ text

-- | Copied from @RawFilePath@ in the @unix@ package, duplicated here
-- to avoid the dependency.
type RawFilePath = ByteString

-- | The path to the store dir
--
-- Many operations need to be parameterized with this, since store paths
-- do not know their own store dir by design.
newtype StoreDir = StoreDir {
    unStoreDir :: RawFilePath
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance Default StoreDir where
  def = StoreDir "/nix/store"

class HasStoreDir r where
  hasStoreDir :: r -> StoreDir

-- | Render a 'StorePath' as a base path (without store directory).
-- This is the relative path format: @hashPart-name@
storePathBaseToRawFilePath :: StorePath -> RawFilePath
storePathBaseToRawFilePath StorePath{..} =
  hashPart <> "-" <> name
 where
  hashPart = Data.Text.Encoding.encodeUtf8 $ storePathHashPartToText storePathHash
  name     = Data.Text.Encoding.encodeUtf8 $ unStorePathName storePathName

-- | Render a 'StorePath' as a base path 'Text' (without store directory).
-- This is the relative path format: @hashPart-name@
storePathBaseToText :: StorePath -> Text
storePathBaseToText = Data.Text.Encoding.decodeUtf8 . storePathBaseToRawFilePath

-- | Render a 'StorePath' as a 'RawFilePath'.
storePathToRawFilePath :: StoreDir -> StorePath -> RawFilePath
storePathToRawFilePath storeDir sp =
  unStoreDir storeDir <> "/" <> storePathBaseToRawFilePath sp

-- | Render a 'StorePath' as a 'FilePath'.
storePathToFilePath :: StoreDir -> StorePath -> FilePath
storePathToFilePath storeDir = Data.ByteString.Char8.unpack . storePathToRawFilePath storeDir

-- | Render a 'StorePath' as a 'Text'.
storePathToText :: StoreDir -> StorePath -> Text
storePathToText storeDir =
  Data.Text.pack
  . Data.ByteString.Char8.unpack
  . storePathToRawFilePath storeDir

-- | Build `narinfo` suffix from `StorePath` which
-- can be used to query binary caches.
storePathToNarInfo :: StorePath -> ByteString
storePathToNarInfo StorePath{..} =
  Data.Text.Encoding.encodeUtf8
  $ System.Nix.Base.encodeWith NixBase32
      (unStorePathHashPart storePathHash) <> ".narinfo"

-- | Render a 'StorePathHashPart' as a 'Text'.
-- This is used by remote store / database
-- via queryPathFromHashPart
storePathHashPartToText :: StorePathHashPart -> Text
storePathHashPartToText =
  System.Nix.Base.encodeWith NixBase32 . unStorePathHashPart

-- | Parse a base name store path --- no store dir prefix.
--
-- Internal
parseBasePath'
  :: String
  -> Either InvalidPathError StorePath
parseBasePath' fname =
  let
    (storeBasedHashPart, namePart) = Data.Text.breakOn "-" $ Data.Text.pack fname
    hashPart =
      Data.Bifunctor.bimap
        HashDecodingFailure
        StorePathHashPart
        $ System.Nix.Base.decodeWith NixBase32 storeBasedHashPart
    name =
      Data.Bifunctor.first
        PathNameInvalid
        $ mkStorePathName . Data.Text.drop 1 $ namePart
  in
    StorePath <$> hashPart <*> name


-- | Parse `StorePath` from `String`, internal
parsePath'
  :: StoreDir
  -> String
  -> Either InvalidPathError StorePath
parsePath' expectedRoot stringyPath =
  let
    (rootDir, fname) = System.FilePath.splitFileName stringyPath
    --rootDir' = dropTrailingPathSeparator rootDir
    -- cannot use ^^ as it drops multiple slashes /a/b/// -> /a/b
    rootDir' = init rootDir
    expectedRootS = Data.ByteString.Char8.unpack (unStoreDir expectedRoot)
    storeDir =
      if expectedRootS == rootDir'
        then pure rootDir'
        else Left $ RootDirMismatch
                      { rdMismatchExpected = expectedRoot
                      , rdMismatchGot = StoreDir $ Data.ByteString.Char8.pack rootDir
                      }
  in
    either Left (pure $ parseBasePath' fname) storeDir

-- | Parse `StorePath` from `ByteString`.
-- This parses a base path (without store directory prefix).
parseBasePath
  :: ByteString
  -> Either InvalidPathError StorePath
parseBasePath = parseBasePath' . Data.ByteString.Char8.unpack

-- | Parse `StorePath` from `ByteString`, checking
-- that store directory matches `expectedRoot`.
parsePath
  :: StoreDir -- ^ expected @StoreDir@
  -> ByteString
  -> Either InvalidPathError StorePath
parsePath sd = parsePath' sd . Data.ByteString.Char8.unpack

-- | Parse `StorePath` from `Text`.
-- This parses a base path (without store directory prefix).
parseBasePathFromText
  :: Text
  -> Either InvalidPathError StorePath
parseBasePathFromText = parseBasePath' . Data.Text.unpack

-- | Parse `StorePath` from `Text`, checking
-- that store directory matches `expectedRoot`.
parsePathFromText
  :: StoreDir -- ^ expected @StoreDir@
  -> Text
  -> Either InvalidPathError StorePath
parsePathFromText sd = parsePath' sd . Data.Text.unpack

-- | Parse a path in the store into the `StorePath` and the rest of the
-- path, which is empty or starts with @/@, as Nix's @toStorePath@ does.
parsePathAndSubpathFromText
  :: StoreDir -- ^ expected @StoreDir@
  -> Text
  -> Either InvalidPathError (StorePath, Text)
parsePathAndSubpathFromText expectedRoot path =
  case Data.Text.stripPrefix (expectedRootT <> "/") path of
    Nothing ->
      Left $ RootDirMismatch
        { rdMismatchExpected = expectedRoot
        , rdMismatchGot =
            StoreDir
            . Data.ByteString.Char8.pack
            . System.FilePath.takeDirectory
            $ Data.Text.unpack path
        }
    Just inStore ->
      let (baseName, subpath) = Data.Text.breakOn "/" inStore
      in (\storePath -> (storePath, subpath)) <$> parseBasePathFromText baseName
 where
  expectedRootT =
      Data.Text.pack
    . Data.ByteString.Char8.unpack
    $ unStoreDir expectedRoot

-- | Attoparsec @StorePath@ @Parser@
pathParser :: StoreDir -> Parser StorePath
pathParser expectedRoot = do
  let expectedRootT =
          Data.Text.pack
        . Data.ByteString.Char8.unpack
        $ unStoreDir expectedRoot

  _ <- Data.Attoparsec.Text.Lazy.string expectedRootT
      <?> "Store root mismatch" -- e.g. /nix/store

  _ <- Data.Attoparsec.Text.Lazy.char '/'
      <?> "Expecting path separator"

  digest <-
    System.Nix.Base.decodeWith NixBase32
    <$> Data.Attoparsec.Text.Lazy.takeWhile1
      (`elem` System.Nix.Base32.digits32)
      <?> "Invalid Base32 part"

  _  <- Data.Attoparsec.Text.Lazy.char '-'
      <?> "Expecting dash (path name separator)"

  c0 <-
    Data.Attoparsec.Text.Lazy.satisfy
      validStorePathNameChar
      <?> "Leading path name character is invalid"

  rest <-
    Data.Attoparsec.Text.Lazy.takeWhile
    validStorePathNameChar
      <?> "Path name contains invalid character"

  let name =
        Data.Bifunctor.first
          PathNameInvalid
          $ mkStorePathName $ Data.Text.cons c0 rest
      hashPart =
        Data.Bifunctor.bimap
          HashDecodingFailure
          StorePathHashPart
          digest

  either
    (fail . show)
    pure
    (StorePath <$> hashPart <*> name)

-- * Utilities for tests

-- | Paths rarely need to be constructed directly.
-- Prefer @parsePath@ or @parsePathFromText@
unsafeMakeStorePath
  :: StorePathHashPart
  -> StorePathName
  -> StorePath
unsafeMakeStorePath = StorePath

-- | Path hash parts rarely need to be constructed directly.
-- Prefer @mkStorePathHashPart@
-- Used by remote store in wire protocol
unsafeMakeStorePathHashPart
  :: ByteString
  -> StorePathHashPart
unsafeMakeStorePathHashPart = StorePathHashPart

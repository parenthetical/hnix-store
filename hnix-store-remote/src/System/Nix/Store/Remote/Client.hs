module System.Nix.Store.Remote.Client
  ( addToStore
  , addToStoreNar
  , addToStoreScanning
  , addTextToStore
  , addSignatures
  , addTempRoot
  , addIndirectRoot
  , addPermRoot
  , buildPaths
  , buildDerivation
  , collectGarbage
  , ensurePath
  , findRoots
  , isValidPath
  , narFromPath
  , queryValidPaths
  , queryAllValidPaths
  , querySubstitutablePaths
  , queryPathInfo
  , queryReferrers
  , queryValidDerivers
  , queryDerivationOutputs
  , queryDerivationOutputNames
  , queryPathFromHashPart
  , queryMissing
  , optimiseStore
  , submitOutput
  , syncWithGC
  , verifyStore
  , module System.Nix.Store.Remote.Client.Core
  ) where

import Control.Exception (tryJust)
import Control.Monad (guard, unless, void, when)
import Control.Monad.Except (throwError)
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Some (Some)
import Data.Word (Word64)

import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Derivation.Traditional qualified
import System.Nix.Derivation.ATerm qualified
import System.Nix.DerivedPath (DerivedPath, SingleDerivedPath)
import System.Nix.Hash (HashAlgo(..))
import System.Nix.Nar (NarSource)
import System.Nix.OutputName (OutputName)
import System.Nix.Signature (Signature)
import System.Nix.StorePath (StorePath, StorePathHashPart, StorePathName)
import System.Nix.StorePath.Metadata (Metadata)
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Types.GC (GCOptions, GCResult, GCRoot)
import System.Nix.Store.Remote.Types.CheckMode (CheckMode)
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoFeature(..), hasFeature)
import System.Nix.Store.Remote.Types.Query.Missing (Missing)
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.StoreText (StoreText)
import System.Nix.Store.Remote.Types.SubstituteMode (SubstituteMode)
import System.Nix.Store.Remote.Client.Core
import System.Nix.ContentAddress (ContentAddressMethod)
import System.Nix.Store.Types (RepairMode(..))

import Control.Monad.IO.Class qualified
import Data.Attoparsec.Text qualified
import Data.ByteString qualified
import Data.ByteString.Char8 qualified
import Data.List qualified
import Data.Text.IO qualified
import GHC.Foreign qualified
import GHC.IO.Encoding qualified
import System.FilePath qualified
import System.IO.Error qualified
import System.Nix.Derivation qualified
import System.Nix.StorePath qualified
import System.Posix.ByteString (RawFilePath)
import System.Posix.Directory qualified
import System.Posix.Files qualified

-- | Add `NarSource` to the store
addToStore
  :: MonadRemoteStore m
  => StorePathName        -- ^ Name part of the newly created `StorePath`
  -> NarSource IO         -- ^ Provide nar stream
  -> ContentAddressMethod -- ^ Content addressing method
  -> Some HashAlgo        -- ^ Hashing algorithm
  -> Set StorePath        -- ^ References
  -> RepairMode           -- ^ Whether to overwrite an existing valid path, repairing corruption.
                          --   Sent to the daemon as part of the request since the 1.25 wire format.
  -> m StorePath
addToStore name source method hashAlgo refs repair = do
  setNarSource source
  doReq (AddToStore name method hashAlgo refs repair)

-- | Add `NarSource` to the store, letting the daemon scan the
-- contents for references instead of declaring them.
--
-- Only valid on connections where @add-to-store-scanning@ was
-- negotiated, i.e. inside a derivation build with the
-- @recursive-nix@ or @builder-rpc-v0@ system feature.
addToStoreScanning
  :: MonadRemoteStore m
  => StorePathName        -- ^ Name part of the newly created `StorePath`
  -> NarSource IO         -- ^ Provide nar stream
  -> ContentAddressMethod -- ^ Content addressing method
  -> Some HashAlgo        -- ^ Hashing algorithm
  -> m StorePath
addToStoreScanning name source method hashAlgo = do
  requireProtoFeature ProtoFeature_AddToStoreScanning
  setNarSource source
  doReq (AddToStoreScanning name method hashAlgo)

addToStoreNar
  :: MonadRemoteStore m
  => StorePath
  -> Metadata StorePath
  -> RepairMode
  -> CheckMode
  -> (Word64 -> IO(Maybe ByteString))
  -> m ()
addToStoreNar path metadata repair checkSigs source = do
  setDataSource source
  void $ doReq (AddToStoreNar path metadata repair checkSigs)
  pure ()

-- | Add @StoreText@ to the store
-- Reference accepts repair but only uses it
-- to throw error in case of remote talking to nix-daemon.
addTextToStore
  :: MonadRemoteStore m
  => StoreText
  -> HashSet StorePath -- ^ Set of `StorePath`s that the added text references
  -> RepairMode        -- ^ Repair mode, must be `RepairMode_DontRepair` for remote backend
                       --   (only valid for local store)
  -> m StorePath
addTextToStore stext references repair = do
  Control.Monad.when
    (repair == RepairMode_DoRepair)
    $ throwError RemoteStoreError_RapairNotSupportedByRemoteStore

  doReq (AddTextToStore stext references repair)

-- | Add @Signature@s to a store path
addSignatures
  :: MonadRemoteStore m
  => StorePath
  -> Set Signature
  -> m ()
addSignatures p signatures = (void . doReq) (AddSignatures p signatures)

-- | Add temporary garbage collector root.
--
-- This root is removed as soon as the client exits.
addTempRoot
  :: MonadRemoteStore m
  => StorePath
  -> m ()
addTempRoot = void . doReq . AddTempRoot

-- | Add the symlink at the given absolute path, which points into the
-- store, as an indirect garbage collector root. Its target stays alive
-- while the link points at it. 'addPermRoot' also makes the link.
addIndirectRoot
  :: MonadRemoteStore m
  => RawFilePath
  -> m ()
addIndirectRoot = void . doReq . AddIndirectRoot

-- | Point the symlink at the given path to the store path and add it as
-- an indirect garbage collector root, as Nix's
-- @IndirectRootStore::addPermRoot@ does. Returns the link's absolute
-- path. Fails when the path is in the store, or is a file other than a
-- symlink into the store.
addPermRoot
  :: MonadRemoteStore m
  => StorePath
  -> FilePath
  -> m FilePath
addPermRoot storePath link = do
  storeDir <- getStoreDir
  let storeDirPath = Data.ByteString.Char8.unpack (System.Nix.StorePath.unStoreDir storeDir)
      isInStore path = (storeDirPath <> "/") `Data.List.isPrefixOf` path
  gcRoot <- Control.Monad.IO.Class.liftIO $ absolutePath link
  when (isInStore gcRoot) $
    throwError $ RemoteStoreError_GCRootInStore gcRoot

  addTempRoot storePath

  existingFile <-
    Control.Monad.IO.Class.liftIO
    $ tryJust (guard . System.IO.Error.isDoesNotExistError)
    $ System.Posix.Files.getSymbolicLinkStatus gcRoot
  case existingFile of
    Left () -> pure ()
    Right status -> do
      linksIntoStore <-
        if System.Posix.Files.isSymbolicLink status
        then isInStore <$> Control.Monad.IO.Class.liftIO (System.Posix.Files.readSymbolicLink gcRoot)
        else pure False
      unless linksIntoStore $
        throwError $ RemoteStoreError_GCRootExists gcRoot

  Control.Monad.IO.Class.liftIO
    $ replaceSymbolicLink
        (System.Nix.StorePath.storePathToFilePath storeDir storePath)
        gcRoot
  addIndirectRoot =<< Control.Monad.IO.Class.liftIO (encodeFilePath gcRoot)
  pure gcRoot
 where
  absolutePath path =
    System.FilePath.normalise
    . (System.FilePath.</> path)
    <$> System.Posix.Directory.getWorkingDirectory

  -- The daemon reads the path as bytes.
  encodeFilePath path = do
    encoding <- GHC.IO.Encoding.getFileSystemEncoding
    GHC.Foreign.withCStringLen encoding path Data.ByteString.packCStringLen

  -- Make the link next to its final path and rename it into place, so
  -- that the path always names a link, as Nix's @replaceSymlink@ does.
  replaceSymbolicLink target path = go (0 :: Int)
   where
    go attempt = do
      let temporaryLink =
            System.FilePath.takeDirectory path
            System.FilePath.</> ("." <> show attempt <> "_" <> System.FilePath.takeFileName path)
      created <-
        tryJust (guard . System.IO.Error.isAlreadyExistsError)
        $ System.Posix.Files.createSymbolicLink target temporaryLink
      case created of
        Left () -> go (attempt + 1)
        Right () -> System.Posix.Files.rename temporaryLink path

-- | Build a derivation available at @StorePath@
buildDerivation
  :: MonadRemoteStore m
  => StorePath
  -> BuildMode
  -> m BuildResult
buildDerivation sp mode = do
  sd <- getStoreDir
  drvContents <-
    Control.Monad.IO.Class.liftIO
    $ Data.Text.IO.readFile
    $ System.Nix.StorePath.storePathToFilePath sd sp
  case Data.Attoparsec.Text.parseOnly
    (System.Nix.Derivation.ATerm.parseDerivation sd) drvContents of
      Left e -> throwError $ RemoteStoreError_DerivationParse e
      Right drv -> do
        let name = System.Nix.StorePath.storePathName sp
        outputs <- case
          System.Nix.Derivation.toSpecificOutputs sd name $
            System.Nix.Derivation.Traditional.anonOutputs drv
          of
           Nothing -> throwError $ RemoteStoreError_DerivationParse "TODO get error"
           Just os -> pure os
        let drv' = System.Nix.Derivation.Traditional.withName name $
              drv
                { System.Nix.Derivation.Traditional.anonOutputs = outputs
                , System.Nix.Derivation.Traditional.anonInputs =
                    System.Nix.Derivation.srcs
                      (System.Nix.Derivation.Traditional.anonInputs drv)
                }
        doReq (BuildDerivation sp drv' mode)

-- | Build paths if they are an actual derivations.
--
-- If derivation output paths are already valid, do nothing.
buildPaths
  :: MonadRemoteStore m
  => Set DerivedPath
  -> BuildMode
  -> m ()
buildPaths a b = (void . doReq) (BuildPaths a b)

collectGarbage
  :: MonadRemoteStore m
  => GCOptions
  -> m GCResult
collectGarbage = doReq . CollectGarbage

ensurePath
  :: MonadRemoteStore m
  => StorePath
  -> m ()
ensurePath = void . doReq . EnsurePath

-- | Find garbage collector roots.
findRoots
  :: MonadRemoteStore m
  => m (Map GCRoot StorePath)
findRoots = doReq FindRoots

isValidPath
  :: MonadRemoteStore m
  => StorePath
  -> m Bool
isValidPath = doReq . IsValidPath

-- | Download a NAR file.
narFromPath
  :: MonadRemoteStore m
  => StorePath -- ^ Path to generate a NAR for
  -> Word64 -- ^ Byte length of NAR
  -> (ByteString -> IO()) -- ^ Data sink where NAR bytes will be written
  -> m ()
narFromPath path narSize sink = do
  setDataSink sink
  setDataSinkSize narSize
  void $ doReq (NarFromPath path)

-- | Query valid paths from a set,
-- optionally try to use substitutes
queryValidPaths
  :: MonadRemoteStore m
  => HashSet StorePath
  -- ^ Set of @StorePath@s to query
  -> SubstituteMode
  -- ^ Try substituting missing paths when @SubstituteMode_DoSubstitute@
  -> m (HashSet StorePath)
queryValidPaths a b = doReq (QueryValidPaths a b)

-- | Query all valid paths
queryAllValidPaths
  :: MonadRemoteStore m
  => m (HashSet StorePath)
queryAllValidPaths = doReq QueryAllValidPaths

-- | Query a set of paths substituable from caches
querySubstitutablePaths
  :: MonadRemoteStore m
  => HashSet StorePath
  -> m (HashSet StorePath)
querySubstitutablePaths = doReq . QuerySubstitutablePaths

-- | Query path metadata
queryPathInfo
  :: MonadRemoteStore m
  => StorePath
  -> m (Maybe (Metadata StorePath))
queryPathInfo = doReq . QueryPathInfo

queryReferrers
  :: MonadRemoteStore m
  => StorePath
  -> m (HashSet StorePath)
queryReferrers = doReq . QueryReferrers

queryValidDerivers
  :: MonadRemoteStore m
  => StorePath
  -> m (HashSet StorePath)
queryValidDerivers = doReq . QueryValidDerivers

queryDerivationOutputs
  :: MonadRemoteStore m
  => StorePath
  -> m (HashSet StorePath)
queryDerivationOutputs = doReq . QueryDerivationOutputs

queryDerivationOutputNames
  :: MonadRemoteStore m
  => StorePath
  -> m (HashSet StorePathName)
queryDerivationOutputNames = doReq . QueryDerivationOutputNames

queryPathFromHashPart
  :: MonadRemoteStore m
  => StorePathHashPart
  -> m StorePath
queryPathFromHashPart = doReq . QueryPathFromHashPart

queryMissing
  :: MonadRemoteStore m
  => Set DerivedPath
  -> m Missing
queryMissing = doReq . QueryMissing

optimiseStore
  :: MonadRemoteStore m
  => m ()
optimiseStore = (void . doReq) OptimiseStore

-- | Register a store object as an output of the currently running
-- derivation.
--
-- Only valid on connections where @submit-output@ was negotiated,
-- i.e. inside a derivation build with the @builder-rpc-v0@ system
-- feature.
submitOutput
  :: MonadRemoteStore m
  => SingleDerivedPath
  -> OutputName
  -> m ()
submitOutput path output = do
  requireProtoFeature ProtoFeature_SubmitOutput
  void $ doReq (SubmitOutput path output)

syncWithGC
  :: MonadRemoteStore m
  => m ()
syncWithGC = (void . doReq) SyncWithGC

-- | Fail early with a clear error instead of letting the daemon
-- reject the opcode.
requireProtoFeature
  :: MonadRemoteStore m
  => ProtoFeature
  -> m ()
requireProtoFeature f = do
  pv <- getProtoVersion
  Control.Monad.when
    (not $ hasFeature f pv)
    $ throwError $ RemoteStoreError_ProtocolFeatureNotNegotiated f

verifyStore
  :: MonadRemoteStore m
  => CheckMode
  -> RepairMode
  -> m Bool
verifyStore check repair = doReq (VerifyStore check repair)

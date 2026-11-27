{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module System.Nix.Store.Remote
  ( module System.Nix.Store.Types
  , module System.Nix.Store.Remote.Client
  , module System.Nix.Store.Remote.MonadStore
  , module System.Nix.Store.Remote.Types
  -- * Compat
  , MonadStore
  -- * Daemon connections
  , defaultStoreSocketPath
  , parseStoreConnection
  , storeConnectionFromEnvironment
  -- * Runners
  , runStore
  , runStoreConnection
  , runStoreConnectionWithVersion
  , runStoreSocket
  , runStoreSocketWithVersion
  -- ** Daemon
  , runDaemon
  , runDaemonConnection
  , justdoit
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default.Class (Default(def))
import Network.Socket (Family, SockAddr(SockAddrUnix))
import System.Nix.Store.Types (RepairMode(..))
import System.Nix.Store.Remote.MonadStore
  ( runRemoteStoreT
  , MonadRemoteStore(..)
  , RemoteStoreT
  , RemoteStoreError(RemoteStoreError_GetAddrInfoFailed))
import System.Nix.Store.Remote.Client
import System.Nix.Store.Remote.Server (WorkerHelper, runProxyDaemon)
import System.Nix.Store.Remote.Types

import Control.Monad.Catch qualified
import Data.Text (Text)
import Data.Text qualified
import Network.Socket qualified
import System.Environment qualified
import System.FilePath qualified
-- see TODO bellow
--import System.Directory qualified

-- wip justdoit
import System.Nix.StorePath (StorePath)
import System.Nix.StorePath qualified

-- * Compat

type MonadStore = RemoteStoreT IO

-- * Daemon connections

-- | The daemon's socket: @NIX_DAEMON_SOCKET_PATH@, or else
-- @daemon-socket/socket@ in @NIX_STATE_DIR@, which is @/nix/var/nix@ by
-- default, as Nix's @getDaemonSocketPath@ finds it.
defaultStoreSocketPath :: IO StoreSocketPath
defaultStoreSocketPath = do
  socketPath <- nonEmptyEnvironmentVariable "NIX_DAEMON_SOCKET_PATH"
  stateDirectory <- nonEmptyEnvironmentVariable "NIX_STATE_DIR"
  pure $ StoreSocketPath $ case socketPath of
    Just path -> path
    Nothing ->
      maybe "/nix/var/nix" id stateDirectory
      System.FilePath.</> "daemon-socket"
      System.FilePath.</> "socket"
 where
  nonEmptyEnvironmentVariable name =
    (\case
      Just value | not (null value) -> Just value
      _ -> Nothing)
    <$> System.Environment.lookupEnv name

-- | The daemon connection that a Nix store URL names. Nix opens an
-- @auto@ or empty store itself when it can write it, but a client of
-- this library can only talk to a daemon, so here they always name the
-- default socket.
parseStoreConnection
  :: StoreSocketPath
  -> Text
  -> Either String StoreConnection
parseStoreConnection defaultSocket storeUrl =
  case Data.Text.breakOn "?" storeUrl of
    (storeWithoutSettings, _settings)
      | storeWithoutSettings `elem` ["", "auto", "daemon", "unix://"] ->
          Right $ StoreConnection_Socket defaultSocket
      | Just socketPath <- Data.Text.stripPrefix "unix://" storeWithoutSettings ->
          Right $ StoreConnection_Socket $ StoreSocketPath $ Data.Text.unpack socketPath
      | otherwise ->
          Left $ "not a Nix daemon's store: " <> Data.Text.unpack storeUrl

-- | The daemon connection that @NIX_REMOTE@ names, as
-- 'parseStoreConnection' reads it, with 'defaultStoreSocketPath'.
storeConnectionFromEnvironment :: IO (Either String StoreConnection)
storeConnectionFromEnvironment = do
  defaultSocket <- defaultStoreSocketPath
  storeUrl <- maybe "" Data.Text.pack <$> System.Environment.lookupEnv "NIX_REMOTE"
  pure $ parseStoreConnection defaultSocket storeUrl

-- * Runners

runStore
  :: ( MonadIO m
     , MonadMask m
     )
  => RemoteStoreT m a
  -> Run m a
runStore = runStoreConnection def

runStoreConnection
  :: ( MonadIO m
     , MonadMask m
     )
  => StoreConnection
  -> RemoteStoreT m a
  -> Run m a
runStoreConnection = runStoreConnectionWithVersion def

-- | Like 'runStoreConnection', but offers the given protocol version
-- and features in the handshake, such as 'builderRpcV0'
runStoreConnectionWithVersion
  :: ( MonadIO m
     , MonadMask m
     )
  => ProtoVersion
  -> StoreConnection
  -> RemoteStoreT m a
  -> Run m a
runStoreConnectionWithVersion offeredVersion sc k =
  connectionToSocket sc
  >>= \case
    Left e -> pure (Left e, mempty)
    Right (fam, sock) -> runStoreSocketWithVersion offeredVersion fam sock k

runStoreSocket
  :: ( MonadIO m
     , MonadMask m
     )
  => Family
  -> SockAddr
  -> RemoteStoreT m a
  -> Run m a
runStoreSocket = runStoreSocketWithVersion def

-- | Like 'runStoreSocket', but offers the given protocol version and
-- features in the handshake, such as 'builderRpcV0'
runStoreSocketWithVersion
  :: ( MonadIO m
     , MonadMask m
     )
  => ProtoVersion
  -> Family
  -> SockAddr
  -> RemoteStoreT m a
  -> Run m a
runStoreSocketWithVersion offeredVersion sockFamily sockAddr code =
  Control.Monad.Catch.bracket
    (liftIO open)
    (liftIO . Network.Socket.close . hasStoreSocket)
    (\s -> runRemoteStoreT s $ setProtoVersion offeredVersion >> greetServer >> code)
  where
    open = do
      soc <-
        Network.Socket.socket
          sockFamily
          Network.Socket.Stream
          Network.Socket.defaultProtocol
      Network.Socket.connect soc sockAddr
      pure soc

justdoit :: Run IO (Bool, Bool)
justdoit = do
  runDaemonConnection runStore (pure ()) (StoreConnection_Socket "/tmp/dsock") $
    runStoreConnection (StoreConnection_Socket "/tmp/dsock")
      $ do
        a <- isValidPath pth
        b <- isValidPath pth
        pure (a, b)
  where
    pth :: StorePath
    pth =
      either (error . show) id
      $ System.Nix.StorePath.parsePathFromText
        def
        "/nix/store/yyznqbwam67cmp7zfwk0rkgmi9yqsdsm-hnix-store-core-0.8.0.0"

runDaemon
  :: forall m a
  . ( MonadIO m
    , MonadConc m
    )
  => WorkerHelper m
  -> m a
  -> m a
runDaemon workerHelper =
  runDaemonConnection
    workerHelper
    (pure ())
    def

-- | Run an emulated nix daemon using given @StoreConnection@
-- the deamon will close when the continuation returns.
runDaemonConnection
  :: forall m a
  . ( MonadIO m
    , MonadConc m
    )
  => WorkerHelper m
  -> RemoteStoreT m ()
  -> StoreConnection
  -> m a
  -> m a
runDaemonConnection workerHelper postGreet sc k =
  connectionToSocket sc
  >>= \case
    Left e -> error $ show e
    Right (fam, sock) -> runDaemonSocket workerHelper postGreet fam sock k

-- | Run an emulated nix daemon using given @StoreConnection@
-- the deamon will close when the continuation returns.
runDaemonSocket
  :: forall m a
  . ( MonadIO m
    , MonadConc m
    )
  => WorkerHelper m
  -> RemoteStoreT m ()
  -> Family
  -> SockAddr
  -> m a
  -> m a
runDaemonSocket workerHelper postGreet sockFamily sockAddr k =
  Control.Monad.Catch.bracket
    (liftIO
      $ Network.Socket.socket
          sockFamily
          Network.Socket.Stream
          Network.Socket.defaultProtocol
    )
    (\lsock -> liftIO $ Network.Socket.close lsock)
    $ \lsock -> do
    -- TODO: the: (\lsock -> liftIO $ Network.Socket.close lsock *> System.Directory.removeFile f)
    -- branch should really be (and even removeFile is currently omitted)
    -- a file lock followed by unlink *before* bind rather than after close.  If
    -- the program crashes (or loses power or something) then a stale unix
    -- socket will stick around and prevent the daemon from starting.  using a
    -- lock file instead means only one "copy" of the daemon can hold the lock,
    -- and can safely unlink the socket before binding no matter how shutdown
    -- occured.

    -- set up the listening socket
    liftIO $ Network.Socket.bind lsock sockAddr
    runProxyDaemon workerHelper postGreet lsock k

connectionToSocket
  :: MonadIO m
  => StoreConnection
  -> m (Either RemoteStoreError (Family, SockAddr))
connectionToSocket (StoreConnection_Socket (StoreSocketPath f)) =
  pure $ pure
    ( Network.Socket.AF_UNIX
    , SockAddrUnix f
    )
connectionToSocket (StoreConnection_TCP StoreTCP{..}) = do
  addrInfo <- liftIO $ Network.Socket.getAddrInfo
    (Just Network.Socket.defaultHints)
    (Just storeTCPHost)
    (Just $ show storeTCPPort)
  case addrInfo of
      (sockAddr:_) ->
        pure $ pure
          ( Network.Socket.addrFamily sockAddr
          , Network.Socket.addrAddress sockAddr
          )
      _ -> pure (Left RemoteStoreError_GetAddrInfoFailed)

{-# LANGUAGE OverloadedStrings #-}

module StoreConnectionSpec where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.Either qualified
import System.Nix.Store.Remote (parseStoreConnection)
import System.Nix.Store.Remote.Types (StoreConnection(..), StoreSocketPath(..))

spec :: Spec
spec =
  describe "parseStoreConnection" $ do
    let defaultSocket = StoreSocketPath "/nix/var/nix/daemon-socket/socket"
        socketConnection = Right . StoreConnection_Socket . StoreSocketPath

    it "names the default socket for daemon, auto, unix:// and the empty URL" $
      fmap (parseStoreConnection defaultSocket) ["daemon", "auto", "unix://", ""]
        `shouldBe` replicate 4 (Right (StoreConnection_Socket defaultSocket))

    it "names the socket of a unix:// URL" $
      parseStoreConnection defaultSocket "unix:///tmp/test-store/socket"
        `shouldBe` socketConnection "/tmp/test-store/socket"

    it "leaves out store settings" $
      parseStoreConnection defaultSocket "unix:///tmp/test-store/socket?root=/tmp/test-store"
        `shouldBe` socketConnection "/tmp/test-store/socket"

    it "rejects stores that aren't daemons" $
      parseStoreConnection defaultSocket "ssh-ng://builder"
        `shouldSatisfy` Data.Either.isLeft

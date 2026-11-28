-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.StringContext where

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

import System.Nix.Arbitrary.DerivedPath ()
import System.Nix.Arbitrary.StorePath ()
import System.Nix.StringContext (StringContextElem)

deriving via GenericArbitrary StringContextElem
  instance Arbitrary StringContextElem

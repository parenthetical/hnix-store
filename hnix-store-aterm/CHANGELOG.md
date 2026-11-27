# [next](https://github.com/haskell-nix/hnix-store/compare/aterm-0.1.0.0...master) 2026-MM-DD

* Additions:
   * `parseDerivation`, `buildDerivation` and their `With` and `Inputs` variants, for derivations whose inputs are `DerivationInputs`.
     Inputs that are outputs of dynamic derivations need the `DrvWithVersion("xp-dyn-drv",...)` version of the format, which the new `DerivationATermVersion` type names.
     `buildDerivation` writes the oldest version that can hold the inputs, as Nix does, and `parseDerivation` reads both versions.
   * Tests against upstream Nix's ATerm test data

* Changes:
   * A fixed output's hash is written in base 16, as Nix writes it, and read in base 16, Nix's base 32 or base 64.
     It was written and read in Nix's base 32 only, so a fixed-output derivation that Nix wrote didn't parse.

# Version 0.1.0.0 (2026-10-21)
* Forked from [nix-derivation-1.1.3](https://hackage.haskell.org/package/nix-derivation)
* Merged with derivation types of `hnix-store-core`

---

`hnix-store-aterm` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

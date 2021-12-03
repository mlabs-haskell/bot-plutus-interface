{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }, deferPluginErrors ? true
, doCoverage ? false }:
let inherit (plutus) pkgs;
in pkgs.haskell-nix.cabalProject rec {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "mlabs-pab";
    src = ./..;
  };

  # Plutus uses a patched GHC. And so shall we.
  compiler-nix-name = "ghc810420210212";

  modules = [{
    packages = {
      cardano-crypto-praos.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

      cardano-crypto-class.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

      marlowe.doHaddock = deferPluginErrors;
      marlowe.flags.defer-plugin-errors = deferPluginErrors;

      plutus-use-cases.doHaddock = deferPluginErrors;
      plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

      plutus-ledger.doHaddock = deferPluginErrors;
      plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

      # This allows us to generate .tix coverage files, which could be useful?
      "${src.name}".components.library.doCoverage = doCoverage;
    };
  }];

  # Using this allows us to leave these nix-specific hashes _out_ of cabal.project
  # Normally, they'd be placed under the `source-repository-package` section as a comment like so:
  # `--sha256: ...`
  sha256map = {
    # Enforce we are using the same hash as niv has
    # i.e. this will now fail to nix-build if you bump it but don't bump the `cabal.project`.
    "https://github.com/input-output-hk/plutus"."${sources.plutus.rev}" =
      sources.plutus.sha256;
    "https://github.com/input-output-hk/plutus-apps.git"."${sources.plutus-apps.rev}" =
      sources.plutus-apps.sha256;
    "https://github.com/Quid2/flat.git"."${sources.flat.rev}" =
      sources.flat.sha256;
    "https://github.com/input-output-hk/purescript-bridge.git"."${sources.purescript-bridge.rev}" =
      sources.purescript-bridge.sha256;
    "https://github.com/input-output-hk/servant-purescript.git"."${sources.servant-purescript.rev}" =
      sources.servant-purescript.sha256;
    "https://github.com/input-output-hk/cardano-base"."${sources.cardano-base.rev}" =
      sources.cardano-base.sha256;
    "https://github.com/input-output-hk/cardano-crypto.git"."${sources.cardano-crypto.rev}" =
      sources.cardano-crypto.sha256;
    "https://github.com/input-output-hk/cardano-ledger-specs"."${sources.cardano-ledger-specs.rev}" =
      sources.cardano-ledger-specs.sha256;
    "https://github.com/input-output-hk/cardano-prelude"."${sources.cardano-prelude.rev}" =
      sources.cardano-prelude.sha256;
    "https://github.com/input-output-hk/goblins"."${sources.goblins.rev}" =
      sources.goblins.sha256;
    "https://github.com/input-output-hk/iohk-monitoring-framework"."${sources.iohk-monitoring-framework.rev}" =
      sources.iohk-monitoring-framework.sha256;
    "https://github.com/input-output-hk/ouroboros-network"."${sources.ouroboros-network.rev}" =
      sources.ouroboros-network.sha256;
    "https://github.com/input-output-hk/cardano-node.git"."${sources.cardano-node.rev}" =
      sources.cardano-node.sha256;
    "https://github.com/input-output-hk/Win32-network"."${sources.Win32-network.rev}" =
      sources.Win32-network.sha256;
    "https://github.com/input-output-hk/hedgehog-extras"."${sources.hedgehog-extras.rev}" =
      sources.hedgehog-extras.sha256;
    "https://github.com/input-output-hk/optparse-applicative"."${sources.optparse-applicative.rev}" =
      sources.optparse-applicative.sha256;
    "https://github.com/input-output-hk/cardano-addresses"."${sources.cardano-addresses.rev}" =
      sources.cardano-addresses.sha256;
    "https://github.com/j-mueller/cardano-wallet"."${sources.cardano-wallet.rev}" =
      sources.cardano-wallet.sha256;
  };
}

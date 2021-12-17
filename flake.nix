{
  description = "mlabs-pab";

  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
  inputs.plutus.url = "github:input-output-hk/plutus"; # used for libsodium-vrf

  outputs = { self, nixpkgs, haskell-nix, plutus }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;

          fakeSrc = pkgs.runCommand "real-source" { } ''
            cp -rT ${self} $out
            chmod u+w $out/cabal.project
            cat $out/cabal-haskell.nix.project >> $out/cabal.project
          '';

          sources = import ./nix/sources.nix { };
        in (nixpkgsFor system).haskell-nix.cabalProject' {
          src = fakeSrc.outPath;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          modules = [{
            packages = {
              marlowe.flags.defer-plugin-errors = deferPluginErrors;
              plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
              plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
              plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
              cardano-crypto-praos.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-wallet-core.components.library.build-tools =
                [ pkgs.buildPackages.buildPackages.gitMinimal ];
              cardano-config.components.library.build-tools =
                [ pkgs.buildPackages.buildPackages.gitMinimal ];
            };
          }];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs =
              [ pkgs.cabal-install pkgs.hlint pkgs.haskellPackages.fourmolu ];

            additional = ps: [
              ps.base-deriving-via
              ps.cardano-addresses
              ps.cardano-addresses-cli
              ps.cardano-binary
              ps.cardano-crypto
              ps.cardano-crypto-class
              ps.cardano-crypto-praos
              ps.cardano-crypto-wrapper
              ps.cardano-ledger-alonzo
              ps.cardano-ledger-byron
              ps.cardano-ledger-core
              ps.cardano-ledger-pretty
              ps.cardano-ledger-shelley
              ps.cardano-ledger-shelley-ma
              ps.cardano-prelude
              ps.cardano-slotting
              ps.flat
              ps.freer-extras
              ps.goblins
              ps.measures
              ps.orphans-deriving-via
              ps.playground-common
              ps.plutus-contract
              ps.plutus-core
              ps.plutus-ledger
              ps.plutus-ledger-api
              ps.plutus-pab
              ps.plutus-playground-server
              ps.plutus-tx
              ps.plutus-tx-plugin
              ps.plutus-use-cases
              ps.prettyprinter-configurable
              ps.quickcheck-dynamic
              ps.Win32-network
              ps.word-array
            ];
          };
          sha256map = {
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
        };
    in {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out");
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}

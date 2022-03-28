{
  description = "bot-plutus-interface";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/71006f9eb956b0004022e80aadd4ad50d837b621";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/41545ba3ac6b3095966316a99883d678b5ab8da8";
      flake = false;
    };
    cardano-config = {
      url =
        "github:input-output-hk/cardano-config/e9de7a2cf70796f6ff26eac9f9540184ded0e4e6";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/814df2c146f5d56f8c35a681fe75e85b905aed5d";
      # flake = false; -- we need it to be available in shell
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    cardano-wallet = {
      url =
        "github:input-output-hk/cardano-wallet/a5085acbd2670c24251cf8d76a4e83c77a2679ba";
      flake = false;
    };
    # We don't actually need this. Removing this might make caching worse?
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/46f994e216a1f8b36fe4669b47b2a7011b0e153c";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/d2d219a86cda42787325bb8c20539a75c2667132";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/cc72a56eafb02333c96f662581b57504f8f8992f";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:input-output-hk/plutus-apps/7f543e21d4945a2024e46c572303b9c1684a5832";
      flake = false;
    };
    purescript-bridge = {
      url =
        "github:input-output-hk/purescript-bridge/47a1f11825a0f9445e0f98792f79172efef66c00";
      flake = false;
    };
    servant-purescript = {
      url =
        "github:input-output-hk/servant-purescript/44e7cacf109f84984cd99cd3faf185d161826963";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      cabalProjectLocal = ''
        allow-newer: size-based:template-haskell
      '';

      haskellModules = [
        (
          { pkgs, ... }:
          {
            packages = {
              marlowe.flags.defer-plugin-errors = true;
              plutus-use-cases.flags.defer-plugin-errors = true;
              plutus-ledger.flags.defer-plugin-errors = true;
              plutus-contract.flags.defer-plugin-errors = true;
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-wallet-core.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
              cardano-config.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
            };
          }
        )
      ];

      extraSources = [
        {
          name = "cardano-addresses";
          src = inputs.cardano-addresses;
          subdirs = [ "core" "command-line" ];
        }
        {
          name = "cardano-base";
          src = inputs.cardano-base;
          subdirs = [
            "base-deriving-via"
            "binary"
            "binary/test"
            "cardano-crypto-class"
            "cardano-crypto-praos"
            "cardano-crypto-tests"
            "measures"
            "orphans-deriving-via"
            "slotting"
            "strict-containers"
          ];
        }
        {
          name = "cardano-crypto";
          src = inputs.cardano-crypto;
          subdirs = [ "." ];
        }
        {
          name = "cardano-ledger";
          src = inputs.cardano-ledger;
          subdirs = [
            "eras/alonzo/impl"
            "eras/byron/chain/executable-spec"
            "eras/byron/crypto"
            "eras/byron/crypto/test"
            "eras/byron/ledger/executable-spec"
            "eras/byron/ledger/impl"
            "eras/byron/ledger/impl/test"
            "eras/shelley/impl"
            "eras/shelley-ma/impl"
            "eras/shelley/test-suite"
            "libs/cardano-data"
            "libs/cardano-ledger-core"
            "libs/cardano-ledger-pretty"
            "libs/cardano-protocol-tpraos"
            "libs/compact-map"
            "libs/non-integral"
            "libs/set-algebra"
            "libs/small-steps"
            "libs/small-steps-test"
          ];
        }
        {
          name = "cardano-node";
          src = inputs.cardano-node;
          subdirs = [ "cardano-api" "cardano-node" "cardano-cli" ];
        }
        {
          name = "cardano-config";
          src = inputs.cardano-config;
          subdirs = [ "." ];
        }
        {
          name = "cardano-prelude";
          src = inputs.cardano-prelude;
          subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
        }
        {
          name = "cardano-wallet";
          src = inputs.cardano-wallet;
          subdirs = [
            "lib/cli"
            "lib/core"
            "lib/core-integration"
            "lib/dbvar"
            "lib/launcher"
            "lib/numeric"
            "lib/shelley"
            "lib/strict-non-empty-containers"
            "lib/test-utils"
            "lib/text-class"
          ];
        }
        {
          name = "flat";
          src = inputs.flat;
          subdirs = [ "." ];
        }
        {
          name = "goblins";
          src = inputs.goblins;
          subdirs = [ "." ];
        }
        {
          name = "iohk-monitoring-framework";
          src = inputs.iohk-monitoring-framework;
          subdirs = [
            "iohk-monitoring"
            "tracer-transformers"
            "contra-tracer"
            "plugins/backend-aggregation"
            "plugins/backend-ekg"
            "plugins/backend-monitoring"
            "plugins/backend-trace-forwarder"
            "plugins/scribe-systemd"
          ];
        }
        {
          name = "optparse-applicative";
          src = inputs.optparse-applicative;
          subdirs = [ "." ];
        }
        {
          name = "ouroboros-network";
          src = inputs.ouroboros-network;
          subdirs = [
            "io-classes"
            "io-sim"
            "monoidal-synchronisation"
            "network-mux"
            "ntp-client"
            "ouroboros-consensus"
            "ouroboros-consensus-byron"
            "ouroboros-consensus-cardano"
            "ouroboros-consensus-protocol"
            "ouroboros-consensus-shelley"
            "ouroboros-network"
            "ouroboros-network-framework"
            "ouroboros-network-testing"
            "strict-stm"
            "typed-protocols"
            "typed-protocols-cborg"
            "typed-protocols-examples"
          ];
        }
        {
          name = "plutus";
          src = inputs.plutus;
          subdirs = [
            "plutus-core"
            "plutus-ledger-api"
            "plutus-tx"
            "plutus-tx-plugin"
            "prettyprinter-configurable"
            "stubs/plutus-ghc-stub"
            "word-array"
          ];
        }
        {
          name = "plutus-apps";
          src = inputs.plutus-apps;
          subdirs = [
            "doc"
            "freer-extras"
            "playground-common"
            "plutus-chain-index"
            "plutus-chain-index-core"
            "plutus-contract"
            "plutus-ledger"
            "plutus-ledger-constraints"
            "plutus-pab"
            "plutus-playground-server"
            "plutus-use-cases"
            "quickcheck-dynamic"
            "web-ghc"
          ];
        }
        {
          name = "purescript-bridge";
          src = inputs.purescript-bridge;
          subdirs = [ "." ];
        }
        {
          name = "servant-purescript";
          src = inputs.servant-purescript;
          subdirs = [ "." ];
        }
        {
          name = "Win32-network";
          src = inputs.Win32-network;
          subdirs = [ "." ];
        }
      ];

      compiler-nix-name = "ghc8107";

      index-state = "2022-01-22T00:00:00Z";

      extraPackageDirs = builtins.concatLists (
        builtins.map
          (dep: builtins.map
            (d: "${dep.src}/${if d == "." then "" else d}")
            dep.subdirs)
          extraSources);

      extraSourceNixFor = system:
        ((nixpkgsFor system).haskell-nix.callCabalProjectToNix {
          inherit compiler-nix-name index-state cabalProjectLocal;
          src = (nixpkgsFor' system).runCommand "empty" { } "mkdir $out; touch $out/empty.cabal";
          name = "extra-source-nix";
          configureArgs = "--disable-tests";
          cabalProject = ''
            packages: ${builtins.concatStringsSep " " extraPackageDirs}
          '';
        }).projectNix;

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          extraSourceNix = import (extraSourceNixFor system);
          pkgDefExtras = hackage: (extraSourceNix.extras hackage).packages;
          pkgNames = builtins.attrNames (pkgDefExtras null);
        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          inherit compiler-nix-name index-state;
          name = "bot-plutus-interface";

          pkg-def-extras = [ pkgDefExtras ];
          modules = haskellModules ++ extraSourceNix.modules;

          cabalProjectLocal = ''
            ${cabalProjectLocal}
            packages: ${builtins.concatStringsSep " " extraPackageDirs}
            ${builtins.concatStringsSep "\n" (builtins.map (x: ''
            package ${x}
              tests: false
            '') pkgNames)}
          '';

          shell = {
            additional = ps: [
              ps.plutus-pab
            ];
            withHoogle = true;
            tools.haskell-language-server = { };
            exactDeps = true;
            nativeBuildInputs = [ pkgs'.cabal-install pkgs'.hlint pkgs'.haskellPackages.fourmolu pkgs'.jq pkgs'.websocat ];
          };
        };

    in
    {
      inherit cabalProjectLocal extraSources haskellModules;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "bot-plutus-interface:lib:bot-plutus-interface";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages // {
        extraSourceNix = extraSourceNixFor system;
      });

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [ self.devShell.${system}.inputDerivation ];
          } "touch $out");

      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      checks = perSystem (system: self.flake.${system}.checks);
    };
}

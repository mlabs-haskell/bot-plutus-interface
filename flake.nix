{
  description = "bot-plutus-interface";

  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    blockfrost-haskell = {
      url = "github:blockfrost/blockfrost-haskell/edfd43a95a21356b0cc540002bd1583a35883f85";
      flake = false;
    };
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/56746f558d57a6b1d66d20eaac8a83dc4455cb4f";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/394c4637c24d82325bd04ceb99c8e8df5617e663";
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
        "github:input-output-hk/cardano-ledger/1db68a3ec0a2dcb5751004beb22b906162474f23";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/65422ff373f2f88a563afa746a9a16d211ffdc7c";
      flake = false; # we need it to be available in shell
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    cardano-wallet = {
      url = "github:ak3n/cw/538375cb3efa0c66604ff0b13b6ecc0c4a694d64";
      flake = false;
    };
    ekg-json = {
      url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
      flake = false;
    };
    ekg-forward = {
      url = "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };
    # We don't actually need this. Removing this might make caching worse?
    flat = {
      url =
        "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    hedgehog-extras = {
      url = "github:input-output-hk/hedgehog-extras/967d79533c21e33387d0227a5f6cc185203fe658";
      flake = false;
    };
    hw-aeson = {
      url = "github:haskell-works/hw-aeson/d99d2f3e39a287607418ae605b132a3deb2b753f";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/eb7854d1337637b8672af1227b276aa33a658f47";
      flake = false;
    };
    io-sim = {
      url =
        "github:input-output-hk/io-sim/606de33fa2f467d108fb1efb86daeb3348bf34e3";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/ad47441e9e399495579d7dd05ccaf96b46a487b2";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/5cc518f1202930ad52b8ba838af32af084c0e754";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:input-output-hk/plutus-apps/5fb99da7bb5a3e5d6ca02527802856c51835f6e0";
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
    typed-protocols = {
      url =
        "github:input-output-hk/typed-protocols/91c3fba44d68439df207796171cd6f867354b76b";
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
        ({ pkgs, ... }:
          {
            packages = {
              marlowe.flags.defer-plugin-errors = true;
              plutus-use-cases.flags.defer-plugin-errors = true;
              plutus-ledger.flags.defer-plugin-errors = true;
              plutus-script-utils.flags.defer-plugin-errors = true;
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
        # {
        #   src = inputs.blockfrost-haskell;
        #   subdirs = [
        #     "blockfrost-api"
        #     "blockfrost-client-core"
        #     "blockfrost-client"
        #     "blockfrost-pretty"
        #   ];
        # }
        {
          src = inputs.cardano-addresses;
          subdirs = [ "core" "command-line" ];
        }
        {
          src = inputs.cardano-base;
          subdirs = [
            "base-deriving-via"
            "binary"
            "binary/test"
            "cardano-crypto-class"
            "cardano-crypto-praos"
            "measures"
            "orphans-deriving-via"
            "slotting"
            "strict-containers"
          ];
        }
        {
          src = inputs.cardano-crypto;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-ledger;
          subdirs = [
            "eras/alonzo/impl"
            "eras/babbage/impl"
            "eras/byron/chain/executable-spec"
            "eras/byron/crypto"
            "eras/byron/crypto/test"
            "eras/byron/ledger/executable-spec"
            "eras/byron/ledger/impl"
            "eras/byron/ledger/impl/test"
            "eras/shelley/impl"
            "eras/shelley/test-suite"
            "eras/shelley-ma/impl"
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
          src = inputs.cardano-node;
          subdirs = [
            "cardano-api"
            "cardano-cli"
            "cardano-git-rev"
            "cardano-node"
            "cardano-submit-api"
            "cardano-testnet"
            "trace-dispatcher"
            "trace-forward"
            "trace-resources"
          ];
        }
        {
          src = inputs.cardano-config;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-prelude;
          subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
        }
        {
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
          src = inputs.ekg-json;
          subdirs = [ "." ];
        }
        {
          src = inputs.ekg-forward;
          subdirs = [ "." ];
        }
        {
          src = inputs.flat;
          subdirs = [ "." ];
        }
        {
          src = inputs.goblins;
          subdirs = [ "." ];
        }
        {
          src = inputs.hedgehog-extras;
          subdirs = [ "." ];
        }
        {
          src = inputs.hw-aeson;
          subdirs = [ "." ];
        }
        {
          src = inputs.iohk-monitoring-framework;
          subdirs = [
            "contra-tracer"
            "iohk-monitoring"
            "tracer-transformers"
            "plugins/backend-ekg"
            "plugins/backend-aggregation"
            "plugins/backend-monitoring"
            "plugins/backend-trace-forwarder"
          ];
        }
        # {
        #   src = inputs.io-sim;
        #   subdirs = [
        #     "io-classes"
        #     "io-sim"
        #     "strict-stm"
        #   ];
        # }
        {
          src = inputs.optparse-applicative;
          subdirs = [ "." ];
        }
        {
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
          src = inputs.plutus-apps;
          subdirs = [
            "doc"
            "freer-extras"
            "playground-common"
            "plutus-chain-index"
            "plutus-chain-index-core"
            "plutus-contract"
            "plutus-contract-certification"
            "plutus-ledger"
            "plutus-ledger-constraints"
            "plutus-pab"
            "plutus-playground-server"
            "plutus-script-utils"
            "plutus-use-cases"
            "quickcheck-dynamic"
            "web-ghc"
          ];
        }
        {
          src = inputs.purescript-bridge;
          subdirs = [ "." ];
        }
        {
          src = inputs.servant-purescript;
          subdirs = [ "." ];
        }
        # {
        #   src = inputs.typed-protocols;
        #   subdirs = [
        #     "typed-protocols"
        #     "typed-protocols-cborg"
        #     "typed-protocols-examples"
        #   ];
        # }
        {
          src = inputs.Win32-network;
          subdirs = [ "." ];
        }
      ];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          inherit cabalProjectLocal extraSources;
          name = "bot-plutus-interface";
          compiler-nix-name = "ghc8107";
          shell = {
            additional = ps: [
              ps.plutus-pab
            ];
            withHoogle = false;
            tools.haskell-language-server = { };
            exactDeps = true;
            nativeBuildInputs = with pkgs'; [
              cabal-install
              haskellPackages.cabal-fmt
              haskellPackages.implicit-hie
              haskellPackages.fourmolu
              hlint
              jq
              websocat
              fd
              nixpkgs-fmt
            ];
          };
          modules = haskellModules;
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          { nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ]; } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          mkdir $out
        '';

    in
    {
      inherit cabalProjectLocal extraSources haskellModules;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "bot-plutus-interface:lib:bot-plutus-interface";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

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
      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
      });

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}

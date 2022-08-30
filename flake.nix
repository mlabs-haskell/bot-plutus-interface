{
  description = "bot-plutus-interface";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix-extra-hackage.url = "github:mlabs-haskell/haskell-nix-extra-hackage/separate-hackages";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/8bf98905b903455196495e231b23613ad2264cb0";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/41545ba3ac6b3095966316a99883d678b5ab8da8";
      flake = false;
    };
    cardano-config = {
      url =
        "github:input-output-hk/cardano-config/1646e9167fab36c0bff82317743b96efa2d3adaa";
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
        "github:input-output-hk/cardano-node/73f9a746362695dc2cb63ba757fbcabb81733d23";
      flake = false;
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    cardano-wallet = {
      url =
        "github:input-output-hk/cardano-wallet/769d3f8e5543f784222c6b5d0ba3ea6c3ccdd7b0";
      flake = false;
    };
    # We don't actually need this. Removing this might make caching worse?
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    ekg-forward = {
      url =
        "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/808724ff8a19a33d0ed06f9ef59fbd900b08553c";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/4fac197b6f0d2ff60dc3486c593b68dc00969fbf";
      flake = false;
    };
    # Patched plutus for metadata support. We need this until `plutus-apps` will update `plutus`,
    # rewrite of `plutus-ledger-constraints`, and possibly some bpi adjustments afterwards.
    # tldr: Dependency hell
    plutus = {
      url =
        "github:mlabs-haskell/plutus/1a3c3a761cf048371c52a34b004f8b3fcf0dab43";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:mlabs-haskell/plutus-apps/82c0725c4d05398ae76d71927cc60aa23db1a11d";
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

    wl-pprint-text = {
      url = "github:ivan-m/wl-pprint-text";
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

        constraints: hedgehog >= 1.0.4, hedgehog < 1.1
      '';

      haskellModules = [
        ({ pkgs, ... }:
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

      extraHackagePackages = [
        "${inputs.Win32-network}"

        "${inputs.cardano-addresses}/command-line"
        "${inputs.cardano-addresses}/core"

        "${inputs.cardano-base}/base-deriving-via"
        "${inputs.cardano-base}/binary"
        "${inputs.cardano-base}/binary/test"
        "${inputs.cardano-base}/cardano-crypto-class"
        "${inputs.cardano-base}/cardano-crypto-praos"
        "${inputs.cardano-base}/cardano-crypto-tests"
        "${inputs.cardano-base}/measures"
        "${inputs.cardano-base}/orphans-deriving-via"
        "${inputs.cardano-base}/slotting"
        "${inputs.cardano-base}/strict-containers"
      
        "${inputs.cardano-config}"

        "${inputs.cardano-crypto}"

        "${inputs.cardano-ledger}/eras/alonzo/impl"
        "${inputs.cardano-ledger}/eras/byron/chain/executable-spec"
        "${inputs.cardano-ledger}/eras/byron/crypto"
        "${inputs.cardano-ledger}/eras/byron/crypto/test"
        "${inputs.cardano-ledger}/eras/byron/ledger/executable-spec"
        "${inputs.cardano-ledger}/eras/byron/ledger/impl"
        "${inputs.cardano-ledger}/eras/byron/ledger/impl/test"
        "${inputs.cardano-ledger}/eras/shelley-ma/impl"
        "${inputs.cardano-ledger}/eras/shelley/impl"
        "${inputs.cardano-ledger}/eras/shelley/test-suite"
        "${inputs.cardano-ledger}/libs/cardano-data"
        "${inputs.cardano-ledger}/libs/cardano-ledger-core"
        "${inputs.cardano-ledger}/libs/cardano-ledger-pretty"
        "${inputs.cardano-ledger}/libs/cardano-protocol-tpraos"
        "${inputs.cardano-ledger}/libs/compact-map"
        "${inputs.cardano-ledger}/libs/non-integral"
        "${inputs.cardano-ledger}/libs/set-algebra"
        "${inputs.cardano-ledger}/libs/small-steps"
        "${inputs.cardano-ledger}/libs/small-steps-test"

        "${inputs.cardano-node}/cardano-api"
        "${inputs.cardano-node}/cardano-cli"
        "${inputs.cardano-node}/cardano-git-rev"
        "${inputs.cardano-node}/cardano-node"
        "${inputs.cardano-node}/trace-dispatcher"
        "${inputs.cardano-node}/trace-forward"
        "${inputs.cardano-node}/trace-resources"

        "${inputs.cardano-prelude}/cardano-prelude"
        "${inputs.cardano-prelude}/cardano-prelude-test"

        "${inputs.cardano-wallet}/lib/cli"
        "${inputs.cardano-wallet}/lib/core"
        "${inputs.cardano-wallet}/lib/core-integration"
        "${inputs.cardano-wallet}/lib/dbvar"
        "${inputs.cardano-wallet}/lib/launcher"
        "${inputs.cardano-wallet}/lib/numeric"
        "${inputs.cardano-wallet}/lib/shelley"
        "${inputs.cardano-wallet}/lib/strict-non-empty-containers"
        "${inputs.cardano-wallet}/lib/test-utils"
        "${inputs.cardano-wallet}/lib/text-class"

        "${inputs.ekg-forward}"

        "${inputs.flat}"

        "${inputs.goblins}"

        "${inputs.iohk-monitoring-framework}/contra-tracer"
        "${inputs.iohk-monitoring-framework}/iohk-monitoring"
        "${inputs.iohk-monitoring-framework}/plugins/backend-aggregation"
        "${inputs.iohk-monitoring-framework}/plugins/backend-ekg"
        "${inputs.iohk-monitoring-framework}/plugins/backend-monitoring"
        "${inputs.iohk-monitoring-framework}/plugins/backend-trace-forwarder"
        "${inputs.iohk-monitoring-framework}/plugins/scribe-systemd"
        "${inputs.iohk-monitoring-framework}/tracer-transformers"

        "${inputs.optparse-applicative}"

        "${inputs.ouroboros-network}/io-classes"
        "${inputs.ouroboros-network}/io-sim"
        "${inputs.ouroboros-network}/monoidal-synchronisation"
        "${inputs.ouroboros-network}/network-mux"
        "${inputs.ouroboros-network}/ntp-client"
        "${inputs.ouroboros-network}/ouroboros-consensus"
        "${inputs.ouroboros-network}/ouroboros-consensus-byron"
        "${inputs.ouroboros-network}/ouroboros-consensus-cardano"
        "${inputs.ouroboros-network}/ouroboros-consensus-protocol"
        "${inputs.ouroboros-network}/ouroboros-consensus-shelley"
        "${inputs.ouroboros-network}/ouroboros-network"
        "${inputs.ouroboros-network}/ouroboros-network-framework"
        "${inputs.ouroboros-network}/ouroboros-network-testing"
        "${inputs.ouroboros-network}/strict-stm"
        "${inputs.ouroboros-network}/typed-protocols"
        "${inputs.ouroboros-network}/typed-protocols-cborg"
        "${inputs.ouroboros-network}/typed-protocols-examples"

        "${inputs.plutus-apps}/doc"
        "${inputs.plutus-apps}/freer-extras"
        "${inputs.plutus-apps}/playground-common"
        "${inputs.plutus-apps}/plutus-chain-index"
        "${inputs.plutus-apps}/plutus-chain-index-core"
        "${inputs.plutus-apps}/plutus-contract"
        "${inputs.plutus-apps}/plutus-ledger"
        "${inputs.plutus-apps}/plutus-ledger-constraints"
        "${inputs.plutus-apps}/plutus-pab"
        "${inputs.plutus-apps}/plutus-playground-server"
        "${inputs.plutus-apps}/plutus-use-cases"
        "${inputs.plutus-apps}/quickcheck-dynamic"
        "${inputs.plutus-apps}/web-ghc"

        "${inputs.plutus}/plutus-core"
        "${inputs.plutus}/plutus-ledger-api"
        "${inputs.plutus}/plutus-tx"
        "${inputs.plutus}/plutus-tx-plugin"
        "${inputs.plutus}/prettyprinter-configurable"
        "${inputs.plutus}/stubs/plutus-ghc-stub"
        "${inputs.plutus}/word-array"

        "${inputs.purescript-bridge}"

        "${inputs.servant-purescript}"

        "${inputs.wl-pprint-text}"
      ];


      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          compiler-nix-name = "ghc8107";
          myHackages = inputs.haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name extraHackagePackages;
        in
        pkgs.haskell-nix.cabalProject' {
          name = "bot-plutus-interface";
          src = ./.;
          inherit compiler-nix-name cabalProjectLocal;
          inherit (myHackages) extra-hackages extra-hackage-tarballs;
          modules = myHackages.modules ++ haskellModules;

          index-state = "2022-01-22T00:00:00Z";

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
      inherit cabalProjectLocal extraHackagePackages haskellModules;

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
    };
}

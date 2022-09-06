{
  description = "bot-plutus-interface";

  inputs = {
    haskell-nix.url = "github:mangoiv/haskell.nix/extra-sources";

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    nixpkgs-upstream.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    haskell-nix-extra-hackage = {
      url = "github:mlabs-haskell/haskell-nix-extra-hackage";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.haskell-nix.follows = "haskell-nix";
    };

    plutus-contract = {
      url =
        "github:mangoiv/plutus-contract/mangoiv/next-node/rip-out-plutus-contract-api";
    };

    # all inputs below here are for pinning with haskell.nix
    cardano-addresses = {
      follows = "plutus-contract/cardano-addresses";
      flake = false;
    };
    cardano-base = {
      follows =
        "plutus-contract/cardano-base";
      flake = false;
    };

    cardano-ledger = {
      follows =
        "plutus-contract/cardano-ledger";
      flake = false;
    };

    cardano-crypto = {
      follows =
        "plutus-contract/cardano-crypto";
      flake = false;
    };

    cardano-node = {
      follows =
        "plutus-contract/cardano-node";
      flake = false;
    };

    cardano-prelude = {
      follows =
        "plutus-contract/cardano-prelude";
      flake = false;
    };

    cardano-wallet = {
      follows =
        "plutus-contract/cardano-wallet";
      flake = false;
    };

    # We don't actually need this. Removing this might make caching worse?
    flat = {
      follows =
        "plutus-contract/flat";
      flake = false;
    };

    ekg-forward = {
      follows =
        "plutus-contract/ekg-forward";
      flake = false;
    };

    goblins = {
      follows =
        "plutus-contract/goblins";
      flake = false;
    };

    iohk-monitoring-framework = {
      follows =
        "plutus-contract/iohk-monitoring-framework";
      flake = false;
    };

    optparse-applicative = {
      follows =
        "plutus-contract/optparse-applicative";
      flake = false;
    };

    ouroboros-network = {
      follows =
        "plutus-contract/ouroboros-network";
      flake = false;
    };

    # Patched plutus for metadata support. We need this until `plutus-contract` will update `plutus`,
    # rewrite of `plutus-ledger-constraints`, and possibly some bpi adjustments afterwards.
    # tldr: Dependency hell
    # NOTE: updated this/ reverse if doesn't work
    plutus = {
      follows = "plutus-contract/plutus";
      flake = false;
    };

    Win32-network = {
      follows =
        "plutus-contract/Win32-network";
      flake = false;
    };
    # ------------------------------

    cardano-config = {
      url =
        "github:input-output-hk/cardano-config/1646e9167fab36c0bff82317743b96efa2d3adaa";
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
  };

  outputs = { self, nixpkgs, nixpkgs-upstream, haskell-nix, haskell-nix-extra-hackage, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs-upstream { inherit system; };

      cabalProjectLocal = ''
        allow-newer: 
          , size-based:template-haskell
          , *:aeson
        constraints: 
          , hedgehog >= 1.0.4
          , aeson >= 2.0
          , beam-migrate >= 0.5.1.2
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
        {
          src = inputs.purescript-bridge;
          subdirs = [ "." ];
        }
        {
          src = inputs.servant-purescript;
          subdirs = [ "." ];
        }
      ];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          compiler-nix-name = "ghc8107";


          bpiHackages = haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name 
          [ 
            "${inputs.plutus-contract}/plutus-contract" 
            "${inputs.plutus-contract}/plutus-ledger" 
            "${inputs.plutus-contract}/plutus-ledger-constraints" 
            "${inputs.plutus-contract}/plutus-chain-index-core" 
            "${inputs.plutus-contract}/plutus-script-utils" 
          ];

          pcHackages = inputs.plutus-contract.hackagesFor system;
        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          inherit cabalProjectLocal extraSources compiler-nix-name;
          extra-hackages = bpiHackages.extra-hackages ++ pcHackages.extra-hackages;
          extra-hackage-tarballs = bpiHackages.extra-hackage-tarballs // pcHackages.extra-hackage-tarballs;

          name = "bot-plutus-interface";
          shell = {
            additional = ps: [
              ps.plutus-ledger-constraints
              ps.plutus-contract
              ps.plutus-tx
              ps.plutus-tx-plugin
            ];
            withHoogle = true;
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
          modules = haskellModules ++ bpiHackages.modules ++ pcHackages.modules;
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
    };
}

with import ./nix { };
(plutus.plutus.haskell.project.shellFor {

  # Select packages who's dependencies should be added to the shell env
  packages = ps:
    [
      # criterion 
      # tasty-quickcheck
    ];

  # Select packages which should be added to the shell env, with their dependencies
  # Should try and get the extra cardano dependencies in here...
  additional = ps:
    with ps; [
      plutus-tx
      plutus-tx-plugin
      plutus-ledger-api
      plutus-core
      prettyprinter-configurable
    ];

  withHoogle = true;

  # Extra haskell tools (arg passed on to mkDerivation)
  # Using the plutus.pkgs to use nixpkgs version from plutus (nixpkgs-unstable, mostly)
  propagatedBuildInputs = with plutus.pkgs; [
    # Haskell Tools
    stack
    cabal-install
    plutus.plutus.hlint
    haskellPackages.fourmolu
    entr
    git
    ghc
    nixfmt
    plutus.plutus.haskell-language-server

    # hls doesn't support preprocessors yet so this has to exist in PATH
    haskellPackages.record-dot-preprocessor
  ];

  buildInputs = (with plutus.pkgs;
    [ lzma zlib pkg-config libsodium-vrf R ]
    ++ (lib.optionals (!stdenv.isDarwin) [ systemd ]));

})

{ sourcesFile ? ./sources.json
, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, deferPluginErrors ? true
, doCoverage ? false
}:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus deferPluginErrors doCoverage;
  };
  npmlock2nix = plutus.pkgs.callPackage sources.npmlock2nix { };
  solc = import ./solc.nix {
    inherit (plutus.pkgs) lib gccStdenv fetchzip boost cmake coreutils fetchpatch ncurses python3 z3 cvc4 cln gmp;
  };
in
rec {
  # What should CI build?

  inherit (project) projectCoverageReport;
  inherit (project.liquidity-bridge.components) library;
  inherit (project.liquidity-bridge.components.exes) liquidity-bridge;
  inherit (project.liquidity-bridge.components.exes) liquidity-bridge-pab;
  inherit (project.liquidity-bridge.components.exes) liquidity-bridge-server;
  inherit (project.liquidity-bridge.components.tests) liquidity-bridge-test;

  inherit solc;

  # We cannot run truffle tests outside of the build, as they utilise file writes, however the build still expects an output
  # Therefore we initialize the output as an empty folder
  truffle-tests = npmlock2nix.build {
    src = ../liquidity-bridge-eth;
    installPhase = "truffle test && mkdir -p $out";
    buildCommands = [
      "truffle compile"
      "typechain --target=truffle-v5 'build/**/[!I]*.json'"
    ];
    buildInputs = [solc];
    HOME="./";
  };

  # This will run the tests within this build and produce the test logs as output
  check = plutus.pkgs.runCommand "run-tests" { } ''
    ${liquidity-bridge-test}/bin/liquidity-bridge-test 2> $out
  '';
}


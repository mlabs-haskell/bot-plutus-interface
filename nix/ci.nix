{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }, deferPluginErrors ? true
, doCoverage ? false }:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus deferPluginErrors doCoverage;
  };
  npmlock2nix = plutus.pkgs.callPackage sources.npmlock2nix { };
in rec {
  # What should CI build?

  inherit (project) projectCoverageReport;
  inherit (project.mlabs-pab.components) library;
  inherit (project.mlabs-pab.components.tests) mlabs-pab-test;

  # This will run the tests within this build and produce the test logs as output
  check = plutus.pkgs.runCommand "run-tests" { } ''
    ${mlabs-pab-test}/bin/mlabs-pab-test 2> $out
  '';
}

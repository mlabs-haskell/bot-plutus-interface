{ sourcesFile ? ./sources.json, system ? builtins.currentSystem }: rec {
  sources = import ./sources.nix { inherit sourcesFile system; };
  plutus = import sources.plutus {
    # See: https://github.com/input-output-hk/plutus/blob/893a887eac83409131b2038820a14962c6796776/ci.nix#L5
    #rev = "fake";
  };
  pkgs = plutus.pkgs // {
    npmlock2nix = pkgs.callPackage sources.npmlock2nix { };
  };
  pab = import ./pab.nix { inherit plutus; };
  nixpkgs = import sources.nixpkgs { };
}

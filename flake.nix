{
  description = "Working through maelstrom curriculum";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      maelstrom = pkgs.callPackage ./nix/maelstrom.nix {};
      maelstromHs = pkgs.callPackage ./maelstromHs.nix {};
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
        inherit maelstrom;
      };
      defaultPackage = maelstromHs;
      packages = flake-utils.lib.flattenTree {
        inherit maelstrom;
        inherit maelstromHs;
      };
    });
}

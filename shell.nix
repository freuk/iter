let
  nixpkgs = builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz";

in { pkgs ? import ./default.nix { pkgs = import nixpkgs { }; } }: pkgs.shell

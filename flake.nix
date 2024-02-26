{
  description = "iter";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        packages = (import ./default.nix) {
          pkgs = import nixpkgs {
            system = system;
            config.allowBroken = true;
          };
        };
      in {
        inherit packages;
        devShells.default = packages.shell;
      });
}

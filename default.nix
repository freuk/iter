{ pkgs }:

let

  inherit (builtins) filterSource baseNameOf elem;
  inherit (pkgs.lib) getBin;

  _iter = srcBasenames:
    let
      src = filterSource (path: _: elem (baseNameOf path) srcBasenames) ./.;
      iter = pkgs.haskellPackages.callCabal2nix "iter" src { };
    in pkgs.haskell.lib.overrideCabal iter (old: {
      buildDepends = [ pkgs.makeWrapper ];
      postInstall =
        "wrapProgram $out/bin/iter --prefix PATH : ${getBin pkgs.icdiff}/bin";
    });

in rec {
  inherit pkgs;

  iter = _iter [ "iter.cabal" "LICENSE" "app" "Main.hs" ];

  shell = pkgs.haskellPackages.shellFor {
    # only include "iter.cabal" for faster iteration via 'cached-nix-shell'
    packages = p: [ (_iter [ "iter.cabal" ]) ];
    buildInputs = [
      pkgs.icdiff # pkgs.haskellPackages.shellFor does not pick this up
      pkgs.ghcid
      pkgs.ormolu
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.apply-refact
      pkgs.cabal-install
      pkgs.nixfmt
      pkgs.ormolu
      pkgs.just
      pkgs.pylint
    ];
  };

}

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hie = (import sources.all-hies {}).versions.ghc865;
in

with pkgs;

let
  hpkgs = haskellPackages;
  btools = [
    hpkgs.cabal-install
    hpkgs.ghcid
    hpkgs.hoogle
    hpkgs.stylish-cabal
    hie
  ];
  modifier = drv: haskell.lib.addBuildTools drv btools;
in

hpkgs.developPackage { root = ./.; inherit modifier; }

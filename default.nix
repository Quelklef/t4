{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "t4";
  src = ./.;

  buildInputs = [ pkgs.elmPackages.elm ];

  buildPhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import ./elm2nix/elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ./elm2nix/registry.dat;
  };

  installPhase = ''
    mkdir -p $out
    elm make Main.elm --optimize --output $out/elm.js
    cp {index.html,css.css} $out
  '';
}

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc883
    cabal-install
    ghcid
  ];
}

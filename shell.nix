{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      cabal-install
      haskell.compiler.ghc943
    ];
  }
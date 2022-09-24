{ pkgs ? import <nixpkgs> {} }:
  let
    ocamlPackages = pkgs.ocamlPackages;
  in
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      dune_3
    ] ++ ( with ocamlPackages;
    [
      ocaml
      core
      core_extended
      batteries
      findlib
      utop
      merlin
      ocp-indent
    ]);
  }

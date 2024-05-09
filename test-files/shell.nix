{ pkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/0096264659df9fa33c46b4d428f07cda83103a4d.tar.gz")
  { } }:

pkgs.mkShell {
  packages = [
    pkgs.nodejs_20
    pkgs.deno

    (pkgs.writeScriptBin "what-the-node" ''
      ${pkgs.nodejs_20}/bin/node --version
    '')
  ];

  shellHook = ''
    echo "Node is version: $(what-the-node)"
  '';

}

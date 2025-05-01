{ pkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/fbc17ef99777656543eb6e296c833e60f1b02862.tar.gz")
  { } }:

pkgs.mkShell {
  packages = [

    pkgs.nodejs_20

    #pkgs.nodePackages.vega-cli
    #pkgs.nodePackages.vega-lite

    pkgs.deno
    pkgs.babashka

    (pkgs.writeScriptBin "what-the-node" ''
      ${pkgs.nodejs_20}/bin/node --version
    '')
  ];

  shellHook = ''
    echo "Node is version: $(what-the-node)"
  '';

}

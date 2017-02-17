{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, errors, exceptions, stdenv
      , transformers, unexceptionalio
      }:
      mkDerivation {
        pname = "error-cause";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base errors exceptions transformers unexceptionalio
        ];
        description = "Remember the causes of your errors";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

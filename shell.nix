{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, Cabal, hspec, hspec-core, lens, linear
      , lrucache, QuickCheck, sdl2, StateVar, stdenv, stm, text
      , transformers
      }:
      mkDerivation {
        pname = "sdl2-compositor";
        version = "1.2.0.7";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base lens linear lrucache QuickCheck sdl2 StateVar stm text
          transformers
        ];
        testHaskellDepends = [
          base Cabal hspec hspec-core lrucache QuickCheck stm
        ];
        description = "image compositing with sdl2 - declarative style";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

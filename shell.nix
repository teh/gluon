{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, gi-gtk, gi-webkit2, haskell-gi-base, haskell, gi-javascriptcore_4_0_6
  , protolude, stdenv, base, bytestring, containers, gi-atk, gi-cairo, gi-gdk,
    gi-gio, gi-glib, gi-gobject, gi-soup, text, transformers, callPackage, hsass
      }:
  let
    gi-webkit2-fixed = haskell.lib.overrideCabal gi-webkit2 (drv:
      {
        libraryHaskellDepends = [
          base bytestring containers gi-atk gi-cairo gi-gdk gi-gio gi-glib
          gi-gobject gi-gtk gi-javascriptcore_4_0_6 gi-soup haskell-gi-base text
          transformers
       ];
      });
    gi-webkit2webextension = callPackage ./gi-webkit2webextension.nix {
      gi-javascriptcore = gi-javascriptcore_4_0_6;
      webkitgtk = pkgs.webkitgtk;
      webkit2gtk = null;
    };
    in mkDerivation {
        pname = "hs-webkit-test";
        version = "0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          gi-gtk gi-webkit2-fixed haskell-gi-base protolude gi-webkit2webextension
          hsass
        ];
    license = stdenv.lib.licenses.unfree;
    libraryPkgconfigDepends = [ pkgs.webkitgtk ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

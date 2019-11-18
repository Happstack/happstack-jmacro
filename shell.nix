{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, bytestring, cereal
      , digest, happstack-server, jmacro, stdenv, text, utf8-string
      , wl-pprint-text, cabal-install
      }:
      mkDerivation {
        pname = "happstack-jmacro";
        version = "7.0.12.2";
        src = ./.;
        libraryHaskellDepends = [
          base base64-bytestring bytestring cereal digest happstack-server
          jmacro text utf8-string wl-pprint-text cabal-install
        ];
        homepage = "http://www.happstack.com/";
        description = "Support for using JMacro with Happstack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

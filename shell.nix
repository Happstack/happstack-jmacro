with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, base64-bytestring, bytestring, cereal
             , digest, happstack-server, jmacro, stdenv, text, utf8-string
             , wl-pprint-text
             }:
             mkDerivation {
               pname = "happstack-jmacro";
               version = "7.0.10";
               src = ./.;
               buildDepends = [
                 base base64-bytestring bytestring cereal digest happstack-server
                 jmacro text utf8-string wl-pprint-text
               ];
               homepage = "http://www.happstack.com/";
               description = "Support for using JMacro with Happstack";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env

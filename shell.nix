{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let 
  inherit (nixpkgs) pkgs;  # ghc-8.0.1’
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [

        # haskell-docs mtl conduit aeson stm HDBC HDBC-sqlite3 HDBC-postgresql happstack-server
         random mtl conduit aeson HDBC HDBC-sqlite3 HDBC-postgresql happstack-server
         
       
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "haskell-happstack";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}

# /nix/store/jhgl6fm6lbxmfjqs9ipa6vxbb0nbg8xm-ghc-7.10.2/bin/runhaskell
#

{ nixpkgs ? import <nixpkgs> {  } }:
let
  # You can get a newer ref by looking under "nixpkgs-unstable" in https://status.nixos.org/
  nixpkgsRev = "ea5234e7073d5f44728c499192544a84244bf35a";
  nixpkgsSha = "sha256:1iqfglh1fdgqxm7n4763k1cipna68sa0cb3azm2gdzhr374avcvk";
  compiler = pkgs.haskellPackages;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha;
  }) {} ;


in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs =  [
      pkgs.tree # show files in directory as tree
      
      pkgs.dbeaver
      pkgs.glibc
      # pkgs.docker
      # pkgs.docker-compose
      pkgs.websocat # Websocat is a command-line utility that can help you test WebSocket connections

      compiler.stack
      compiler.cabal-install
      compiler.ghcid
      compiler.haskell-language-server
      compiler.ghcide
      pkgs.postgresql # needs for postgresql-simple-migration

      pkgs.ormolu
      pkgs.hpack
      compiler.ghc
      pkgs.haskellPackages.record-dot-preprocessor
      pkgs.zlib
      pkgs.pcre # A library for Perl Compatible Regular Expressions

      pkgs.go
      pkgs.gopls # Official language server for the Go language
    ];


#     virtualisation.docker.enable = true;
#     users.users.kolay = {
#       isNormalUser = true;
#       home = "/home/kolay";
#       extraGroups = [ "wheel" "networkmanager" "vboxusers" "docker" "audio"]; # "wheel" Enable ‘sudo’ for the user.
#     };

     shellHook = ''
         echo "Entering my Nix shell environment..."
         cd DB
         term -e 'docker-compose up' &
         cd ..
         # term &
         # term &
         # dbeaver &
         # code . &
     '';

}


# To run HLS:
# stack new MyProgram
# rm Setup.hs 
# rm stack.yaml 
# rm MyProgram.cabal
# rm -rf .stack-work/
# hpack
# gen-hie > hie.yaml
# cabal build

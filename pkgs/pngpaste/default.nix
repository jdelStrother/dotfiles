{ config, lib, pkgs, ... }:

pkgs.stdenv.mkDerivation {
  # for pasting images into org mode
  pname = "pngpaste";
  version = "0.2.3";

  src = pkgs.fetchFromGitHub {
    owner = "jcsalterego";
    repo = "pngpaste";
    rev = "67c39829fedb97397b691617f10a68af75cf0867";
    sha256 = "089rqjk7khphs011hz3f355c7z6rjd4ydb4qfygmb4x54z2s7xms";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp pngpaste $out/bin/
  '';
}

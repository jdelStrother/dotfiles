{ config, lib, pkgs, ... }:

# Use 'trash' rather than slow Applescript to delete files in Emacs
pkgs.stdenv.mkDerivation {
  pname = "macos-trash";
  version = "0.9.2";

  src = pkgs.fetchFromGitHub {
    owner = "ali-rantakari";
    repo = "trash";
    rev = "d33f12fb91d2ccb553098e0f0aea57a2add77e09";
    sha256 = "1d3rc03vgz32faj7qi18iiggxvxlqrj9lsk5jkpa9r1mcs5d89my";
  };

  buildInputs = [
    #pkgs.darwin.apple_sdk.frameworks.Cocoa
    #pkgs.darwin.apple_sdk.frameworks.ScriptingBridge
    pkgs.perl
  ];

  patchPhase = if pkgs.stdenv.hostPlatform.isAarch64 then
    "substituteInPlace Makefile --replace '-arch i386 -arch x86_64' '-arch arm64'"
  else
    "substituteInPlace Makefile --replace '-arch i386' ''";

  buildPhase = "make && make docs";
  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share/man/man1
    cp trash    $out/bin
    cp trash.1 $out/share/man/man1
  '';

  meta = with lib; { platforms = [ "aarch64-darwin" "x86_64-darwin" ]; };
}

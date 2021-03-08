# Updating channel:
# sudo -H nix-channel --list
# sudo -H nix-channel --update
#
# Update user packages with:
# nix-env -irf ~/.config/profile.nix
#
with import <nixpkgs> { };
# https://nixos.wiki/wiki/User:Raboof#using_a_fork_of_a_packaged_project
let nix-vendor = import(/Users/jon/Developer/vendor/nixpkgs) {
  config.allowUnfree = true;
};
gccemacs = (import (pkgs.fetchFromGitHub {
  owner = "twlz0ne";
  repo = "nix-gccemacs-darwin";
  rev = "e5019ce975516cbef2202e7316356f1342c22806";
  sha256 = "0ak19pfwj604p2gzz5zbfi2v2fqymkaxg396xch74qjixgb83qf1";
})).emacsGccDarwin;
emacsPackages = emacsPackagesNgGen gccemacs;
emacsWithPackages = emacsPackages.emacsWithPackages;
in {
  emacs = emacsWithPackages (epkgs: [ epkgs.magit epkgs.vterm ]);

  inherit awslogs
    awscli2
    coreutils
    direnv
    fish
    fzf
    gist
    git
    gnupg
    jq
    nixfmt
    nodejs-14_x
    parallel
    ruby_2_7
    pssh
    ripgrep
    # common dependencies for gem installs (nokogiri)
    # You'll need `gem install nokogiri -- --use-system-libraries` and/or `bundle config build.nokogiri --use-system-libraries`
    zlib libiconv libxml2
    rdbtools
    go
    shellcheck
    tmux
    yarn;

  # for pasting images into org mode
  pngpaste = stdenv.mkDerivation rec {
    src = pkgs.fetchFromGitHub {
      owner = "jcsalterego";
      repo="pngpaste";
      rev="67c39829fedb97397b691617f10a68af75cf0867";
      sha256="089rqjk7khphs011hz3f355c7z6rjd4ydb4qfygmb4x54z2s7xms";
    };
    name = "pngpaste";
    buildInputs = [pkgs.darwin.apple_sdk.frameworks.Cocoa];
    installPhase = ''
      mkdir -p $out/bin
      cp pngpaste $out/bin/
    '';
  };

  # Use 'trash' rather than slow Applescript to delete files in Emacs
  trash = stdenv.mkDerivation rec {
    src = pkgs.fetchFromGitHub {
      owner = "ali-rantakari";
      repo = "trash";
      rev = "d33f12fb91d2ccb553098e0f0aea57a2add77e09";
      sha256 = "1d3rc03vgz32faj7qi18iiggxvxlqrj9lsk5jkpa9r1mcs5d89my";
    };
    name = "trash";
    buildInputs = [
      pkgs.darwin.apple_sdk.frameworks.Cocoa
      pkgs.darwin.apple_sdk.frameworks.ScriptingBridge
      pkgs.perl
    ];
    patchPhase = "substituteInPlace Makefile --replace '-arch i386' ''";
    buildPhase = "make && make docs";
    installPhase = ''
      mkdir -p $out/bin
      mkdir -p $out/share/man/man1
      cp trash    $out/bin
      cp trash.1 $out/share/man/man1
    '';
  };
}

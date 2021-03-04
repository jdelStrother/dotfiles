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
in {
  inherit awslogs
    awscli2
    coreutils
    direnv
    # emacsMacport
    fish
    fzf
    gist
    git
    gnupg
    nixfmt
    nodejs-14_x
    parallel
    # ruby_2_7
    pssh
    ripgrep
    # common dependencies for gem installs (nokogiri)
    # You'll need `gem install nokogiri -- --use-system-libraries` and/or `bundle config build.nokogiri --use-system-libraries`
    zlib libiconv libxml2
    rdbtools
    go
    shellcheck
    yarn;

  ruby_3_0 = nix-vendor.ruby_3_0.override {
    docSupport = false;
  };

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

  # for emacs/roam/forge/vterm
  # (Pretty sure you should be able to use emacsWithPackages for this, but couldn't make it work)
  inherit sqlite cmake libtool;
  gccemacs = (import (pkgs.fetchFromGitHub {
    owner = "twlz0ne";
    repo = "nix-gccemacs-darwin";
    rev = "e5019ce975516cbef2202e7316356f1342c22806";
    sha256 = "0ak19pfwj604p2gzz5zbfi2v2fqymkaxg396xch74qjixgb83qf1";
  })).emacsGccDarwin;
}

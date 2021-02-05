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
    coreutils
    emacsMacport
    fish
    gist
    git
    nixfmt
    nodejs-14_x
    parallel
    ruby_2_7
    # common dependencies for gem installs (nokogiri)
    # You'll need `gem install nokogiri -- --use-system-libraries` and/or `bundle config build.nokogiri --use-system-libraries`
    zlib libiconv libxml2
    go;

  awscli2 = nix-vendor.awscli2;

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
}

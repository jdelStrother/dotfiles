# Updating channel:
# sudo -H nix-channel --list
# sudo -H nix-channel --update
#
# Update user packages with:
# nix-env -irf ~/.config/profile.nix
#
let pkgs = import(<nixpkgs>) {
  config.allowUnfree = true;
  # Build for M1 if that's what we're on
  localSystem = builtins.currentSystem;
  overlays = [
    (self: super: { nix-direnv = super.nix-direnv.override { enableFlakes = true; }; } )
  ];
};
pkgs_intel = import(<nixpkgs>) {
  config.allowUnfree = true;
  localSystem = "x86_64-darwin";
};

gccemacs = (import (pkgs.fetchFromGitHub {
  owner = "twlz0ne";
  repo = "nix-gccemacs-darwin";
  rev = "088f97e2939f33d6983fb90649a9c51d572736ec";
  sha256 = "0mdbgl82r0dpgsz71i2npfy6kyd16637sk7glagwwdz7l0zxxmwn";
})).emacsGccDarwin;
emacsPackages = pkgs.emacsPackagesNgGen gccemacs;
# emacsPackages = pkgs_next.emacsPackagesNgGen pkgs_next.emacsMacport;

emacsWithPackages = emacsPackages.emacsWithPackages;
in {
  # I'm using https://emacsformacosx.com nightlies instead nowadays
  # emacs = emacsWithPackages (epkgs: [ epkgs.magit epkgs.vterm ]);

  inherit (pkgs_intel)
    niv;
  inherit (pkgs)
    awscli2
    nixUnstable
    awslogs
    aws-vault
    clang
    coreutils
    direnv
    nix-direnv
    git
    fish
    fzf
    gist
    gnugrep #macos grep is weird
    gnupg
    gnused # macos sed is weird
    graphviz # dot for emacs/roam
    jq
    nodejs-16_x
    parallel
    ruby_2_7
    pssh
    ripgrep
    # common dependencies for gem installs (nokogiri)
    # You'll need `gem install nokogiri -- --use-system-libraries` and/or `bundle config build.nokogiri --use-system-libraries`
    zlib libiconv libxml2
    rdbtools
    go
    tmux
    oathToolkit
    shellcheck
    nixfmt
    cmake;

  # for pasting images into org mode
  pngpaste = pkgs.stdenv.mkDerivation rec {
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
  trash = pkgs.stdenv.mkDerivation rec {
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

    patchPhase = if builtins.currentSystem == "aarch64-darwin" then
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
  };
}

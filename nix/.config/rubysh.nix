# Sample usage:
#
# $ nix-shell ~/.config/rubysh.nix --argstr ruby_version ruby_2_7
# $ nix-shell ~/.config/rubysh.nix --argstr ruby_version ruby_3_0
#

{ pkgs ? import <nixpkgs> {
    # localSystem = "x86_64-darwin";
    localSystem = builtins.currentSystem;
  },
  ruby_version ? "ruby_2_7"
 }:

with pkgs;
let
  ruby = builtins.getAttr ruby_version pkgs;
  gems = ruby.gems;
  gemHome = "$HOME/.gem/ruby/${ruby.version.libDir}-${stdenv.system}";

in mkShell {
  buildInputs = [
    ncurses
    sqlite
    ruby
    # For regular gems, just run 'gem install'.
    # These are included here because they have awkward build requirements.
    gems.libxml-ruby
    gems.pg
    gems.sassc
  ];

  shellHook = ''
    export GEM_HOME="${gemHome}";
    export GEM_PATH="${gemHome}";
    export PATH="${gemHome}/bin:$PATH";
    export BUNDLE_BUILD__NOKOGIRI="${builtins.concatStringsSep " " ruby.gems.nokogiri.buildFlags}"
    export BUNDLE_BUILD__SQLITE3="${builtins.concatStringsSep " " ruby.gems.sqlite3.buildFlags} --with-cflags=-fdeclspec"
    export BUNDLE_BUILD__MYSQL2="--with-mysql-dir=${mysql80}"
  '';
}

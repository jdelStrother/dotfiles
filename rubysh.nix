# Sample usage:
#
# $ nix-shell ~/.config/rubysh.nix --argstr ruby_version ruby_2_7
# $ nix-shell ~/.config/rubysh.nix --argstr ruby_version ruby_3_0
#

{ pkgs ? import <nixpkgs> {
    # localSystem = "x86_64-darwin";
    localSystem = builtins.currentSystem;
  },
  ruby_version ? "ruby_3_1",
  extras ? ""
 }:

with pkgs;
let
  ruby = builtins.getAttr ruby_version pkgs;
  gems = ruby.gems;
  # Use the nix name (eg '5rkmmj1wy7m3vyj7qn3gizzfivbrc1r5-ruby-2.7.6') to keep gem homes separate.
  # Otherwise native gems fail if you're using a different ruby derivation, even for the same ruby version.
  gemHome = "$HOME/.gem/ruby/${builtins.baseNameOf ruby}";

in mkShell {
  buildInputs = [
    ncurses
    sqlite
    ruby
    libyaml
    # For regular gems, just run 'gem install'.
    # These are included here because they have awkward build requirements.
    # gems.libxml-ruby
    gems.pg
    gems.sassc
  ] ++ map (p:  pkgs.${p}) (lib.filter (x: x != "") (lib.splitString "," extras));

  shellHook = ''
    export GEM_HOME="${gemHome}";
    export GEM_PATH="${gemHome}";
    export PATH="${gemHome}/bin:$PATH";
    export BUNDLE_BUILD__LIBXML___RUBY="${builtins.concatStringsSep " " ruby.gems.libxml-ruby.buildFlags}"
    export BUNDLE_BUILD__NOKOGIRI="${builtins.concatStringsSep " " ruby.gems.nokogiri.buildFlags}"
    export BUNDLE_BUILD__PG="${builtins.concatStringsSep " " ruby.gems.pg.buildFlags}"
    export BUNDLE_BUILD__SQLITE3="${builtins.concatStringsSep " " ruby.gems.sqlite3.buildFlags} --with-cflags=-fdeclspec"
    export BUNDLE_BUILD__MYSQL2="--with-mysql-dir=${mysql80}"
    export BUNDLE_BUILD__HIREDIS="--with-cflags=-fdeclspec"
  '';
}

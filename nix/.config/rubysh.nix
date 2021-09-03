{ pkgs ? import <nixpkgs> { localSystem = "x86_64-darwin"; } }:

with pkgs;
let
  ruby = ruby_2_7;
  gems = ruby.gems;

in mkShell {
  buildInputs = [
    ncurses
    sqlite
    ruby
    # For regular gems, just run 'gem install'.
    # These are included here because they have awkward build requirements.
    gems.libxml-ruby
    gems.mysql2
    gems.pg
    gems.sassc
    gems.sqlite3
  ];

  shellHook = ''
    export GEM_HOME="$HOME/.gem/ruby/${ruby.version.libDir}-${stdenv.system}";
    export GEM_PATH="$HOME/.gem/ruby/${ruby.version.libDir}-${stdenv.system}";
  '';
}

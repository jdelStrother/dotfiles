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
}

# Update with:
# nix-env -irf .config/profile.nix
#
with import <nixpkgs> { }; {
  inherit awslogs coreutils emacsMacport fish git nixfmt nodejs-14_x parallel ruby_2_7 go;

  awscli2 = pkgs.awscli2.overrideAttrs(oldAttrs: rec {
    postBuild = ''
      PYTHONPATH=${oldAttrs.src}:$PYTHONPATH
      ${oldAttrs.src}/scripts/gen-ac-index --index-location $out/${python3.sitePackages}/awscli/data/ac.index
    '';
  });


}

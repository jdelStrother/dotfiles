{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf hm;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in {
  disabledModules = [ "targets/darwin/linkapps.nix" ];

  # Copy apps to ~/Applications/ rather than symlinking,
  # so they can be picked up by Alfred.
  # https://github.com/nix-community/home-manager/issues/1341
  config = mkIf isDarwin {
    home.activation.copyApplications = let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in hm.dag.entryAfter [ "writeBoundary" ] ''
      baseDir="$HOME/Applications/Home Manager Apps"
      if [ -d "$baseDir" ]; then
        rm -rf "$baseDir"
      fi
      mkdir -p "$baseDir"
      for appFile in ${apps}/Applications/*; do
        target="$baseDir/$(basename "$appFile")"
        $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
        $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
      done
    '';
  };
}

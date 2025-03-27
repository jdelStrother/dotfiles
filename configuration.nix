# darwin-rebuild switch --flake ~/dotfiles

{ pkgs, config, lib, unstable, inputs, ... }: {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [ pkgs.vim pkgs.git pkgs.fish ];
  environment.shells = [ pkgs.fish ];
  # Hack: https://github.com/ghostty-org/ghostty/discussions/2832
  environment.variables.XDG_DATA_DIRS =
    [ "$GHOSTTY_SHELL_INTEGRATION_XDG_DIR" ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  # add the current version of pkgs to the search path,
  # so that they consistently refer to the same thing (eg for `home-manager switch`)
  nix.nixPath = [ ("nixpkgs=" + toString pkgs.path) ];
  nix.settings = {
    sandbox = true;
    substituters =
      [ "https://nix-community.cachix.org" "https://devenv.cachix.org" ];
    trusted-public-keys = [
      "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    ];
    trusted-substituters = [
      "https://cachix.org/api/v1/cache/emacs"
      "https://cachix.org/api/v1/cache/nix-community"
    ];
  };
  # expose 'nixpkgs' and 'unstable' to the registry list, so that they're searchable with, eg, `nix search {nixpkgs,unstable} foo`
  nix.registry.nixpkgs.flake = inputs.nixpkgs;
  nix.registry.unstable.flake = inputs.unstable;

  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = true;
  programs.fish.enable = true;

  users.users.jon = {
    home = "/Users/jon";
    # Sadly setting the shell doesn't work unless you're creating a new user.
    # Use `chsh -s /run/current-system/sw/bin/fish` instead
    shell = pkgs.fish;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  system.defaults = {
    CustomUserPreferences = {
      # suppress crash popups, put them in Notification Center instead
      "com.apple.CrashReporter" = { "UseUNC" = 1; };
    };
  };
  security.pam.enableSudoTouchIdAuth = true;

  homebrew = {
    # Not actually using any homebrew packages right now
    enable = false;
    brews = [ ];
    # Keep things deterministic.
    onActivation.autoUpdate = false;
    # Properly uninstall all things not managed by Nix homebrew.
    onActivation.cleanup = "zap";
  };

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.extraSpecialArgs = { inherit unstable; };

  home-manager.users.jon = import ./home.nix;

  # manually specifying launchd rather than just `services.emacs.enable = true` because I want to be able to override TERMINFO
  launchd.user.agents.emacs = {
    command =
      "${config.home-manager.users.jon.programs.emacs.finalPackage}/bin/emacs --fg-daemon";
    path = [ config.environment.systemPath ];
    environment = {
      TERMINFO = "/Applications/Ghostty.app/Contents/Resources/terminfo";
    };
    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
    };
  };
}

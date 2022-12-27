# darwin-rebuild switch --flake ~/dotfiles

{ pkgs, config, lib, ... }:
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [
    pkgs.vim
    pkgs.git
    pkgs.fish
  ];
  environment.shells = [pkgs.fish];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  # add the current version of pkgs to the search path,
  # so that they consistently refer to the same thing (eg for `home-manager switch`)
  nix.nixPath = [ ("nixpkgs=" + toString pkgs.path) ];

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

  homebrew = {
    enable = true;
    brews = [
      "pinentry-mac"
    ];
    # Keep things deterministic.
    onActivation.autoUpdate = false;
    # Properly uninstall all things not managed by Nix homebrew.
    onActivation.cleanup = "zap";
  };

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  home-manager.users.jon = import ./home.nix ;
}

# darwin-rebuild switch --flake (realpath ~/.config/nix-darwin)

{ pkgs, ... }:
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

  programs.zsh.enable = true;
  programs.fish.enable = true;

  # Sadly setting the shell doesn't work unless you're creating a new user.
  # Use `chsh -s /run/current-system/sw/bin/fish` instead
  users.users.jon = {
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

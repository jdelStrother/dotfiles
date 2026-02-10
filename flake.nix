{
  description = "Jon's Nix";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-26.05-darwin";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      darwin,
      home-manager,
      nixpkgs,
      unstable,
      emacs-overlay,
      ...
    }@inputs:
    let
      darwinConfig = darwin.lib.darwinSystem rec {
        system = "aarch64-darwin";
        # add 'unstable' & 'inputs' as arguments that gets passed to modules
        specialArgs = {
          inherit inputs;
          unstable = unstable.legacyPackages.${system};
        };
        modules = [
          ./configuration.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs.overlays = [
              emacs-overlay.overlay
            ];
          }
        ];
      };
    in
    {
      darwinConfigurations."M1MBP" = darwinConfig;
      darwinConfigurations."M5MBP" = darwinConfig;
    };
}

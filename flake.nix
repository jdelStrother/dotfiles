{
  description = "M1 Nix";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-25.05-darwin";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:lnl7/nix-darwin/nix-darwin-25.05";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
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
    {
      darwinConfigurations."M1MBP" = darwin.lib.darwinSystem rec {
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
              # # fish 4
              # (self: super: { fish = unstable.legacyPackages.${system}.fish; })
              # # atuin 18.4
            ];
          }
        ];
      };
    };
}

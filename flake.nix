{
  description = "M1 Nix";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self, darwin, home-manager, nixpkgs, emacs-overlay, unstable, ... }: {
      darwinConfigurations."M1MBP" = darwin.lib.darwinSystem rec {
        system = "aarch64-darwin";
        # add 'unstable' as an argument that gets passed to modules
        specialArgs = { unstable = unstable.legacyPackages.${system}; };
        modules = [
          ./configuration.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs.overlays = [
              emacs-overlay.overlay
              # fix PATH in `nix shell` https://github.com/NixOS/nixpkgs/pull/352666
              (self: super: { fish = unstable.legacyPackages.${system}.fish; })
            ];
          }
        ];
      };
    };
}

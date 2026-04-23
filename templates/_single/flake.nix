{
  description = "dotfiles instantiation";

  inputs = {
    dotfiles.url = "github:seh/dotfiles";
    flake-parts.follows = "dotfiles/flake-parts";
    home-manager.follows = "dotfiles/home-manager";
    import-tree.follows = "dotfiles/import-tree";
    nix-darwin.follows = "dotfiles/nix-darwin";
    nixos.follows = "dotfiles/nixos";
    nixpkgs.follows = "dotfiles/nixpkgs";
    pre-commit-hooks.follows = "dotfiles/pre-commit-hooks";
    treefmt-nix.follows = "dotfiles/treefmt-nix";
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  outputs = {
    flake-parts,
    import-tree,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.dotfiles.flakeModules.default
        inputs.dotfiles.flakeModules.checks # optional
        inputs.dotfiles.flakeModules.style # optional
        (import-tree ./modules)
      ];

      # List the platforms of the hosts you define under
      # "./modules/hosts/". Replace or extend as appropriate.
      systems = [
        "aarch64-darwin"
      ];
    };
}

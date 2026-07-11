{
  description = "dotfiles instantiation";

  inputs = {
    dotfiles.url = "github:seh/dotfiles";
    flake-parts.follows = "dotfiles/flake-parts";
    git-hooks-nix.follows = "dotfiles/git-hooks-nix";
    import-tree.follows = "dotfiles/import-tree";
    nixpkgs.follows = "dotfiles/nixpkgs";
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
        inputs.dotfiles.modules.flake.style # optional
        (import-tree ./modules)
      ];

      # List the platforms of the hosts you define under the
      # "./modules/hosts/" directory. Replace or extend as
      # appropriate. Home Manager runs on all three common platforms;
      # likely alternatives: "aarch64-linux", "x86_64-linux".
      systems = [
        "aarch64-darwin"
      ];
    };
}

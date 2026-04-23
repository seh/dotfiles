# Basis of inspiration:
#   https://www.chrisportela.com/posts/home-manager-flake
#   https://github.com/sebastiant/dotfiles/blob/master/flake.nix
{
  description = "SEH Home Manager Flake";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    import-tree.url = "github:vic/import-tree";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    flake-parts,
    import-tree,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        # Exclude paths that the top-level flake-parts walk must not
        # evaluate: the embedded template flake trees under
        # "modules/templates/<name>/", which are separate flakes, not
        # flake-parts modules. The "modules/templates/default.nix"
        # file must still be imported; it registers the templates.
        (import-tree.matchNot ".*/templates/[^/]+/.*" ./modules)
      ];
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}

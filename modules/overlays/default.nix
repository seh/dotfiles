_: {
  flake.overlays = rec {
    default = nixpkgs;
    nixpkgs = import ./nixpkgs.nix;
  };
}

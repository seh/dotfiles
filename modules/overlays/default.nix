_: {
  flake.overlays = rec {
    default = nixpkgs;
    nixpkgs = import ./_nixpkgs.nix;
  };
}

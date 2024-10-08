{ inputs }:

rec {
  default =
    final: prev:
    # Forward to the right overlay by comparing releases
    let
      inherit (inputs) self;

      # NOTE: this doesn't compare individual commits, only releases
      isSameRelease = pkgs: flake: pkgs.lib.trivial.release == flake.lib.trivial.release;

      overlay = if isSameRelease prev inputs.nixpkgs then nixpkgs else final: prev: { };
    in
    overlay final prev;

  nixpkgs = import ./nixpkgs.nix;
}

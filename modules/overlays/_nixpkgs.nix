# Overlay for the nixpkgs input to this flake
final: prev: {
  # NB: The channel name "stable" here ideally matches the default
  # value of the "dotfiles.lix.channel" option, but this overlay runs at nixpkgs
  # instantiation time and cannot access flake-level options. A
  # caller that changes the channel option cannot easily rectify the
  # resulting divergence here short of supplying a replacement overlay.
  #
  # NB: We cannot use "final.lixPackageSets.stable.nix-direnv" or
  # "prev.lixPackageSets.stable.nix-direnv" directly here, as both
  # cause infinite recursion: the "nix-direnv" entry within
  # "lixPackageSets" is defined as "nix-direnv.override { ... }",
  # where the bare "nix-direnv" reference resolves back into the
  # fixed point of the package set being extended. Instead we
  # override "prev.nix-direnv" directly, supplying Lix ourselves.
  nix-direnv = prev.nix-direnv.override {
    nix = final.lixPackageSets.stable.lix;
  };
}

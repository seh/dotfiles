{
  flake.templates = rec {
    single-darwin = {
      path = ./_single-darwin;
      description = "Configure one Mac with nix-darwin and Home Manager.";
    };

    single-nixos = {
      path = ./_single-nixos;
      description = "Configure one Linux machine with NixOS and Home Manager.";
    };

    single-home = {
      path = ./_single-home;
      description = "Configure one user's home directory with Home Manager (no system-level configuration).";
    };

    default = single-darwin;
  };
}

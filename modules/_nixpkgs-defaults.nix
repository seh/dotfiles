# Plain-data nixpkgs configuration shared by the flake-parts
# evaluator's "perSystem._module.args.pkgs" (see
# "./nixpkgs-config.nix") and the standalone "pkgsFor" used by
# "lib.mkHome"/"lib.mkDarwin"/"lib.mkNixOS" (see
# "./lib/_constructors.nix"). Centralizing it here keeps both sites
# in lockstep: any nixpkgs "config" adjustment made for the
# flake-parts pipeline is seen equally by constructor-built systems.
#
# The leading underscore in the filename excludes this file from
# "import-tree" in "../flake.nix"; it is imported explicitly.
{
  config.allowUnfreePackages = [
    "1password"
    "1password-cli"
    "claude-code"
    "coder"
    "dropbox"
    "orbstack"
    "ngrok"
    "slack"
    "terraform"
    "zoom"
  ];
}

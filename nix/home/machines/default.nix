# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/home/machines/default.nix
{
  config,
  inputs,
  ...
}:

let
  inherit (inputs.self.lib) importHome pkgsFor;
  pkgs = pkgsFor "aarch64-darwin";
  username = config.dotfiles.user.name;
in
{
  flake.homeConfigurations = rec {
    basic = importHome ./basic.nix { inherit pkgs; };
    development = importHome ./development.nix { inherit pkgs; };

    # By default, Home Manager will look for an attribute whose name
    # matches "username@hostname" in order to build its
    # configuration. If no match is found, it falls back to the
    # current username.
    ${username} = basic;
  };
}

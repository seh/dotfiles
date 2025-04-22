# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/home/machines/default.nix
{ inputs }:

let
  inherit (inputs.self.lib) config importHome;
in
rec {
  basic = importHome ./basic.nix { };
  development = importHome ./development.nix { };

  # By default, Home Manager will look for an attribute whose name
  # matches "username@hostname" in order to build its
  # configuration. If no match is found, it falls back to the current
  # username.
  ${config.user.name} = basic;
}

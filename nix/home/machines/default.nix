# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/home/machines/default.nix
{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (inputs.self.lib) importHome pkgsFor;
  pkgs = pkgsFor "aarch64-darwin";
  username = config.dotfiles.user.name;
in
{
  flake.homeConfigurations =
    let
      commonMkHomeArgs = {
        inherit pkgs;

        modules = [
          {
            nixpkgs.config = {
              #allowUnfree = true;
              allowUnfreePredicate =
                pkg:
                builtins.elem (lib.getName pkg) [
                  "1password"
                  "1password-cli"
                  "claude-code"
                  #"discord"
                  "dropbox"
                  # TODO(seh): We don't install this explicitly, but it's an
                  # implicit dependency of some other package.
                  "ngrok"
                  "slack"
                ];
            };
          }
        ];
      };
    in
    rec {
      basic = importHome ./basic.nix commonMkHomeArgs;
      development = importHome ./development.nix commonMkHomeArgs;

      # By default, Home Manager will look for an attribute whose name
      # matches "username@hostname" in order to build its
      # configuration. If no match is found, it falls back to the
      # current username.
      ${username} = basic;
    };
}

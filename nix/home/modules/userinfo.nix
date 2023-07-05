{ lib, dotfiles, ... }:

let
  inherit (dotfiles.lib) config;
  hasGPGSigningKey = builtins.hasAttr "gpgKey" config.user;
in
{
  config = {
    dotfiles.git.config = {
      commit.gpgsign = hasGPGSigningKey;
      user = {
        name = config.user.fullName;
        email = config.user.email;
      } // lib.optionalAttrs hasGPGSigningKey {
        signingkey = config.user.ggpKey;
      };
    };
    # TODO(seh): Consider setting Emacs variables here
    # (user-full-name, user-mail-address).
  };
}

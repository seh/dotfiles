{ lib, dotfiles, ... }:

let
  inherit (dotfiles.lib) config;
  hasGPGSigningKey = builtins.hasAttr "gpgKey" config.user;
in
{
  config = {
    dotfiles.git.config =
      {
        user = {
          name = config.user.fullName;
          email = config.user.email;
        } // lib.optionalAttrs hasGPGSigningKey { signingKey = config.user.gpgKey; };
      }
      // lib.optionalAttrs hasGPGSigningKey {
        commit.gpgSign = true;
        tag.gpgSign = true;
      };
    dotfiles.jujutsu.extraSettings =
      {
        user = {
          name = config.user.fullName;
          email = config.user.email;
        };
      }
      // lib.optionalAttrs hasGPGSigningKey {
        git = {
          # NB: Opt for this instead of enabling "signing.sign-all".
          sign-on-push = true;
        };
        signing = {
          backend = "gpg";
          key = config.user.gpgKey;

          backends = {
            gpg = {
              allow-expired-keys = false;
            };
          };
        };
      };
    # TODO(seh): Consider setting Emacs variables here
    # (user-full-name, user-mail-address).
  };
}

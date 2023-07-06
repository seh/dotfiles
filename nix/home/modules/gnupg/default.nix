# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/modules/gnupg/default.nix
{ lib, config, pkgs, dotfiles, ... }:

let
  inherit (lib) mkDefault mkEnableOption mkIf mkMerge mkOption optional;
  cfg = config.dotfiles.gnupg;
  # NB: Merely mentioning the "pinentry_mac" package here will make it
  # available in the Nix store. See
  # https://github.com/midchildan/dotfiles/issues/97 for further
  # discussion.
  pinentry-mac = "${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac";
in
{
  options.dotfiles.gnupg = {
    enable = mkEnableOption "GnuPG";

    enablePackage = mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to install the GnuPG package.";
    };

    package = mkOption {
      type = lib.types.package;
      default = pkgs.gnupg;
      defaultText = lib.literalExpression "pkgs.gnupg";
      description = "The GnuPG package to use.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = optional cfg.enablePackage cfg.package;

      home.file.".gnupg/gpg.conf".source = pkgs.substituteAll {
        src = ./gpg.conf;
        gpgKey = dotfiles.lib.config.user.gpgKey or "@removeMe@";
        # Remove lines in which we failed to replace the intended
        # content correctly.
        postInstall = ''
          sed -i '/@removeMe@/d' "$target"
        '';
      };
    }

    # NB: Home Manager's "gpg-agent" service is only supported on
    # Linux for now.
    # See:
    #   https://github.com/nix-community/home-manager/issues/91
    #   https://github.com/nix-community/home-manager/issues/3864
    (mkIf pkgs.stdenv.isDarwin {
      home.file.".gnupg/gpg-agent.conf".text = ''
        default-cache-ttl 600
        max-cache-ttl 7200

        enable-ssh-support

        pinentry-program ${pinentry-mac}
      '';

      launchd.agents.gpg-agent = mkIf cfg.enablePackage {
        enable = true;
        config = {
          ProgramArguments = [
            "${cfg.package}/bin/gpgconf"
            "--launch"
            "gpg-agent"
          ];
          RunAtLoad = true;
        };
      };
    })
  ]);
}

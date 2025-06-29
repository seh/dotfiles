# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/modules/gnupg/default.nix
{
  lib,
  config,
  pkgs,
  dotfiles,
  ...
}:

let
  inherit (lib)
    mkDefault
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    optional
    optionalString
    ;
  inherit (config.dotfiles) flakeOptions;
  userConfig = flakeOptions.user;
  hasGPGSigningKey = builtins.hasAttr "gpgKey" userConfig && userConfig.gpgKey != "";
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

    enableSSHSupport = mkEnableOption "GnuPG SSH support";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = optional cfg.enablePackage cfg.package;

      home.file.".gnupg/gpg.conf".source = pkgs.replaceVarsWith {
        src = ./gpg.conf;
        replacements = {
          gpgKey = if hasGPGSigningKey then userConfig.gpgKey else "!!!remove!!!";
        };
        # Remove lines in which we failed to replace the intended
        # content correctly.
        postInstall = ''
          sed -i '/!!!remove!!!/d' "$target"
        '';
      };
    }

    # NB: Home Manager's "gpg-agent" service is only supported on
    # Linux for now.
    # See:
    #   https://github.com/nix-community/home-manager/issues/91
    #   https://github.com/nix-community/home-manager/issues/3864
    (mkIf pkgs.stdenv.isDarwin {
      home.file.".gnupg/gpg-agent.conf".text =
        ''
          default-cache-ttl 600
          max-cache-ttl 7200

          pinentry-program ${pinentry-mac}
        ''
        + optionalString cfg.enableSSHSupport ''

          enable-ssh-support
        '';

      # Emulate Home Manager's "gpg-agent" service's treatment.
      # See:
      #   https://github.com/nix-community/home-manager/blob/f5b03feb33629cb2b6dd513935637e8cc718a5ba/modules/services/gpg-agent.nix#L240-L244
      home.sessionVariablesExtra = optionalString cfg.enableSSHSupport ''
        # NB: On macOS, the "com.openssh.ssh-agent" launchd service
        # sets SSH_AUTH_SOCK to a value like the following:
        #     /private/tmp/com.apple.launchd.ac809KiF2l/Listeners
        if [[ -z "''${SSH_AUTH_SOCK}" ]] || [[ "''${SSH_AUTH_SOCK}" =~ '^/private/tmp/com\.apple\.launchd\.[^/]+/Listeners''$' ]]; then
          export SSH_AUTH_SOCK="$(${cfg.package}/bin/gpgconf --list-dirs agent-ssh-socket)"
        fi
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

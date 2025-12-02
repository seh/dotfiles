{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    optionals
    ;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
{
  options.dotfiles.profiles.essential.enable =
    mkEnableOption "essential packages for servers and desktops alike";

  config = mkIf config.dotfiles.profiles.essential.enable {
    home = {
      packages =
        with pkgs;
        [
          # TODO(seh): Consider moving some of these into separate files
          # depending on which kinds of machines should include them
          # (e.g. personal v. work.
          _1password-cli
          # NB: The "_1password-gui" does not work on macOS for now; it
          # refuses to run if it's not in the "/Applications" directory.
          age
          btop
          coreutils
          d2
          deadnix
          dig
          duckdb
          hunspell
          hunspellDicts.en-us
          jless
          jq
          jqp
          lsof
          miller
          # NB: We also configure this as the flake-level tool to use
          # with the "nix check" command, but that configuraton does
          # not link the "nixfmt" program provided by this package
          # from a directory on our path along with these other tools.
          nixfmt-rfc-style
          openssl
          sops
          sqlite
          statix
          tailscale
          tmux
          tree
          typst
          typstyle
          unzip
          watchman
          wget
          yq-go
          yubikey-manager
          yubikey-personalization
          zoom-us
        ]
        ++ optionals (!isDarwin) [
          dnsutils
          file
          netcat
          whois
        ]
        ++ optionals isDarwin [
          ssh-copy-id
          watch
        ];
    };

    programs = {
      bat = {
        enable = true;
        #config = { };
      };
      carapace = {
        enable = true;
        enableBashIntegration = true;
        enableNushellIntegration = true;
        enableZshIntegration = true;
      };
    };

    # TODO(seh): The "gpg-agent" service is only supported on Linux for now.
    # See:
    #   https://github.com/nix-community/home-manager/issues/91
    #   https://github.com/nix-community/home-manager/issues/3864
    # services.gpg-agent = {
    #   enable = true;
    #   enableSshSupport = true;
    #   defaultCacheTtl = 600;
    #   maxCacheTtl = 7200;
    #   pinentryFlavor = "emacs";     # TODO(seh): See https://github.com/NixOS/nixpkgs/issues/240819 for using "pinentry-mac".
    # };

    dotfiles = {
      bash = {
        enable = lib.mkDefault true;
      };
      delta = {
        enable = lib.mkDefault true;
      };
      emacs = {
        enable = lib.mkDefault true;
      };
      git = {
        enable = lib.mkDefault true;
      };
      jujutsu = {
        enable = lib.mkDefault true;
      };
      kitty = {
        enable = lib.mkDefault true;
      };
      nh = {
        enable = lib.mkDefault true;
      };
      nushell = {
        enable = lib.mkDefault true;
      };
      ssh = {
        enable = lib.mkDefault true;
        enableMultiplexing = lib.mkDefault true;
      };
      zsh = {
        enable = lib.mkDefault true;
      };
    };
  };
}

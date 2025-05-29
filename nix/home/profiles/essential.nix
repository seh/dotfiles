{
  config,
  lib,
  pkgs,
  dotfiles,
  ...
}:

let
  inherit (lib)
    mkDefault
    mkEnableOption
    mkIf
    optionals
    ;
  inherit (pkgs.stdenv.hostPlatform) isDarwin system;
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
          delta
          dig
          hunspell
          hunspellDicts.en-us
          jj-fzf
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
          sile
          sops
          sqlite
          tailscale
          tmux
          tree
          unzip
          wget
          yq-go
          yubikey-manager
          zoom-us
        ]
        ++ optionals (!isDarwin) [
          dnsutils
          file
          git
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
      emacs = {
        enable = lib.mkDefault true;
      };
      git.config = {
        alias = {
          # See https://ses4j.github.io/2020/04/01/git-alias-recent-branches/.
          # (And https://ses4j.github.io/2020/04/01/git-alias-recent-branches/#comment-4863945965 for the right-aligned column.)
          lb = ''
            !git reflog show --pretty=format:'%gs ~ %gd' --date=relative | grep 'checkout:' | grep -oE '[^ ]+ ~ .*' | awk -F~ '!seen[''$1]++' | head -n 10 | awk -F' ~ HEAD@{' '{printf(\"  \\033[33m%12s:\\t\\033[37m %s\\033[0m\\n\", substr(''$2, 1, length(''$2)-1), ''$1)}'
          '';
          rb = ''
            for-each-ref --sort='-authordate:iso8601' --format=' %(align:25)%(color:green)%(authordate:relative)%(end)%(color:bold blue)%(refname:short)' refs/heads
          '';
        };
      };
      jujutsu = {
        enable = lib.mkDefault true;
      };
      kitty = {
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

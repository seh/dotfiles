{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkMerge optionals;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in {
  config = mkMerge [
    {
      dotfiles._knownTags = ["essential"];
    }
    (mkIf (config.dotfiles._host.hasTag "essential") {
      home = {
        packages = with pkgs;
          [
            # TODO(seh): Consider moving some of these into separate files
            # depending on which kinds of machines should include them
            # (e.g. personal v. work.
            _1password-cli
            # NB: The "_1password-gui" does not work on macOS for now; it
            # refuses to run if it's not in the "/Applications" directory.
            age
            # NB: We also configure this as the flake-level tool to use
            # with the "nix check" command, but that configuraton does
            # not link the "alejandra" program provided by this package
            # from a directory on our path along with these other tools.
            alejandra
            btop
            coreutils
            d2
            deadnix
            duckdb
            glow
            hunspell
            hunspellDicts.en-us
            jless
            lsof
            miller
            nixfmt
            openssl
            rumdl
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
          ]
          ++ optionals (!isDarwin) [
            dnsutils
            file
            netcat
            # NB: On Darwin, /usr/bin/perl is available system-wide.
            perl
            whois
          ]
          ++ optionals isDarwin [
            # NB: On Linux, "dig" is provided by "dnsutils" above.
            dig
            # NB: On Linux, "ssh-copy-id" is bundled with OpenSSH.
            ssh-copy-id
          ]
          ++ optionals (!isLinux) [
            # NB: On Linux, "watch" is provided by "procps-ng".
            watch
          ]
          ++ [
            # NB: This works around a gap in the "nix-mode" Emacs mode's
            # invocation of a formatting command.
            (writeShellScriptBin "alejandra-quiet" ''
              ${lib.getExe alejandra} --quiet "$@"
            '')
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
        jq = {
          enable = true;
        };
        jqp = {
          enable = true;
        };
      };

      dotfiles = {
        bash = {
          enable = lib.mkDefault true;
        };
        difftastic = {
          enable = lib.mkDefault true;
        };
        emacs = {
          enable = lib.mkDefault true;
        };
        git = {
          enable = lib.mkDefault true;
        };
        gnupg = {
          enable = lib.mkDefault true;
          enableSSHSupport = true;
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
    })
  ];
}

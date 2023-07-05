{ config, lib, pkgs, dotfiles, ... }:

let
  inherit (lib) mkDefault mkEnableOption mkIf optionals;
  inherit (pkgs.stdenv.hostPlatform) isDarwin system;
in
{
  options.dotfiles.profiles.essential.enable =
    mkEnableOption "essential packages for servers and desktops alike";

  config = mkIf config.dotfiles.profiles.essential.enable {
    home = {
      packages = with pkgs;
        [
          # TODO(seh): Consider moving some of these into separate files
          # depending on which kinds of machines should include them
          # (e.g. personal v. work.
          _1password
          # NB: The "_1password-gui" does not work on macOS for now; it
          # refuses to run if it's not in the "/Applications" directory.
          age
          bazel # TODO(seh): Sholud we install this directly?
          bazel-buildtools
          bazelisk
          coreutils
          cue
          dig
          elvish
          gnupg
          go
          go-jsonnet
          go-tools
          gofumpt
          golangci-lint
          gopls
          hunspell
          hunspellDicts.en-us
          jless
          jq
          kubectl
          kustomize
          lsof
          nixpkgs-fmt
          openssl
          pinentry_mac
          sbcl
          shellcheck
          sops
          sqlite
          tailscale
          tmux
          tree
          unzip
          wget
          yq-go
          yubikey-manager
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

    programs.bat = {
      enable = true;
      config = {
        theme = "Monokai Extended Light";
      };
    };
    programs.direnv = {
      enable = true;
      nix-direnv = { enable = true; };
    };
    programs.fzf = {
      enable = true;
      defaultOptions = [
        "--info=inline"
        "--bind=ctrl-r:toggle-sort"
      ];
    };
    programs.gpg = {
      enable = true;
    };
    programs.k9s = {
      enable = true;
      # TODO(seh): Configure settings.
    };
    programs.go = {
      enable = true;
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
      emacs = {
        enable = lib.mkDefault true;
      };
      git.config = {
        branch = {
          autoSetupMerge = "always";
          autoSetupRebase = "local";
        };
        rebase = {
          autosqaush = true;
        };
        rerere = {
          enabled = 1;
          autoupdate = 1;
        };
      };
      zsh = {
        enable = lib.mkDefault true;
      };
    };
  };
}

# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/development.nix
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
    mkOption
    optionals
    ;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux system;
  isGenericLinux = (config.targets.genericLinux.enable or false);
  isNixOS = isLinux && !isGenericLinux;
  cfg = config.dotfiles.profiles;
  myPkgs = dotfiles.packages.${system};
in
{
  options.dotfiles.profiles.development = {
    enable = mkEnableOption "development packages";

    enableKubernetes = mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to install Kubernetes-related development tools.";
    };

    enableLanguageServers = mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to install per-language servers that implement the LSP.";
    };

    enableRust = mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to install Rust-related development tools.";
    };
  };

  config = mkIf cfg.development.enable {
    home.packages =
      with pkgs;
      [
        aws-vault
        awscli2
        bazel-buildtools
        bazel_7 # TODO(seh): Upgrade this to Bazel 8 once it's available.
        bazelisk
        bombardier
        bruno
        buf
        cue
        fswatch
        github-cli
        go-jsonnet
        go-tools
        gofumpt
        golangci-lint
        google-cloud-sdk
        mkcert
        ngrok
        nodePackages.prettier
        nodePackages.typescript
        nssTools # For use with mkcert
        # See the following issues and patches:
        # https://github.com/NixOS/nixpkgs/issues/408192
        # https://github.com/NixOS/nixpkgs/pull/408599
        #podman
        ripgrep
        sbcl
        shellcheck
        tenv
        trurl
        watchman
        # NB: The Wireshark application still fails, per this report:
        #     https://github.com/NixOS/nixpkgs/issues/103944#issuecomment-1627759940
        # wireshark
      ]
      ++ optionals isDarwin [
        # Without QEMU available, Podman can't work as intended atop
        # macOS.
        qemu
      ]
      ++ optionals cfg.development.enableKubernetes [
        fluxcd
        k3d
        k9s
        kind
        kubectl
        kubernetes-helm
        kustomize
      ]
      ++ optionals cfg.development.enableLanguageServers [
        bash-language-server
        gopls
        graphql-language-service-cli
        jsonnet-language-server
        nixd # Compare with "nil"
        nodePackages.vscode-json-languageserver
        nodePackages.typescript-language-server
        postgres-lsp # Compare with "sqls"
        taplo # For TOML files
        terraform-ls
        yaml-language-server
      ]
      ++ optionals cfg.development.enableRust [
        # NB: rustup includes the following:
        # - cargo
        # - rust-analyzer
        # - rustfmt
        rustup
      ];

    programs.go = {
      enable = true;
    };

    programs.granted = {
      enable = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
    };

    programs.k9s = mkIf cfg.development.enableKubernetes {
      enable = true;
      # TODO(seh): Configure settings.
    };

    dotfiles.git.config = {
      branch = {
        autoSetupMerge = "always";
        autoSetupRebase = "local";
      };
      merge = {
        conflictStyle = "zdiff3";
      };
      rebase = {
        autosqaush = true;
      };
      rerere = {
        enabled = 1;
        autoupdate = 1;
      };
      # Per https://golang.org/doc/faq#git_https, for gopls against private repositories:
      url = {
        "ssh://git@github.com/" = {
          insteadOf = "https://github.com/";
        };
      };
    };

    dotfiles.gnupg = {
      enable = mkDefault true;
      enablePackage = mkDefault (!isNixOS);
      enableSSHSupport = true;
    };
  };
}

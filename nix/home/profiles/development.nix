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
  };

  config = mkIf cfg.development.enable {
    home.packages =
      with pkgs;
      [
        aws-vault
        awscli2
        bazel # TODO(seh): Sholud we install this directly?
        bazel-buildtools
        bazelisk
        bombardier
        cue
        github-cli
        go-jsonnet
        go-tools
        gofumpt
        golangci-lint
        gopls
        nodePackages.prettier
        nodePackages.typescript
        nodePackages.typescript-language-server
        ngrok
        podman
        # See https://github.com/NixOS/nixpkgs/issues/259147 and
        # https://github.com/postmanlabs/postman-app-support/issues/12383
        # for why we can't install this package for now.
        #postman
        sbcl
        shellcheck
        terraform
        # NB: tfenv is not available as a package.
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
      ];

    programs.go = {
      enable = true;
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

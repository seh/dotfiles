# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/development.nix
{
  config,
  lib,
  pkgs,
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
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
  isGenericLinux = config.targets.genericLinux.enable or false;
  isNixOS = isLinux && !isGenericLinux;
  cfg = config.dotfiles.profiles;
in
{
  options.dotfiles.profiles.development = {
    enable = mkEnableOption "development packages";

    enabledCloudProviders = {
      aws = mkEnableOption "AWS-related development tools";
      azure = mkEnableOption "Azure-related development tools";
      gcp = mkEnableOption "Google Cloud Platform-related development tools";
    };

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
        bazel-buildtools
        bazel_8
        bazelisk
        bombardier
        # TODO(seh): Enable this again after
        # https://github.com/NixOS/nixpkgs/pull/453796 is available.
        #bruno
        buf
        copilot-cli
        cue
        fswatch
        git-town
        github-cli
        go-jsonnet
        go-tools
        gofumpt
        golangci-lint
        goperf
        mkcert
        ngrok
        nodePackages.prettier
        nodePackages.typescript
        nssTools # For use with mkcert
        podman
        ripgrep
        sbcl
        shellcheck
        tenv
        # TODO(seh): Enable this again after
        # https://github.com/NixOS/nixpkgs/issues/458008 is fixed.
        #wireshark
      ]
      ++ optionals isDarwin [
        # Without QEMU available, Podman can't work as intended atop
        # macOS.
        qemu
      ]
      ++ optionals cfg.development.enabledCloudProviders.aws [
        aws-vault
        awscli2
      ]
      ++ optionals cfg.development.enabledCloudProviders.azure [
        azure-cli
      ]
      ++ optionals cfg.development.enabledCloudProviders.gcp [
        (google-cloud-sdk.withExtraComponents (
          with google-cloud-sdk.components;
          [
            gke-gcloud-auth-plugin
          ]
        ))
      ]
      ++ optionals cfg.development.enableKubernetes [
        fluxcd
        k3d
        k9s
        kind
        kpt
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
        postgres-language-server # Compare with "sqls"
        starpls
        taplo # For TOML files
        tinymist # For typst files
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

    programs = {
      go = {
        enable = true;
      };

      granted = {
        enable = cfg.development.enabledCloudProviders.aws;
        enableFishIntegration = true;
        enableZshIntegration = true;
      };

      k9s = {
        enable = cfg.development.enableKubernetes;
        # TODO(seh): Configure settings.
      };
    };

    dotfiles = {
      claude = {
        enable = lib.mkDefault true;
      };
      gemini = {
        enable = lib.mkDefault true;
      };
      gnupg = {
        enable = mkDefault true;
        enablePackage = mkDefault (!isNixOS);
        enableSSHSupport = true;
      };
      helix = {
        enable = lib.mkDefault true;
      };
      opencode = {
        enable = lib.mkDefault true;
      };
    };
  };
}

# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/development.nix
{
  flake.profileModules.homeManager.development = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (lib) mkIf mkMerge optionals;
    inherit (pkgs.stdenv.hostPlatform) isDarwin;
    hasTag = config.dotfiles._host.hasTag;
  in {
    config = mkMerge [
      {
        dotfiles._knownTags = [
          "development"
          "kubernetes"
          "rust"
          "aws"
          "azure"
          "gcp"
          "language-servers"
        ];
      }
      (mkIf (hasTag "development") {
        home.packages = with pkgs;
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
            lcov
            mkcert
            ngrok
            nodejs-slim
            nssTools # For use with mkcert
            podman
            prettier
            sbcl
            shellcheck
            shfmt
            tenv
            typescript
            # TODO(seh): Enable this again after
            # https://github.com/NixOS/nixpkgs/issues/458008 is fixed.
            #wireshark
          ]
          ++ optionals isDarwin [
            orbstack
            # Without QEMU available, Podman can't work as intended atop
            # macOS.
            qemu
          ]
          ++ optionals (hasTag "aws") [
            aws-vault
            awscli2
          ]
          ++ optionals (hasTag "azure") [
            azure-cli
          ]
          ++ optionals (hasTag "gcp") [
            (google-cloud-sdk.withExtraComponents (
              with google-cloud-sdk.components; [
                gke-gcloud-auth-plugin
              ]
            ))
          ]
          ++ optionals (hasTag "kubernetes") (
            [
              fluxcd
              k3d
              k9s
              kind
              kpt
              kubernetes-helm
              kustomize
            ]
            # NB: On Darwin, "kubectl" is provided by OrbStack.
            ++ optionals (!isDarwin) [
              kubectl
            ]
          )
          ++ optionals (hasTag "language-servers") [
            bash-language-server
            emmylua-ls
            gopls
            graphql-language-service-cli
            jq-lsp
            jsonnet-language-server
            nixd # Compare with "nil"
            postgres-language-server # Compare with "sqls"
            starpls
            taplo # For TOML files
            terraform-ls
            tinymist # For typst files
            typescript-language-server
            vscode-json-languageserver
            yaml-language-server
          ]
          ++ optionals (hasTag "rust") [
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
            enable = hasTag "aws";
            enableFishIntegration = true;
            enableZshIntegration = true;
          };

          k9s = {
            enable = hasTag "kubernetes";
            # TODO(seh): Configure settings.
          };

          ripgrep = {
            enable = true;
          };
        };

        dotfiles = {
          claude = {
            enable = lib.mkDefault true;
          };
          coder = {
            enable = lib.mkDefault true;
            enableSSHIntegration = lib.mkDefault true;
          };
          helix = {
            enable = lib.mkDefault true;
          };
          opencode = {
            enable = lib.mkDefault true;
          };
        };
      })
    ];
  };
}

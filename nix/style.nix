{ lib, inputs, ... }:

{
  imports = [
    inputs.treefmt-nix.flakeModule
    inputs.pre-commit-hooks.flakeModule
  ];

  perSystem = {
    treefmt = {
      projectRootFile = lib.mkDefault "flake.nix";
      programs = {
        buildifier.enable = lib.mkDefault true;
        gofumpt.enable = lib.mkDefault true;
        nixfmt.enable = lib.mkDefault true;
        prettier = {
          enable = lib.mkDefault true;
          settings = {
            overrides = [
              {
                files = [
                  "*.md"
                  "*.mdx"
                ];
                # The default value is 80.
                options.printWidth = 70;
              }
            ];
            proseWrap = lib.mkDefault "always";
          };
        };
        shellcheck.enable = lib.mkDefault true;
        terraform.enable = lib.mkDefault true;
      };
      settings = {
        global = {
          excludes = [
            "pnpm-lock.yaml"
          ];
        };
        formatter = {
          buildifier = {
            includes = [
              "*/*.bzl"
              "*/BUILD.bazel"
              "BUILD.bazel"
              "MODULE.bazel"
              "REPO.bazel"
              "WORKSPACE.bazel"
              "WORKSPACE.bzlmod"
            ];
          };
          gofumpt = {
            excludes = [
              "*.gen.go"
              "*/generated/*.go"
            ];
          };
          shellcheck = {
            includes = [
              "*.bash"
              "*.envrc"
              "*.envrc.*"
              "*.sh"
            ];
            options = [
              "--external-sources"
              "--source-path=SCRIPTDIR"
            ];
          };
        };
      };
    };

    pre-commit.settings.hooks = {
      check-executables-have-shebangs.enable = lib.mkDefault true;
      check-json.enable = lib.mkDefault true;
      check-merge-conflicts.enable = lib.mkDefault true;
      check-shebang-scripts-are-executable.enable = lib.mkDefault true;
      check-symlinks.enable = lib.mkDefault true;
      check-toml.enable = lib.mkDefault true;
      check-yaml.enable = lib.mkDefault true;
      deadnix.enable = lib.mkDefault true;
      detect-private-keys.enable = lib.mkDefault true;
      end-of-file-fixer.enable = lib.mkDefault true;
      mixed-line-endings.enable = lib.mkDefault true;
      pretty-format-json.enable = lib.mkDefault true;
      statix.enable = lib.mkDefault true;
      treefmt.enable = lib.mkDefault true;
      trim-trailing-whitespace = {
        enable = lib.mkDefault true;
        args = [
          # Preclude complaints about ending list items with
          # hanging paragraphs with two trailing spaces.
          #
          # See https://github.com/pre-commit/pre-commit-hooks?tab=readme-ov-file#trailing-whitespace.
          "--markdown-linebreak-ext=md"
        ];
      };
    };
  };
}

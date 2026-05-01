# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/development.nix
{
  dotfiles.knownProfiles = ["development"];

  dotfiles.profileModules.homeManager.development = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (lib) mkIf optionals;
    inherit (pkgs.stdenv.hostPlatform) isDarwin;
    inherit (config.dotfiles._host) activatesProfile;
  in {
    config = mkIf (activatesProfile "development") {
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
        ];

      programs = {
        go = {
          enable = true;
        };

        ripgrep = {
          enable = true;
        };
      };

      dotfiles = {
        coder = {
          enableSSHIntegration = lib.mkDefault true;
        };
      };
    };
  };
}

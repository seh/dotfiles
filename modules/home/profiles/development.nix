# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/development.nix
{flakeLib, ...}:
flakeLib.mkProfile "development" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: let
    inherit (lib) optionals;
    inherit (pkgs.stdenv.hostPlatform) isDarwin;
  in {
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
        cue
        diffnav
        fswatch
        git-town
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
      gh = {
        enable = true;
      };

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
}

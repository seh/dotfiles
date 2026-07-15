# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/development.nix
{flakeLib, ...}:
flakeLib.mkProfile "development" {
  # The development profile brings along the features a working
  # engineer's machine wants.
  implies = [
    "cloud/aws"
    "cloud/azure"
    "cloud/gcp"
    "cloud/terraform"
    "coder"
    "dev/bazel"
    "dev/containers"
    "dev/coverage"
    "dev/diffnav"
    "dev/editorconfig"
    "dev/emulation"
    "editor/helix"
    "kubernetes"
    "lang/common-lisp"
    "lang/cue"
    "lang/go"
    "lang/javascript"
    "lang/jsonnet"
    "lang/lua"
    "lang/protobuf"
    "lang/rust"
    "lang/shell"
    "model-agent/claude"
    "model-agent/copilot"
    "model-agent/opencode"
    "net/http-clients"
    "net/local-tls"
    "net/tunnels"
    "vcs/commit-signing"
    "vcs/git-town"
    "vcs/github"
  ];

  homeManager = {lib, ...}: {
    dotfiles = {
      coder = {
        enableSSHIntegration = lib.mkDefault true;
      };
    };
  };
}

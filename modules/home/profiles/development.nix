# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/development.nix
{flakeLib, ...}:
flakeLib.mkProfile "development" {
  # The development profile brings along the features a working
  # engineer's machine wants.
  implies = [
    "dev/bazel"
    "dev/containers"
    "dev/coverage"
    "dev/diffnav"
    "dev/emulation"
    "editor/helix"
    "net/http-clients"
    "net/local-tls"
    "net/tunnels"
    "vcs/commit-signing"
    "vcs/github"
  ];
}

{flakeLib, ...}:
# The Go toolchain, active when this configuration expresses the
# "lang/go" interest. Home Manager's "programs.go" module installs the
# SDK; the packages below add static analysis, a stricter formatter, a
# linter runner, and benchmark analysis.
flakeLib.mkFeature "lang/go/tools" {
  preconditions = ["lang/go"];
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    home.packages = with pkgs; [
      go-tools
      gofumpt
      golangci-lint
      goperf
    ];

    programs.go = {
      enable = true;
      # Home Manager's "programs.go" module selects the "go" package,
      # which nixpkgs binds to whichever release it treats as current.
      # Take the newest release it packages instead.
      package = lib.mkDefault pkgs.go_latest;
    };
  };
}

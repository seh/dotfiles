{flakeLib, ...}:
flakeLib.mkFeature "lang/go" {
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

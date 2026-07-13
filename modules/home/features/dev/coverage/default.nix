{flakeLib, ...}:
flakeLib.mkFeature "dev/coverage" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      lcov
    ];
  };
}

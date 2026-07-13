{flakeLib, ...}:
flakeLib.mkFeature "lang/shell" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      shellcheck
      shfmt
    ];
  };
}

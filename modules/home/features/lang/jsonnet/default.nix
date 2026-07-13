{flakeLib, ...}:
flakeLib.mkFeature "lang/jsonnet" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      go-jsonnet
    ];
  };
}

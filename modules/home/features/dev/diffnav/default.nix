{flakeLib, ...}:
flakeLib.mkFeature "dev/diffnav" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      diffnav
    ];
  };
}

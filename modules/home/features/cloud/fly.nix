{flakeLib, ...}:
flakeLib.mkFeature "cloud/fly" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      flyctl
      sprite
    ];
  };
}

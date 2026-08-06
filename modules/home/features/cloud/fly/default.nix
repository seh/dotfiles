{flakeLib, ...}:
flakeLib.mkFeature "cloud/fly" {
  # The "sprite" package is unfree, so instantiating it demands
  # toleration for that name.
  unfreePackages = ["sprite"];

  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      flyctl
      sprite
    ];
  };
}

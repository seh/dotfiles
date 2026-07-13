{flakeLib, ...}:
flakeLib.mkFeature "dev/containers" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      podman
    ];
  };
}

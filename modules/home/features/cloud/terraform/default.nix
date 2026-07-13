{flakeLib, ...}:
flakeLib.mkFeature "cloud/terraform" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      tenv
    ];
  };
}

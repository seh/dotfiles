{flakeLib, ...}:
flakeLib.mkFeature "cloud/azure" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      azure-cli
    ];
  };
}

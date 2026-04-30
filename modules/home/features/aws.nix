{flakeLib, ...}:
flakeLib.mkFeature "cloud/aws" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      aws-vault
      awscli2
    ];

    programs.granted = {
      enable = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
    };
  };
}

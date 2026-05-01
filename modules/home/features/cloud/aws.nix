{flakeLib, ...}:
flakeLib.mkFeature "cloud/aws" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      aws-vault
    ];

    programs.awscli = {
      enable = true;
    };

    programs.granted = {
      enable = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
    };
  };
}

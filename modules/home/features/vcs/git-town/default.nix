{flakeLib, ...}:
flakeLib.mkFeature "vcs/git-town" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      git-town
    ];
  };
}

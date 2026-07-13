{flakeLib, ...}:
flakeLib.mkFeature "lang/cue" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      cue
    ];
  };
}

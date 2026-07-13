{flakeLib, ...}:
flakeLib.mkFeature "lang/protobuf" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      buf
    ];
  };
}

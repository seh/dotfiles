{flakeLib, ...}:
flakeLib.mkFeature "lang/javascript" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      nodejs-slim
      prettier
      typescript
    ];
  };
}

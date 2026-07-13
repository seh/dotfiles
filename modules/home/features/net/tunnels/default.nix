{flakeLib, ...}:
flakeLib.mkFeature "net/tunnels" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      ngrok
    ];
  };
}

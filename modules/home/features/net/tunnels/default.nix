{flakeLib, ...}:
flakeLib.mkFeature "net/tunnels" {
  unfreePackages = ["ngrok"];

  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      ngrok
    ];
  };
}

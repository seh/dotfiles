{flakeLib, ...}:
flakeLib.mkFeature "net/local-tls" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      mkcert
      nssTools # For use with mkcert
    ];
  };
}

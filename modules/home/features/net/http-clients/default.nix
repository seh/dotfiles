{flakeLib, ...}:
flakeLib.mkFeature "net/http-clients" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      bombardier
      # TODO(seh): Enable this again after
      # https://github.com/NixOS/nixpkgs/pull/453796 is available.
      #bruno
    ];
  };
}

{flakeLib, ...}:
# The Jsonnet toolchain, active when this configuration expresses the
# "lang/jsonnet" interest. The "go-jsonnet" program is the
# implementation of the language written in Go.
flakeLib.mkFeature "lang/jsonnet/tools" {
  preconditions = ["lang/jsonnet"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      go-jsonnet
    ];
  };
}

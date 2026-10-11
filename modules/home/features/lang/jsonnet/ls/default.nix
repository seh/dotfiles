{flakeLib, ...}:
# The Jsonnet language server, active only when both the
# "dev/language-servers" and "lang/jsonnet" interests are active.
flakeLib.mkFeature "lang/jsonnet/ls" {
  preconditions = [
    "dev/language-servers"
    "lang/jsonnet"
  ];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.jsonnet-language-server];
  };
}

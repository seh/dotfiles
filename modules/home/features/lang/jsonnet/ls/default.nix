{flakeLib, ...}:
# The Jsonnet language server, active only when the "lang/jsonnet"
# interest and the "dev/language-servers" interest are both active.
flakeLib.mkFeature "lang/jsonnet/ls" {
  preconditions = ["lang/jsonnet" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.jsonnet-language-server];
  };
}

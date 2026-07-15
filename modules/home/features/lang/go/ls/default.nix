{flakeLib, ...}:
# The Go language server, active only when the "lang/go" feature and
# the "dev/language-servers" interest are both active.
flakeLib.mkFeature "lang/go/ls" {
  preconditions = ["lang/go" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.gopls];
  };
}

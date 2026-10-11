{flakeLib, ...}:
# The Go language server, active only when both the
# "dev/language-servers" and "lang/go" interests are active.
flakeLib.mkFeature "lang/go/ls" {
  preconditions = [
    "dev/language-servers"
    "lang/go"
  ];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.gopls];
  };
}

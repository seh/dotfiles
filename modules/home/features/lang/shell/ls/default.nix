{flakeLib, ...}:
# The shell language server, active only when the "lang/shell" feature
# and the "dev/language-servers" interest are both active.
flakeLib.mkFeature "lang/shell/ls" {
  preconditions = ["lang/shell" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.bash-language-server];
  };
}

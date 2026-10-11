{flakeLib, ...}:
# The shell language server, active only when both the
# "dev/language-servers" and "lang/shell" interests are active.
flakeLib.mkFeature "lang/shell/ls" {
  preconditions = [
    "dev/language-servers"
    "lang/shell"
  ];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.bash-language-server];
  };
}

{flakeLib, ...}:
# The Lua language server, active only when the "lang/lua" feature and
# the "dev/language-servers" interest are both active.
flakeLib.mkFeature "lang/lua/ls" {
  preconditions = ["lang/lua" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.emmylua-ls];
  };
}

{flakeLib, ...}:
# The Lua language server, active only when both the
# "dev/language-servers" and "lang/lua" interests are active.
flakeLib.mkFeature "lang/lua/ls" {
  preconditions = [
    "dev/language-servers"
    "lang/lua"
  ];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.emmylua-ls];
  };
}

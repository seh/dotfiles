{flakeLib, ...}:
# The Zig language server, active only when both the
# "dev/language-servers" and "lang/zig" interests are active.
flakeLib.mkFeature "lang/zig/ls" {
  preconditions = [
    "dev/language-servers"
    "lang/zig"
  ];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.zls];
  };
}

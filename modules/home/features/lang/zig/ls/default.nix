{flakeLib, ...}:
# The Zig language server, active only when the "lang/zig" interest and
# the "dev/language-servers" interest are both active.
flakeLib.mkFeature "lang/zig/ls" {
  preconditions = ["lang/zig" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.zls];
  };
}

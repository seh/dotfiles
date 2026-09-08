{flakeLib, ...}:
# The Zig toolchain, active when this configuration expresses the
# "lang/zig" interest: the "zig" compiler and build tool, and the shell
# completions for it, which the Home Manager profile places where the
# "shell/zsh" and "shell/bash" features find them.
flakeLib.mkFeature "lang/zig/tools" {
  preconditions = ["lang/zig"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      zig
      zig-shell-completions
    ];
  };
}

{flakeLib, ...}:
# The shell scripting toolchain, active when this configuration
# expresses the "lang/shell" interest: the "shellcheck" analyzer and
# the "shfmt" formatter, which reads the "[shell]" section of the
# ".editorconfig" file the "dev/editorconfig" feature writes.
flakeLib.mkFeature "lang/shell/tools" {
  preconditions = ["lang/shell"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      shellcheck
      shfmt
    ];
  };
}

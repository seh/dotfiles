{flakeLib, ...}:
# The Starlark language server, active only when the "dev/bazel"
# feature and the "dev/language-servers" interest are both active.
flakeLib.mkFeature "dev/bazel/ls" {
  preconditions = ["dev/bazel" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.starpls];
  };
}

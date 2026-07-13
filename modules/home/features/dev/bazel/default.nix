{flakeLib, ...}:
flakeLib.mkFeature "dev/bazel" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      bazel-buildtools
      bazel_9
      bazelisk
    ];
  };
}

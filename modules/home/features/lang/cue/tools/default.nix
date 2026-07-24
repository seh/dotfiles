{flakeLib, ...}:
# The CUE toolchain, active when this configuration expresses the
# "lang/cue" interest. The "cue" program is the whole of it.
flakeLib.mkFeature "lang/cue/tools" {
  preconditions = ["lang/cue"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      cue
    ];
  };
}

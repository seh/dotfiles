{flakeLib, ...}:
# The Protocol Buffers toolchain, active when this configuration
# expresses the "lang/protobuf" interest. The "buf" program builds and
# checks Protobuf schemas.
flakeLib.mkFeature "lang/protobuf/tools" {
  preconditions = ["lang/protobuf"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      buf
    ];
  };
}

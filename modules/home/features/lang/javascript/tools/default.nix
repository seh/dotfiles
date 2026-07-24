{flakeLib, ...}:
# The JavaScript toolchain, active when this configuration expresses
# the "lang/javascript" interest: the Node.js runtime without its
# documentation, the "prettier" formatter, and the TypeScript
# compiler.
flakeLib.mkFeature "lang/javascript/tools" {
  preconditions = ["lang/javascript"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      nodejs-slim
      prettier
      typescript
    ];
  };
}

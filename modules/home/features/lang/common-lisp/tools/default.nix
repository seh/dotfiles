{flakeLib, ...}:
# The Common Lisp toolchain, active when this configuration expresses
# the "lang/common-lisp" interest. The "sbcl" package is the Steel
# Bank Common Lisp compiler.
flakeLib.mkFeature "lang/common-lisp/tools" {
  preconditions = ["lang/common-lisp"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      sbcl
    ];
  };
}

{flakeLib, ...}:
flakeLib.mkFeature "lang/common-lisp" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      sbcl
    ];
  };
}

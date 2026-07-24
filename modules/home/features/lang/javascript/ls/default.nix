{flakeLib, ...}:
# The TypeScript language server, living with the JavaScript subject
# until a "lang/typescript" interest ever exists; active only when the
# "lang/javascript" interest and the "dev/language-servers" interest
# are both active.
flakeLib.mkFeature "lang/javascript/ls" {
  preconditions = ["lang/javascript" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.typescript-language-server];
  };
}

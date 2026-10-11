{flakeLib, ...}:
# The TypeScript language server, living with the JavaScript subject
# until a "lang/typescript" interest ever exists; active only when
# both the "dev/language-servers" and "lang/javascript" interests are
# active.
flakeLib.mkFeature "lang/javascript/ls" {
  preconditions = [
    "dev/language-servers"
    "lang/javascript"
  ];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.typescript-language-server];
  };
}

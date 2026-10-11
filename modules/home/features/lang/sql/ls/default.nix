{flakeLib, ...}:
# The SQL language server, active only when the "lang/sql" interest and
# the "dev/language-servers" interest are both active.
flakeLib.mkFeature "lang/sql/ls" {
  preconditions = ["lang/sql" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.syntaqlite];
  };
}

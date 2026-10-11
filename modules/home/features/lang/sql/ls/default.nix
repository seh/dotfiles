{flakeLib, ...}:
# The SQL language server, active only when both the
# "dev/language-servers" and "lang/sql" interests are active.
flakeLib.mkFeature "lang/sql/ls" {
  preconditions = [
    "dev/language-servers"
    "lang/sql"
  ];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.syntaqlite];
  };
}

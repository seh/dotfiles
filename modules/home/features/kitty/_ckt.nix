# The "ckt" program, for "choose kitty theme", together with the list
# of theme names that it offers.
#
# The leading underscore in this file's name excludes it from
# "import-tree" in the flake's "flake.nix" file, since this file is a
# plain function rather than a module. The "kitty" feature beside it
# imports it explicitly.
{
  kittyPackage,
  pkgs,
}: let
  themesDir = "${pkgs.kitty-themes}/share/kitty-themes/themes";

  # Collect the theme names when building this derivation, so that the
  # "ckt" program need not read the directory on each run. The
  # "collect-theme-names" program reads the "themesDir" variable
  # supplied here and writes to the path that Nix supplies in the "out"
  # variable.
  themeNames = pkgs.runCommand "kitty-theme-names" {
    inherit themesDir;
  } (builtins.readFile ./collect-theme-names);

  # Fills the picker's preview pane with a page of text in one theme's
  # colors, and needs nothing beyond bash itself to do it.
  renderTheme = pkgs.writeShellApplication {
    name = "render-theme";
    text = builtins.readFile ./render-theme;
  };

  # Puts the marker into the picker's query or takes it out again,
  # which is what one keystroke in the program below asks for.
  toggleQueryMarker = pkgs.writeShellApplication {
    name = "toggle-query-marker";
    text = builtins.readFile ./toggle-query-marker;
  };
in
  pkgs.writeShellApplication {
    name = "ckt";
    runtimeInputs = [
      # Supplies the "basename" and "mkdir" tools.
      pkgs.coreutils
      pkgs.fzf
      # Keeps the record of which themes were chosen when.
      pkgs.sqlite
      # Supplies the "kitten" tool, with which the program controls the
      # running kitty instance.
      kittyPackage
    ];
    # Substituting these paths here leaves the program file free of Nix
    # syntax, so that the "shellcheck" and "shfmt" tools read it as an
    # ordinary bash program. Each substitution preserves its store
    # reference, so the "kitty-themes" package, the list of theme
    # names, the two programs above and the schema all become
    # dependencies of this one.
    text =
      builtins.replaceStrings
      [
        "@themesDir@"
        "@themeNamesFile@"
        "@renderTheme@"
        "@toggleQueryMarker@"
        "@schemaFile@"
      ]
      [
        themesDir
        "${themeNames}"
        "${renderTheme}/bin/render-theme"
        "${toggleQueryMarker}/bin/toggle-query-marker"
        "${./ckt-ddl.sql}"
      ]
      (builtins.readFile ./ckt);
  }

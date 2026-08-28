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

  # Compose each theme's preview page while building this derivation,
  # with its colors already resolved into escape sequences. Doing it
  # here rather than on each cursor movement means the printing program
  # handles no color syntax, and so cannot misread one; kitty itself
  # reads the colors, so every form kitty accepts is drawn correctly.
  themePages = pkgs.runCommand "kitty-theme-pages" {
    inherit themesDir;
    # The same kitty that will apply a chosen theme is the one asked
    # what its colors mean, so the preview cannot disagree with it.
    nativeBuildInputs = [kittyPackage];
    resolveColors = ./resolve-theme-colors.py;
  } (builtins.readFile ./compose-theme-pages);

  # Fills the picker's preview pane with one of the composed pages,
  # padded to the pane's width and set against its foot.
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
        "@themePages@"
        "@toggleQueryMarker@"
        "@schemaFile@"
      ]
      [
        themesDir
        "${themeNames}"
        "${renderTheme}/bin/render-theme"
        "${themePages}"
        "${toggleQueryMarker}/bin/toggle-query-marker"
        "${./ckt-ddl.sql}"
      ]
      (builtins.readFile ./ckt);
  }

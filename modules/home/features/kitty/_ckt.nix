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
in
  pkgs.writeShellApplication {
    name = "ckt";
    runtimeInputs = [
      # Supplies the "basename" tool.
      pkgs.coreutils
      pkgs.fzf
      # Supplies the "kitten" tool, with which the program controls the
      # running kitty instance.
      kittyPackage
    ];
    # Substituting the two paths here leaves the program file free of
    # Nix syntax, so that the "shellcheck" and "shfmt" tools read it as
    # an ordinary bash program. Each substitution preserves its store
    # reference, making both the "kitty-themes" package and the list of
    # theme names dependencies of this program.
    text =
      builtins.replaceStrings
      ["@themesDir@" "@themeNamesFile@" "@renderTheme@"]
      [themesDir "${themeNames}" "${renderTheme}/bin/render-theme"]
      (builtins.readFile ./ckt);
  }

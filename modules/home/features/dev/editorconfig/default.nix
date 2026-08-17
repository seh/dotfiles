{flakeLib, ...}:
flakeLib.mkFeature "dev/editorconfig" {
  homeManager = {lib, ...}: {
    # Build this file with the INI generator directly instead of
    # through Home Manager's "editorconfig" module. That module always
    # serializes section names with the default, bracket-escaping
    # "mkSectionName" and offers no way to override it, so it cannot
    # emit shfmt's non-standard "[[shell]]" header. Work around that
    # by passing "mkSectionName = name: name", which leaves the
    # brackets intact--EditorConfig is INI-like but not strict INI, so
    # literal brackets in a section name are legal.
    home.file.".editorconfig".text = lib.mkDefault (
      lib.generators.toINIWithGlobalSection {mkSectionName = name: name;} {
        globalSection.root = true;
        sections."[shell]" = {
          indent_style = "space";
          indent_size = 2;
        };
      }
    );
  };
}

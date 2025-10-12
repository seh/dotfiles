{ config, lib, ... }:

let
  knownProfiles = [
    "desktop"
    "development"
    "essential"
    "fonts"
    "macos"
    "minimal"
    "web"
  ];

  cfg = config.dotfiles.profiles;
in
{
  options.dotfiles.profiles.selected = lib.mkOption {
    default = [ ];
    description = "Profiles to enable by name.";
    type = lib.types.listOf (lib.types.enum knownProfiles);
  };

  config = {
    dotfiles.profiles = lib.mkMerge (
      map (name: lib.mkIf (lib.elem name cfg.selected) { ${name}.enable = true; }) knownProfiles
    );
  };
}

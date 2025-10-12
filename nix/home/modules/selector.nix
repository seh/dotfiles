{
  config,
  lib,
  ...
}:

let
  cfg = config.installer.modules;
  setOf = elemType: lib.types.addCheck (lib.types.listOf elemType) (list: list == lib.unique list);
  moduleRegistry = import ../module-registry.nix;
  moduleSet = setOf (lib.types.enum moduleRegistry);
in
{
  options.installer.modules = {
    disable = lib.mkOption {
      default = [ ];
      description = "Modules to disable forcibly on this machine.";
      type = moduleSet;
    };

    enable = lib.mkOption {
      default = [ ];
      description = "Modules to enable forcibly on this machine.";
      type = moduleSet;
    };
  };

  config =
    let
      overlap = lib.intersectLists cfg.disable cfg.enable;
      mkModuleConfigs =
        names: enabled:
        map (name: lib.mkIf (lib.elem name names) { ${name}.enable = lib.mkForce enabled; }) moduleRegistry;
    in
    {
      assertions = [
        {
          assertion = lib.length overlap == 0;
          message = "Modules that are both enabled and disabled explicitly: ${lib.concatStringsSep ", " (lib.sort lib.lessThan overlap)}";
        }
      ];

      dotfiles = lib.mkMerge (mkModuleConfigs cfg.disable false ++ mkModuleConfigs cfg.enable true);
    };
}

localFlake:
{
  lib,
  config,
  getSystem,
  ...
}:

let
  inherit (localFlake.inputs.home-manager.lib) homeManagerConfiguration;
  inherit (localFlake.inputs.nix-darwin.lib) darwinSystem;
  inherit (localFlake.inputs.nixos.lib) nixosSystem;

  cfg = config.dotfiles;

  mkHome =
    {
      pkgs,
      overlays ? [ ],
      modules ? [ ],
      ...
    }@args:
    let
      finalPkgs =
        if overlays == [ ] then pkgs else pkgs.extend (lib.composeManyExtensions overlays);
      userDir = if finalPkgs.stdenv.hostPlatform.isDarwin then "/Users" else "/home";
      flakeOptionsModule =
        { lib, config, ... }:
        {
          dotfiles._flakeOptions = cfg;
          home = {
            username = lib.mkDefault cfg.user.name;
            homeDirectory = lib.mkDefault "${userDir}/${config.home.username}";
          };
        };
    in
    homeManagerConfiguration (
      builtins.removeAttrs args [ "overlays" ]
      // {
        pkgs = finalPkgs;
        modules =
          modules
          ++ cfg.home.modules
          ++ [
            localFlake.inputs.self.homeModules.default
            flakeOptionsModule
          ];
      }
    );

  # Basis of inspiration:
  #  https://stackoverflow.com/a/54505212
  #  https://discourse.nixos.org/t/nix-function-to-merge-attributes-records-recursively-and-concatenate-arrays/2030
  recursiveMerge =
    attrList:
    let
      f =
        attrPath:
        builtins.zipAttrsWith (
          n: values:
          if lib.tail values == [ ] then
            lib.head values
          else if lib.all lib.isList values then
            lib.unique (lib.concatLists values)
          else if lib.all lib.isAttrs values then
            f (attrPath ++ [ n ]) values
          else
            lib.last values
        );
    in
    f [ ] attrList;

  importHome =
    configPath: args:
    mkHome (recursiveMerge [
      args
      { modules = [ (import configPath) ]; }
    ]);

  mkDarwin =
    {
      hostPlatform ? "aarch64-darwin",
      pkgs ? pkgsFor hostPlatform,
      overlays ? [ ],
      modules ? [ ],
      ...
    }@args:
    let
      finalPkgs =
        if overlays == [ ] then pkgs else pkgs.extend (lib.composeManyExtensions overlays);
      nixpkgsModule = {
        nixpkgs.pkgs = finalPkgs;
      };
      flakeOptionsModule = _: {
        # Set up the default value for the option proxy.
        dotfiles._flakeOptions = cfg;
        home-manager = {
          useGlobalPkgs = true;
          sharedModules = cfg.home.modules ++ [
            localFlake.inputs.self.homeModules.default
            {
              # Set up the default value for the option proxy.
              dotfiles._flakeOptions = cfg;
            }
          ];
        };
      };
      machineDefaultsModule =
        { config, ... }:
        let
          # Use the final values for the options.
          currentConfig = config.dotfiles._flakeOptions;
          username = currentConfig.user.name;
        in
        {
          nixpkgs.hostPlatform = hostPlatform;
          # See the following GitHub issues for what makes this
          # necessary, perhaps only temporarily:
          #   https://github.com/nix-darwin/nix-darwin/issues/1462
          #   https://github.com/nix-darwin/nix-darwin/issues/1457
          system = {
            primaryUser = lib.mkDefault cfg.user.name;
            stateVersion = lib.mkDefault 6;
          };
          users.users.${username}.home = lib.mkDefault "/Users/${username}";
        };
    in
    darwinSystem (
      builtins.removeAttrs args [
        "hostPlatform"
        "overlays"
        "pkgs"
      ]
      // {
        modules =
          modules
          ++ cfg.darwin.modules
          ++ [
            nixpkgsModule
            localFlake.inputs.self.darwinModules.default
            localFlake.inputs.home-manager.darwinModules.default
            flakeOptionsModule
            machineDefaultsModule
          ];
      }
    );

  importDarwin =
    configPath: args:
    mkDarwin (recursiveMerge [
      args
      { modules = [ (import configPath) ]; }
    ]);

  mkNixOS =
    {
      hostPlatform ? "aarch64-linux",
      pkgs ? pkgsFor hostPlatform,
      overlays ? [ ],
      modules ? [ ],
      ...
    }@args:
    let
      finalPkgs =
        if overlays == [ ] then pkgs else pkgs.extend (lib.composeManyExtensions overlays);
      nixpkgsModule = {
        nixpkgs.pkgs = finalPkgs;
      };
      flakeOptionsModule =
        { config, ... }:
        let
          currentConfig = config.dotfiles._flakeOptions;
          username = currentConfig.user.name;
        in
        {
          dotfiles._flakeOptions = cfg;
          users.users.${username}.home = lib.mkDefault "/home/${username}";
        };
      machineDefaultsModule = {
        nixpkgs.hostPlatform = hostPlatform;
        nix.settings.experimental-features = [
          "nix-command"
          "flakes"
        ];
      };
    in
    nixosSystem (
      builtins.removeAttrs args [
        "hostPlatform"
        "overlays"
        "pkgs"
      ]
      // {
        modules =
          modules
          ++ cfg.nixos.modules
          ++ [
            nixpkgsModule
            localFlake.inputs.self.nixosModules.default
            localFlake.inputs.home-manager.nixosModules.home-manager
            flakeOptionsModule
            machineDefaultsModule
          ];
      }
    );

  importNixOS =
    configPath: args:
    mkNixOS (recursiveMerge [
      args
      { modules = [ (import configPath) ]; }
    ]);

  pkgsFor = system: (getSystem system).allModuleArgs.pkgs;
in
{
  # Allow downstream flakes to define additional things under lib.
  options.flake.lib = lib.mkOption {
    type = with lib.types; lazyAttrsOf raw;
  };

  config.flake.lib = {
    inherit
      importDarwin
      importHome
      importNixOS
      mkDarwin
      mkHome
      mkNixOS
      pkgsFor
      ;
  };
}

{ inputs }@localFlake:
{
  lib,
  config,
  getSystem,
  ...
}@flake:

let
  inherit (localFlake.inputs.home-manager.lib) homeManagerConfiguration;
  inherit (localFlake.inputs.nix-darwin.lib) darwinSystem;

  cfg = config.dotfiles;

  mkHome =
    {
      pkgs,
      modules ? [ ],
      ...
    }@args:
    let
      userDir = if pkgs.stdenv.isDarwin then "/Users" else "/home";
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
      args
      // {
        inherit pkgs;
        modules =
          modules
          ++ cfg.home.modules
          ++ [
            localFlake.inputs.self.homeModules.default
            flakeOptionsModule
          ];
      }
    );

  importHome = configPath: args: mkHome (args // { modules = [ (import configPath) ]; });

  mkDarwin =
    {
      hostPlatform ? "aarch64-darwin",
      modules ? [ ],
      ...
    }@args:
    let
      flakeOptionsModule =
        { lib, ... }:
        {
          dotfiles._flakeOptions = cfg;
          home-manager = {
            useGlobalPkgs = true;
            sharedModules = cfg.home.modules ++ [
              localFlake.inputs.self.homeModules.default
              { dotfiles._flakeOptions = cfg; }
            ];
          };
          nixpkgs.hostPlatform = hostPlatform;
          # See the following GitHub issues for what makes this
          # necessary, perhaps only temporarily:
          #   https://github.com/nix-darwin/nix-darwin/issues/1462
          #   https://github.com/nix-darwin/nix-darwin/issues/1457
          system.primaryUser = lib.mkDefault cfg.user.name;
        };
    in
    darwinSystem (
      builtins.removeAttrs args [ "hostPlatform" ]
      // {
        modules =
          modules
          ++ cfg.darwin.modules
          ++ [
            localFlake.inputs.self.darwinModules.default
            localFlake.inputs.home-manager.darwinModules.default
            flakeOptionsModule
          ];
      }
    );

  importDarwin = configPath: args: mkDarwin (args // { modules = [ (import configPath) ]; });

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
      mkDarwin
      mkHome
      pkgsFor
      ;
  };
}

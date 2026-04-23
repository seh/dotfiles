{
  inputs,
  lib,
  config,
  getSystem,
  ...
}: let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (inputs.nix-darwin.lib) darwinSystem;
  inherit (inputs.nixos.lib) nixosSystem;

  cfg = config.dotfiles;
  dotfilesFlake = config.flake;

  inherit (dotfilesFlake.lib) cascadesForTags expandTagClosure;

  # Split a caller-supplied "host" record into a module that seeds
  # "dotfiles._host" with its fields. The declared "tags" list is
  # first expanded into its transitive closure under the cascade
  # table, so that downstream feature modules see a static,
  # fully-resolved tag set and no fixpoint is required.
  #
  # "profiles" and "features" fields on the host record (added by
  # the typed-activation migration) are forwarded verbatim; their
  # closures are computed inside the host submodule in
  # "modules/_tags.nix".
  mkHostModule = {
    host,
    isDarwin,
  }:
    lib.optional (host != null) {
      dotfiles._host =
        (builtins.removeAttrs host [
          "tags"
          "profiles"
          "features"
        ])
        // {
          tags = expandTagClosure (cascadesForTags {
            framework = host.framework or null;
            inherit isDarwin;
          }) (host.tags or []);
        }
        // lib.optionalAttrs (host ? profiles) {profiles = host.profiles;}
        // lib.optionalAttrs (host ? features) {features = host.features;};
    };

  mkHome = {
    pkgs,
    overlays ? [],
    modules ? [],
    host ? null,
    ...
  } @ args: let
    finalPkgs = pkgs.extend (
      lib.composeManyExtensions ([dotfilesFlake.overlays.nixpkgs] ++ overlays)
    );
    userDir =
      if finalPkgs.stdenv.hostPlatform.isDarwin
      then "/Users"
      else "/home";
    flakeOptionsModule = {
      lib,
      config,
      ...
    }: {
      dotfiles._flakeOptions = cfg;
      home = {
        username = lib.mkDefault cfg.user.name;
        homeDirectory = lib.mkDefault "${userDir}/${config.home.username}";
      };
    };
    hostModule = mkHostModule {
      inherit host;
      inherit (finalPkgs.stdenv.hostPlatform) isDarwin;
    };
  in
    homeManagerConfiguration (
      builtins.removeAttrs args [
        "overlays"
        "host"
      ]
      // {
        pkgs = finalPkgs;
        modules =
          modules
          ++ cfg.home.modules
          ++ [
            dotfilesFlake.homeModules.default
            flakeOptionsModule
          ]
          ++ hostModule;
      }
    );

  # Basis of inspiration:
  #  https://stackoverflow.com/a/54505212
  #  https://discourse.nixos.org/t/nix-function-to-merge-attributes-records-recursively-and-concatenate-arrays/2030
  recursiveMerge = attrList: let
    f = attrPath:
      builtins.zipAttrsWith (
        n: values:
          if lib.tail values == []
          then lib.head values
          else if lib.all lib.isList values
          then lib.unique (lib.concatLists values)
          else if lib.all lib.isAttrs values
          then f (attrPath ++ [n]) values
          else lib.last values
      );
  in
    f [] attrList;

  importHome = configPath: args:
    mkHome (recursiveMerge [
      args
      {modules = [(import configPath)];}
    ]);

  mkDarwin = {
    hostPlatform ? "aarch64-darwin",
    pkgs ? pkgsFor hostPlatform,
    overlays ? [],
    modules ? [],
    host ? null,
    ...
  } @ args: let
    finalPkgs = pkgs.extend (
      lib.composeManyExtensions ([dotfilesFlake.overlays.nixpkgs] ++ overlays)
    );
    nixpkgsModule = {
      nixpkgs.pkgs = finalPkgs;
    };
    hostModule = mkHostModule {
      inherit host;
      isDarwin = true;
    };
    flakeOptionsModule = _: {
      # Set up the default value for the option proxy.
      dotfiles._flakeOptions = cfg;
      home-manager = {
        useGlobalPkgs = true;
        sharedModules =
          cfg.home.modules
          ++ [
            dotfilesFlake.homeModules.default
            {
              # Set up the default value for the option proxy.
              dotfiles._flakeOptions = cfg;
            }
          ]
          ++ hostModule;
      };
    };
    machineDefaultsModule = {config, ...}: let
      # Use the final values for the options.
      currentConfig = config.dotfiles._flakeOptions;
      username = currentConfig.user.name;
    in {
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
        "host"
      ]
      // {
        modules =
          modules
          ++ cfg.darwin.modules
          ++ [
            nixpkgsModule
            dotfilesFlake.darwinModules.default
            inputs.home-manager.darwinModules.default
            flakeOptionsModule
            machineDefaultsModule
          ]
          ++ hostModule;
      }
    );

  importDarwin = configPath: args:
    mkDarwin (recursiveMerge [
      args
      {modules = [(import configPath)];}
    ]);

  mkNixOS = {
    hostPlatform ? "aarch64-linux",
    pkgs ? pkgsFor hostPlatform,
    overlays ? [],
    modules ? [],
    host ? null,
    ...
  } @ args: let
    finalPkgs = pkgs.extend (
      lib.composeManyExtensions ([dotfilesFlake.overlays.nixpkgs] ++ overlays)
    );
    nixpkgsModule = {
      nixpkgs.pkgs = finalPkgs;
    };
    flakeOptionsModule = {config, ...}: let
      currentConfig = config.dotfiles._flakeOptions;
      username = currentConfig.user.name;
    in {
      dotfiles._flakeOptions = cfg;
      users.users.${username}.home = lib.mkDefault "/home/${username}";
    };
    machineDefaultsModule = {
      nixpkgs.hostPlatform = hostPlatform;
    };
    hostModule = mkHostModule {
      inherit host;
      isDarwin = false;
    };
  in
    nixosSystem (
      builtins.removeAttrs args [
        "hostPlatform"
        "overlays"
        "pkgs"
        "host"
      ]
      // {
        modules =
          modules
          ++ cfg.nixos.modules
          ++ [
            nixpkgsModule
            dotfilesFlake.nixosModules.default
            inputs.home-manager.nixosModules.home-manager
            flakeOptionsModule
            machineDefaultsModule
          ]
          ++ hostModule;
      }
    );

  importNixOS = configPath: args:
    mkNixOS (recursiveMerge [
      args
      {modules = [(import configPath)];}
    ]);

  pkgsFor = system: (getSystem system).allModuleArgs.pkgs;
in {
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

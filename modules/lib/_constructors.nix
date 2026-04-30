# Constructors that build Home Manager, nix-darwin, and NixOS
# configurations from this flake's class aggregators. Published as
# "flake.lib.{mkHome,mkDarwin,mkNixOS,importHome,importDarwin,importNixOS,pkgsFor}".
#
# The functions close over this flake's own "self" (captured as
# "inputs.self" at this file's evaluation time), so consumers call
# them as plain library functions:
#
#   inputs.dotfiles.lib.mkDarwin {
#     modules = [./machine.nix];
#     host = { framework = "nixDarwin"; profiles = ["essential"]; };
#   };
#
# without having to install any flake-parts module into the consumer
# evaluator. All identity and behavior assignments (e.g.
# "dotfiles.user.email", "dotfiles.knownProfiles") happen inside the
# modules the consumer passes through "modules = [...]", flowing
# through the target evaluator's module-system merge where those
# fields are actually read. That keeps assignments close to the
# evaluator that reads them and avoids the flake-parts boundary
# crossing that the previous proxy option ("_flakeOptions") used to
# bridge.
{
  lib,
  inputs,
}: let
  dotfilesFlake = inputs.self;

  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (inputs.nix-darwin.lib) darwinSystem;
  inherit (inputs.nixos.lib) nixosSystem;

  nixpkgsDefaults = import ../_nixpkgs-defaults.nix;

  # Build a "pkgs" instance for the given system, with this flake's
  # own nixpkgs overlay and shared "allowUnfreePackages" list
  # applied. This is a fresh instantiation, independent of whatever
  # the consumer's flake-parts "perSystem" may have produced.
  #
  # Road not taken: accept flake-parts' "getSystem" as a function
  # argument and read "(getSystem system).allModuleArgs.pkgs"
  # instead. That would reuse the consumer's already-instantiated
  # nixpkgs (potentially saving eval time and memory) but would tie
  # these constructors back to flake-parts, defeating the point of
  # the relocation. Revisit only if repeated nixpkgs instantiation
  # becomes a measurable cost.
  pkgsFor = system:
    import inputs.nixpkgs (
      {
        inherit system;
      }
      // nixpkgsDefaults
      // {
        overlays = [dotfilesFlake.overlays.nixpkgs];
      }
    );

  # Split a caller-supplied "host" record into a module that seeds
  # "dotfiles._host" with its fields. The declared "profiles" and
  # "features" lists are forwarded verbatim; their closures are
  # computed inside the host submodule in "../_tags.nix".
  mkHostModule = host:
    lib.optional (host != null) {
      dotfiles._host = host;
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
    # Default "home.username" and "home.homeDirectory" from the
    # target evaluator's merged "config.dotfiles.user.name". The
    # consumer can override either by assigning "home.username" /
    # "home.homeDirectory" directly, or by assigning
    # "dotfiles.user.name" inside a module it passes in.
    homeDefaultsModule = {
      lib,
      config,
      ...
    }: {
      home = {
        username = lib.mkDefault config.dotfiles.user.name;
        homeDirectory = lib.mkDefault "${userDir}/${config.home.username}";
      };
    };
    hostModule = mkHostModule host;
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
          ++ [
            dotfilesFlake.modules.homeManager.default
            homeDefaultsModule
          ]
          ++ hostModule;
      }
    );

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
    homeManagerSharedModule = {
      home-manager = {
        useGlobalPkgs = true;
        sharedModules = [dotfilesFlake.modules.homeManager.default] ++ mkHostModule host;
      };
    };
    machineDefaultsModule = {config, ...}: let
      username = config.dotfiles.user.name;
    in {
      nixpkgs.hostPlatform = hostPlatform;
      # See the following GitHub issues for what makes this
      # necessary, perhaps only temporarily:
      #   https://github.com/nix-darwin/nix-darwin/issues/1462
      #   https://github.com/nix-darwin/nix-darwin/issues/1457
      system = {
        primaryUser = lib.mkDefault username;
        stateVersion = lib.mkDefault 6;
      };
      users.users.${username}.home = lib.mkDefault "/Users/${username}";
    };
    hostModule = mkHostModule host;
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
          ++ [
            nixpkgsModule
            dotfilesFlake.modules.darwin.default
            inputs.home-manager.darwinModules.default
            homeManagerSharedModule
            machineDefaultsModule
          ]
          ++ hostModule;
      }
    );

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
    machineDefaultsModule = {config, ...}: let
      username = config.dotfiles.user.name;
    in {
      nixpkgs.hostPlatform = hostPlatform;
      users.users.${username}.home = lib.mkDefault "/home/${username}";
    };
    hostModule = mkHostModule host;
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
          ++ [
            nixpkgsModule
            dotfilesFlake.modules.nixos.default
            inputs.home-manager.nixosModules.home-manager
            machineDefaultsModule
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

  importDarwin = configPath: args:
    mkDarwin (recursiveMerge [
      args
      {modules = [(import configPath)];}
    ]);

  importNixOS = configPath: args:
    mkNixOS (recursiveMerge [
      args
      {modules = [(import configPath)];}
    ]);
in {
  inherit
    importDarwin
    importHome
    importNixOS
    mkDarwin
    mkHome
    mkNixOS
    pkgsFor
    ;
}

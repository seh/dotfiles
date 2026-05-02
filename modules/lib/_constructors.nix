# Constructors that build Home Manager, nix-darwin, and NixOS
# configurations from this flake's class aggregators. Published as
# "flake.lib.{mkHome,mkDarwin,mkNixOS,importHome,importDarwin,importNixOS,pkgsFor}".
#
# The functions close over this flake's own "self" (captured as
# "inputs.self" at this file's evaluation time), so consumers call
# them as plain library functions:
#
#   inputs.dotfiles.lib.mkDarwin {
#     modules = [
#       {
#         dotfiles.host = {
#           framework = "nixDarwin";
#         };
#         dotfiles.users.seh = {
#           identity = {
#             email = "seh@panix.com";
#             fullName = "Steven E. Harris";
#           };
#           profiles = ["essential"];
#         };
#       }
#       ./machine.nix
#     ];
#   };
#
# without having to install any flake-parts module into the consumer
# evaluator. All identity and behavior assignments (e.g.
# "dotfiles.users.<name>.identity.email", "dotfiles.users.<name>.profiles",
# "dotfiles.knownProfiles") happen inside the modules the consumer
# passes through "modules = [...]", flowing through the target
# evaluator's module-system merge where those fields are actually
# read. That keeps assignments close to the evaluator that reads
# them and avoids the flake-parts boundary crossing that the
# previous proxy option ("_flakeOptions") used to bridge.
#
# For the system constructors ("mkDarwin" and "mkNixOS"), each user
# assigned under "dotfiles.users" is mirrored into
# "home-manager.users.<name>.dotfiles" in two places: the user's
# identity fields under "dotfiles.identity", and the system-level
# "dotfiles.host" extended with the user's cascade inputs (profiles,
# features, excludeProfiles, excludeFeatures) under "dotfiles.host".
# The latter shape lets the existing "dotfiles._host" derivation in
# "modules/_tags.nix" continue to read its inputs from one place
# inside the nested home-manager evaluator without modification.
{
  lib,
  inputs,
}: let
  dotfilesFlake = inputs.self;

  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (inputs.nix-darwin.lib) darwinSystem;
  inherit (inputs.nixos.lib) nixosSystem;

  inherit (import ./_cascades.nix {inherit lib;}) cascadesFor expandClosure;

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

  # System-level module that, for each user assigned under
  # "config.dotfiles.users", mirrors the user's identity and the
  # combined host record into that user's nested home-manager
  # evaluator, and assigns the user's home directory.
  #
  # Also computes the system-level cascade as the union of each
  # user's resolved active sets. NixOS profile/feature modules
  # contribute system-level configuration (for example, an "essential"
  # profile that sets "programs.zsh.enable = true;" so that
  # "/etc/shells" registers zsh as a legitimate login shell). A
  # profile that any user activates must therefore reach the
  # system-level evaluator. The cascade walk runs once per user,
  # using that user's seed and exclusions; the deduplicated union
  # of those active sets is assigned to the system-level
  # "dotfiles.host.{profiles,features}". Per-user exclusions are
  # already applied before unioning, so the system-level
  # "excludeProfiles" / "excludeFeatures" are left empty.
  #
  # The nested home-manager evaluator sees:
  #   dotfiles.identity = <user>.identity
  #   dotfiles.host = (system-level) config.dotfiles.host
  #                 // <user>'s cascade inputs
  # so the existing "dotfiles._host" derivation logic (which reads
  # "config.dotfiles.host.{profiles,features,excludeProfiles,excludeFeatures}"
  # from inside the home-manager evaluator) works unchanged.
  multiUserPropagationModule = userDir: {config, ...}: let
    inherit (config.dotfiles) host;
    cascades = cascadesFor {
      inherit (host) framework;
      isDarwin = host.framework == "nixDarwin";
      knownProfiles = config.dotfiles._knownProfiles;
    };
    knownByRole = {
      profiles = config.dotfiles._knownProfiles;
      features = config.dotfiles._knownFeatures;
    };
    # Per-user effective active sets: expand each user's seed
    # through the cascade, then subtract that user's exclusions.
    activePerUser =
      lib.mapAttrs (
        _: userCfg: let
          expanded = expandClosure cascades knownByRole {
            inherit (userCfg) profiles features;
          };
        in {
          profiles = lib.subtractLists userCfg.excludeProfiles expanded.profiles;
          features = lib.subtractLists userCfg.excludeFeatures expanded.features;
        }
      )
      config.dotfiles.users;
    # Union of every user's active profiles / features. Stable
    # order via "lib.unique" applied after concatenation.
    unionActive = {
      profiles = lib.unique (lib.concatMap (a: a.profiles) (lib.attrValues activePerUser));
      features = lib.unique (lib.concatMap (a: a.features) (lib.attrValues activePerUser));
    };
  in {
    dotfiles.host = {
      profiles = lib.mkDefault unionActive.profiles;
      features = lib.mkDefault unionActive.features;
    };

    home-manager.users =
      lib.mapAttrs (_: userCfg: {
        imports = [userCfg.homeManagerConfig];
        dotfiles = {
          inherit (userCfg) identity;
          host =
            host
            // {
              inherit
                (userCfg)
                profiles
                excludeProfiles
                features
                excludeFeatures
                ;
            };
        };
      })
      config.dotfiles.users;

    users.users =
      lib.mapAttrs (userName: _: {
        home = lib.mkDefault "${userDir}/${userName}";
      })
      config.dotfiles.users;
  };

  mkHome = {
    pkgs,
    overlays ? [],
    modules ? [],
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
    # target evaluator's merged "config.dotfiles.identity.name". The
    # consumer can override either by assigning "home.username" /
    # "home.homeDirectory" directly, or by assigning
    # "dotfiles.identity.name" inside a module it passes in.
    #
    # Default "programs.home-manager.enable" to true so that the
    # "home-manager" CLI is available in the activated user
    # profile. Consumers managing user profiles centrally (for
    # example, an administrator who deploys home-manager
    # configurations on behalf of users) can override this to
    # false in a module they pass in.
    homeDefaultsModule = {
      lib,
      config,
      ...
    }: {
      home = {
        username = lib.mkDefault config.dotfiles.identity.name;
        homeDirectory = lib.mkDefault "${userDir}/${config.home.username}";
      };
      programs.home-manager.enable = lib.mkDefault true;
    };
  in
    homeManagerConfiguration (
      builtins.removeAttrs args [
        "overlays"
      ]
      // {
        pkgs = finalPkgs;
        modules =
          modules
          ++ [
            dotfilesFlake.modules.homeManager.default
            homeDefaultsModule
          ];
      }
    );

  mkDarwin = {
    hostPlatform ? "aarch64-darwin",
    pkgs ? pkgsFor hostPlatform,
    overlays ? [],
    modules ? [],
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
        sharedModules = [dotfilesFlake.modules.homeManager.default];
      };
    };
    machineDefaultsModule = {config, ...}: {
      nixpkgs.hostPlatform = hostPlatform;
      # See the following GitHub issues for what makes this
      # necessary, perhaps only temporarily:
      #   https://github.com/nix-darwin/nix-darwin/issues/1462
      #   https://github.com/nix-darwin/nix-darwin/issues/1457
      system = {
        primaryUser = lib.mkDefault config.dotfiles.primaryUser;
        stateVersion = lib.mkDefault 6;
      };
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
          ++ [
            nixpkgsModule
            dotfilesFlake.modules.darwin.default
            inputs.home-manager.darwinModules.default
            homeManagerSharedModule
            machineDefaultsModule
            (multiUserPropagationModule "/Users")
          ];
      }
    );

  mkNixOS = {
    hostPlatform ? "aarch64-linux",
    pkgs ? pkgsFor hostPlatform,
    overlays ? [],
    modules ? [],
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
        useUserPackages = true;
        sharedModules = [dotfilesFlake.modules.homeManager.default];
      };
    };
    machineDefaultsModule = {
      nixpkgs.hostPlatform = hostPlatform;
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
          ++ [
            nixpkgsModule
            dotfilesFlake.modules.nixos.default
            inputs.home-manager.nixosModules.home-manager
            homeManagerSharedModule
            machineDefaultsModule
            (multiUserPropagationModule "/home")
          ];
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

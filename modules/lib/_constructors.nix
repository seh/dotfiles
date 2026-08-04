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
#         dotfiles.users.seh = {
#           identity = {
#             email = "seh@panix.com";
#             fullName = "Steven E. Harris";
#           };
#           features = ["essential"];
#         };
#       }
#       ./machine.nix
#     ];
#   };
#
# without having to install any flake-parts module into the consumer
# evaluator. All identity and behavior assignments (e.g.
# "dotfiles.users.<name>.identity.email",
# "dotfiles.users.<name>.features", "dotfiles.knownFeatures") happen
# inside the modules the consumer passes through "modules = [...]",
# flowing through the target evaluator's module-system merge where
# those fields are actually read. That keeps assignments close to the
# evaluator that reads them and avoids the flake-parts boundary
# crossing that the retired "_flakeOptions" option used to bridge.
#
# For the system constructors ("mkDarwin" and "mkNixOS"), each user
# assigned under "dotfiles.users" is mirrored into
# "home-manager.users.<name>.dotfiles" in two places: the user's
# identity fields under "dotfiles.identity", and the machine's
# "dotfiles.host" record with its selections and exclusions layered
# with that user's own under "dotfiles.host". The mirroring assigns
# nothing into "dotfiles.host.{features,interests}" at the system
# level: those stay the machine's own selections, which alone decide
# the machine's own configuration.
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
  # Pass "applyOverlays = false" to omit that overlay.
  #
  # Road not taken: accept flake-parts' "getSystem" as a function
  # argument and read "(getSystem system).allModuleArgs.pkgs"
  # instead. That would reuse the consumer's already-instantiated
  # nixpkgs (potentially saving eval time and memory) but would tie
  # these constructors back to flake-parts, defeating the point of
  # the relocation. Revisit only if repeated nixpkgs instantiation
  # becomes a measurable cost.
  pkgsFor = {
    system,
    applyOverlays ? true,
  }:
    import inputs.nixpkgs (
      {
        inherit system;
      }
      // nixpkgsDefaults
      // lib.optionalAttrs applyOverlays {
        overlays = [dotfilesFlake.overlays.nixpkgs];
      }
    );

  # System-level module that, for each user assigned under
  # "config.dotfiles.users", creates the user's operating-system
  # account, spawns the user's nested home-manager evaluator, and
  # mirrors the user's identity and a layered host record into that
  # evaluator. It assigns nothing into
  # "dotfiles.host.{features,interests}" at the system level: those
  # stay the machine's own selections, which alone decide the
  # machine's own configuration.
  #
  # The nested home-manager evaluator sees:
  #   dotfiles.identity = <user>.identity
  #   dotfiles.host = the machine's record, its selections and
  #                   exclusions layered with that user's own
  # so the "dotfiles._host" record in "modules/_activation.nix"
  # resolves one coherent activation for that user from the machine's
  # selections and the user's together: the machine provisions every
  # user it manages, and each user adds to that.
  #
  # Layering lets an exclusion hold in three ways, applied alike to
  # the machine's features and interests:
  #   1. The machine's "forbidFeatures" and "forbidInterests" pass
  #      through untouched—the "//" below never names them—and prune
  #      every walk, so a user naming a forbidden thing still does not
  #      receive it.
  #   2. The machine's "excludeFeatures" and "excludeInterests"
  #      withhold a name from what it provisions, yet a user who asks
  #      for that same name—selecting it directly, or selecting a
  #      bundle that brings it along—drops it from the exclusions in
  #      force for that user, opting back in.
  #   3. A user's own exclusions always hold, for that user alone.
  multiUserPropagationModule = userDir: {config, ...}: let
    inherit (config.dotfiles) host;
    flakeLib = config.dotfiles._flakeLib;
    hasImplicationsLib =
      flakeLib != null && flakeLib ? implicationsFor && flakeLib ? resolveActivation;
    implications =
      if hasImplicationsLib
      then
        flakeLib.implicationsFor {
          platform = config.dotfiles._host.platform;
          preconditions = config.dotfiles._featurePreconditions;
          supportedPlatforms = config.dotfiles._supportedPlatforms;
          impliedEdges = config.dotfiles._impliedEdges;
          knownFeatures = config.dotfiles._knownFeatures;
        }
      else null;
    # Everything one user's own selections entail: those selections
    # expanded along the implication graph, after that user's own
    # exclusions and everything the machine forbids prune it. The
    # machine's exclusions yield to a user who asks for a name, and
    # selecting a bundle asks for everything it brings along, so the
    # layering below subtracts this closure rather than the bare lists
    # the user wrote: a bundle then opts back in exactly as selecting
    # each of its members directly would. The preconditions table
    # stays empty on purpose: a contingent feature cannot be selected,
    # so no selection asks for one, and an exclusion of one never
    # yields.
    entailedBy = userCfg:
      if hasImplicationsLib
      then
        flakeLib.resolveActivation {
          inherit implications;
          known = config.dotfiles._knownNames;
          platform = config.dotfiles._host.platform;
          preconditions = {};
          supportedPlatforms = config.dotfiles._supportedPlatforms;
          selected = userCfg.features ++ userCfg.interests;
          excluded =
            userCfg.excludeFeatures
            ++ userCfg.excludeInterests
            ++ host.forbidFeatures
            ++ host.forbidInterests;
        }
      else userCfg.features ++ userCfg.interests;
  in {
    home-manager.users =
      lib.mapAttrs (_: userCfg: let
        entailed = entailedBy userCfg;
      in {
        imports = [userCfg.homeManagerConfig];
        dotfiles = {
          inherit (userCfg) identity;
          # What the machine forbids, written where no module of this
          # user's can displace it. The "host" record below carries the
          # same two lists, but a user's own module may rewrite that
          # record, so the walk reads both and these two hold.
          _machineForbidFeatures = host.forbidFeatures;
          _machineForbidInterests = host.forbidInterests;
          host =
            host
            // {
              features = host.features ++ userCfg.features;
              interests = host.interests ++ userCfg.interests;
              excludeFeatures =
                userCfg.excludeFeatures
                ++ lib.subtractLists entailed host.excludeFeatures;
              excludeInterests =
                userCfg.excludeInterests
                ++ lib.subtractLists entailed host.excludeInterests;
            };
        };
      })
      config.dotfiles.users;

    # What the machine forbids that a user's own evaluator no longer
    # prunes. Only a module inside that user's home configuration can
    # produce this, by defining "dotfiles.host" or the propagated
    # lists again at a priority displacing what this module wrote.
    # The test sits here, in the machine's own evaluator, because
    # nothing a user writes reaches this far: a check inside the
    # user's evaluator would compare two values that same user can
    # rewrite together.
    # Only the users this flake provisions are tested. A home-manager
    # user some other module declares receives none of the propagated
    # lists, so its empty copies would read as every forbidden name
    # gone missing, and the complaint would accuse a person whose
    # configuration this flake never wrote.
    assertions =
      lib.mapAttrsToList (
        userName: _: let
          evaluated = config.home-manager.users.${userName}.dotfiles;
          lost =
            lib.subtractLists
            (evaluated.host.forbidFeatures ++ evaluated._machineForbidFeatures)
            host.forbidFeatures
            ++ lib.subtractLists
            (evaluated.host.forbidInterests ++ evaluated._machineForbidInterests)
            host.forbidInterests;
        in {
          assertion = lost == [];
          message = let
            label =
              if host.name == null
              then "an unnamed host"
              else ''host "${host.name}"'';
            names = lib.concatMapStringsSep ", " (n: ''"${n}"'') lost;
          in ''
            Resolving ${label}: the configuration of the user "${userName}" removed ${names} from what this machine forbids. The "dotfiles.host.forbidFeatures" and "dotfiles.host.forbidInterests" lists are the machine's, and no user may set them aside. Remove whatever line in that user's own configuration writes them. To decline a name the machine merely excludes, select it instead.
          '';
        }
      )
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
    pkgs ?
      pkgsFor {
        system = hostPlatform;
        applyOverlays = false;
      },
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
      # See the following GitHub issues for what makes this assignment
      # necessary, perhaps only temporarily:
      #   https://github.com/nix-darwin/nix-darwin/issues/1462
      #   https://github.com/nix-darwin/nix-darwin/issues/1457
      system.primaryUser = lib.mkDefault config.dotfiles.primaryUser;
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
    pkgs ?
      pkgsFor {
        system = hostPlatform;
        applyOverlays = false;
      },
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

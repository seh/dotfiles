# Constructors that build Home Manager, nix-darwin, and NixOS
# configurations from this flake's class aggregators. Published as the
# "flake.lib.{mkHome,mkDarwin,mkNixOS,importHome,importDarwin,importNixOS,pkgsFor}"
# functions.
#
# The functions close over this flake's own "self" record (captured
# from the "inputs.self" attribute at this file's evaluation time), so
# consumers call them as plain library functions:
#
#   inputs.dotfiles.lib.mkDarwin {
#     hostPlatform = "aarch64-darwin";
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
# evaluator. All identity and behavior assignments (e.g. the
# "dotfiles.users.<name>.identity.email",
# "dotfiles.users.<name>.features", and "dotfiles.knownFeatures"
# options) happen inside the modules the consumer passes through the
# "modules = [...]" argument, flowing through the target evaluator's
# module-system merge where those fields are actually read. That keeps
# assignments close to the evaluator that reads them, with no
# flake-parts boundary to cross.
#
# For the system constructors ("mkDarwin" and "mkNixOS"), each user
# assigned under the "dotfiles.users" registry is mirrored into the
# "home-manager.users.<name>.dotfiles" option path: the user's
# identity fields under the "dotfiles.identity" record, the machine's
# "dotfiles.host" record with its selections and exclusions layered
# with that user's own, and that user's own four lists kept apart from
# the layered ones, so that a diagnostic can tell what the user wrote
# from what the machine passed down. The mirroring assigns nothing
# into the "dotfiles.host.{features,interests}" lists at the system
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
  inherit (import ./_diagnostics.nix) describeHost;

  # Build a "pkgs" instance for the given system, with this flake's
  # own nixpkgs overlay applied and its features' unfree packages
  # tolerated. That toleration set is the union this flake publishes
  # as the "allowUnfreePackages" output (see the
  # "../nixpkgs-config.nix" file), so the package set built here
  # tolerates exactly what the flake-parts evaluator's own does. This
  # is a fresh instantiation, independent of whatever the consumer's
  # flake-parts "perSystem" option may have produced.
  #
  # Pass the "applyOverlays = false" argument to omit that overlay.
  #
  # Road not taken: accept flake-parts' "getSystem" as a function
  # argument and read "(getSystem system).allModuleArgs.pkgs" instead.
  # That would reuse the consumer's already-instantiated nixpkgs
  # (potentially saving eval time and memory) but would tie these
  # constructors back to flake-parts, defeating the point of the
  # relocation. Revisit only if repeated nixpkgs instantiation becomes
  # a measurable cost.
  pkgsFor = {
    system,
    applyOverlays ? true,
  }:
    import inputs.nixpkgs (
      {
        inherit system;
        config.allowUnfreePackages = dotfilesFlake.allowUnfreePackages;
      }
      // lib.optionalAttrs applyOverlays {
        overlays = [dotfilesFlake.overlays.nixpkgs];
      }
    );

  # Format a list of names as a quoted, comma-separated English
  # enumeration with a serial comma, matching the "enumerateNames"
  # helper in the "modules/lib/_features.nix" file: one name renders
  # as "a", two as "a" and "b", and three or more as "a", "b", and
  # "c".
  enumerateNames = names: let
    quoted = map (n: "\"${n}\"") names;
    count = lib.length quoted;
  in
    if count == 1
    then lib.head quoted
    else if count == 2
    then "${lib.head quoted} and ${lib.last quoted}"
    else "${lib.concatStringsSep ", " (lib.init quoted)}, and ${lib.last quoted}";

  # The machine-wide forbid options, read together so that one walk
  # over a user's selections covers both kinds.
  forbidOptions = ["forbidFeatures" "forbidInterests"];

  # The selections a user writes that "forbidFeatures" or
  # "forbidInterests" overrules, one entry per pair of a selected name
  # and whichever option forbids it. Both of the user's own lists are
  # read together, since a name written under either enters the same
  # activation walk.
  # What forbidding costs one user. Both warnings below draw on this:
  # a user who writes a forbidden name and a user whose bundle entails
  # one lose the same names, so they deserve the same account of the
  # loss.
  forbidCost = {
    closureOf,
    featureClasses,
    host,
    userCfg,
  }: let
    forbidden = lib.concatMap (option: host.${option}) forbidOptions;
    ownExclusions = userCfg.excludeFeatures ++ userCfg.excludeInterests;
    closureFrom = roots: closureOf roots ownExclusions;
    written = lib.unique (userCfg.features ++ userCfg.interests);
    entailed = closureFrom written;
    # Everything forbidding costs: the walk without it, less the walk
    # with it.
    withheld = lib.subtractLists (closureOf written (ownExclusions ++ forbidden)) entailed;
    # A feature whose body serves a system class alone never reaches a
    # user's home environment, so naming it as this user's loss would
    # name something they were never going to receive. The
    # "userSystemOnlyFeatureAssertion" assertion refuses a user who
    # selects one directly, for that same reason.
    reachesUser = name: let
      classes = featureClasses.${name} or [];
    in
      classes == [] || builtins.elem "homeManager" classes;
  in {
    inherit closureFrom entailed written;
    # The withheld names lying beyond one forbidden name: what goes
    # with it. A name the machine forbids outright draws its own
    # warning and stays out, which also keeps the closing advice
    # true—every name left arrives once nothing forbidden lies on the
    # way to it. A name leaves the walk only when each way to it
    # crosses a forbidden vertex, so every withheld name lies beyond
    # at least one of them and some warning names it.
    beyond = name:
      builtins.filter (
        n:
          n
          != name
          && builtins.elem n withheld
          && !(builtins.elem n forbidden)
          && reachesUser n
      )
      (closureFrom [name]);
  };

  overruledSelections = cost: host:
    lib.concatMap (
      option:
        map (name: {
          inherit name option;
          alsoWithheld = cost.beyond name;
        })
        (builtins.filter (n: builtins.elem n host.${option}) cost.written)
    )
    forbidOptions;

  # Report one overruled selection to the user who wrote it. The
  # forbid entry is absolute and the machine's owner is entitled to
  # it, so the configuration stands and this is a warning rather than
  # an error.
  # The clause naming what a forbidden name takes with it, empty when
  # it takes nothing. Both messages carry it, since both report the
  # same loss.
  describeCollateral = alsoWithheld:
    if alsoWithheld == []
    then ""
    else " Withholding it withholds ${enumerateNames alsoWithheld} as well, which this user's selections bring along only by way of a forbidden name, and each of those arrives once nothing forbidden lies on the way to it.";

  describeOverruledSelection = hostName: userName: {
    alsoWithheld,
    name,
    option,
  }: ''
    Resolving ${describeHost hostName}: the user "${userName}" selects "${name}", which "dotfiles.host.${option}" forbids machine-wide, so this user does not receive it.${describeCollateral alsoWithheld} Forbidding prunes every activation walk, the machine's own and every user's, and no user may undo it; drop the entry from "dotfiles.host.${option}" to let this selection stand, or drop the selection.
  '';

  # The names a user's selections entail that the machine forbids,
  # though the user wrote none of them: the closure of the user's
  # selections computed by the "closureOf" function with only that
  # user's own exclusions pruning, filtered to what it forbids. A name
  # the user wrote directly is left out, since the
  # "overruledSelections" function above already covers it and one
  # withheld name deserves one warning; a name the user's own
  # exclusions prune never enters the closure, so a user who declines
  # such a name hears nothing. Each entry lists the written selections
  # whose own closures hold the name, for the warning to cite.
  overruledEntailments = cost: host:
    lib.concatMap (
      option:
        map (name: {
          inherit name option;
          alsoWithheld = cost.beyond name;
          through =
            builtins.filter
            (root: builtins.elem name (cost.closureFrom [root]))
            cost.written;
        })
        (builtins.filter (
            name: builtins.elem name host.${option} && !(builtins.elem name cost.written)
          )
          cost.entailed)
    )
    forbidOptions;

  # Report a withheld entailed name to the user whose selections
  # entail it. The user did not write the name, so the message
  # identifies the written selections that entail it instead—the name
  # is reachable from at least one of them by construction, so the
  # "through" list is never empty—and says that the rest of what those
  # selections entail still arrives.
  describeOverruledEntailment = hostName: userName: {
    alsoWithheld,
    name,
    option,
    through,
  }: let
    one = lib.length through == 1;
    entailment =
      if one
      then "selects ${enumerateNames through}, and that selection entails"
      else "selects ${enumerateNames through}, and those selections entail";
  in ''
    Resolving ${describeHost hostName}: the user "${userName}" ${entailment} "${name}", which "dotfiles.host.${option}" forbids machine-wide, so this user does not receive it.${describeCollateral alsoWithheld} Forbidding prunes every activation walk, the machine's own and every user's, and no user may undo it; drop the entry from "dotfiles.host.${option}" to let the name activate here.
  '';

  # System-level module that, for each user assigned under the
  # "config.dotfiles.users" registry, gives the user's
  # operating-system account its home directory, spawns the user's
  # nested home-manager evaluator, and mirrors the user's identity, a
  # layered host record, that user's own four lists, and the option
  # path holding those four lists into that evaluator. It serves both
  # system classes, so it assigns only what nix-darwin and NixOS
  # share; what NixOS alone insists upon lives in the
  # "modules/_nixos-user-accounts.nix" file, which the NixOS class
  # aggregator imports by itself. It assigns nothing into the
  # "dotfiles.host.{features,interests}" lists at the system level:
  # those stay the machine's own selections, which alone decide the
  # machine's own configuration.
  #
  # The nested home-manager evaluator sees:
  #   dotfiles.identity = <user>.identity
  #   dotfiles._ownFeatures = <user>.features
  #   dotfiles._ownInterests = <user>.interests
  #   dotfiles._ownExcludeFeatures = <user>.excludeFeatures
  #   dotfiles._ownExcludeInterests = <user>.excludeInterests
  #   dotfiles._ownListPrefix = the option path holding those four
  #   dotfiles.host = the machine's record, its selections and
  #                   exclusions layered with that user's own
  # so the "dotfiles._host" record in the "modules/_activation.nix"
  # file resolves one coherent activation for that user from the
  # machine's selections and the user's together: the machine
  # provisions every user it manages, and each user adds to that.
  #
  # Layering lets an exclusion hold in three ways, applied alike to
  # the machine's features and interests:
  #   1. The machine's "forbidFeatures" and "forbidInterests" lists
  #      pass through untouched—the "//" operator below leaves them
  #      alone—and prune every walk, so a user who asks for a
  #      forbidden thing—selecting it directly, or selecting a bundle
  #      that entails it—still does not receive it. That user's own
  #      evaluator emits a warning saying so, since forbidding is the
  #      one place where this flake sets aside what a person wrote.
  #   2. The machine's "excludeFeatures" and "excludeInterests" lists
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
          featureClasses = config.dotfiles._featureClasses;
        }
      else null;
    # The closure of the given selections along the implication graph,
    # with the given exclusions pruned as vertices, so a name
    # reachable only through a pruned one stays out. Without the
    # implications library the selections stand unexpanded. The
    # preconditions table stays empty on purpose: a contingent feature
    # cannot be selected, so no selection asks for one, and an
    # exclusion of one never yields.
    selectionClosure = selected: excluded:
      if hasImplicationsLib
      then
        flakeLib.resolveActivation {
          inherit implications selected excluded;
          known = config.dotfiles._knownNames;
          platform = config.dotfiles._host.platform;
          preconditions = {};
          supportedPlatforms = config.dotfiles._supportedPlatforms;
        }
      else selected;
    # Everything one user's own selections entail: those selections
    # expanded along the implication graph, after that user's own
    # exclusions and everything the machine forbids prune it. The
    # machine's exclusions yield to a user who asks for a name, and
    # selecting a bundle asks for everything it brings along, so the
    # layering below subtracts this closure rather than the bare lists
    # the user wrote: a bundle then opts back in exactly as selecting
    # each of its members directly would.
    entailedBy = userCfg:
      selectionClosure (userCfg.features ++ userCfg.interests) (
        userCfg.excludeFeatures
        ++ userCfg.excludeInterests
        ++ host.forbidFeatures
        ++ host.forbidInterests
      );
  in {
    home-manager.users =
      lib.mapAttrs (userName: userCfg: let
        entailed = entailedBy userCfg;
      in {
        imports = [userCfg.homeManagerConfig];
        dotfiles = {
          inherit (userCfg) identity;
          # What the machine forbids, written where no module of this
          # user's can displace it. The "host" record below holds the
          # same two lists, but a user's own module may rewrite that
          # record, so the walk reads both and these two hold.
          _machineForbidFeatures = host.forbidFeatures;
          _machineForbidInterests = host.forbidInterests;
          # This user's own four lists, recorded beside the layered
          # ones below so that a diagnostic in the
          # "modules/_activation.nix" file can tell what this user
          # wrote from what the machine passed down. A user
          # countermands the machine's exclusion by selecting the
          # name; their own exclusion always holds. The path lets a
          # message cite the line its reader would edit, and comes
          # from here because a user may set an identity name
          # differing from the attribute key their lists live under.
          _ownFeatures = userCfg.features;
          _ownInterests = userCfg.interests;
          _ownExcludeFeatures = userCfg.excludeFeatures;
          _ownExcludeInterests = userCfg.excludeInterests;
          _ownListPrefix = ''dotfiles.users."${userName}"'';
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
        warnings = let
          cost = forbidCost {
            inherit host userCfg;
            closureOf = selectionClosure;
            featureClasses = config.dotfiles._featureClasses;
          };
        in
          map (describeOverruledSelection host.name userName)
          (overruledSelections cost host)
          ++ map (describeOverruledEntailment host.name userName)
          (overruledEntailments cost host);
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

  # Argument validation shared by the three constructors below,
  # holding each to its contract in this flake's vocabulary. Each
  # constructor forwards every argument it does not consume itself, so
  # an unrecognized name passes through to home-manager, nix-darwin,
  # or NixOS, where the complaint speaks that evaluator's vocabulary
  # and specifies neither the constructor called nor what it would
  # have accepted. A required name is held here too, rather than by
  # the constructor's own function pattern, whose complaint specifies
  # an anonymous lambda.
  #
  # The check wraps the configuration a constructor returns, so
  # forcing that configuration raises the complaint first and every
  # binding derived from an argument under complaint stays unread.
  #
  # The "accepted" argument lists every name the constructor takes:
  # those it consumes itself, plus those its builder accepts, read
  # from the builder's own argument pattern, less any the constructor
  # withholds for a reason its own comment gives. The "required"
  # argument lists the names the constructor has no basis to default.
  #
  # A supplied "pkgs" argument must build for the platform that the
  # "hostPlatform" argument specifies. This is a throw rather than a
  # module assertion because nothing forces the assertions on the way
  # to an answer: the activation walk in the "modules/_activation.nix"
  # file reads the platform from the package set, so a configuration
  # whose two platforms disagree resolves its features from the
  # package set's platform and answers every question put to it while
  # the assertions go unread. Only a supplied "pkgs" argument can
  # disagree, since the package set a constructor instantiates itself
  # comes from the "hostPlatform" argument.
  checkArguments = {
    accepted,
    args,
    constructor,
    hostPlatform ? null,
    required ? [],
  }: configuration: let
    missing = builtins.filter (name: !(args ? ${name})) required;
    unknown = builtins.attrNames (builtins.removeAttrs args accepted);
    suppliedPlatform = args.pkgs.stdenv.hostPlatform.system;
  in
    if missing != []
    then throw ''${constructor}: ${
        if lib.length missing == 1
        then "this call omits the required argument ${enumerateNames missing}, which has no default"
        else "this call omits the required arguments ${enumerateNames missing}, which have no defaults"
      }. The accepted arguments are ${enumerateNames accepted}.''
    else if unknown != []
    then throw ''${constructor}: ${
        if lib.length unknown == 1
        then "this constructor does not accept the argument ${enumerateNames unknown}"
        else "this constructor does not accept the arguments ${enumerateNames unknown}"
      }. The accepted arguments are ${enumerateNames accepted}.''
    else if hostPlatform != null && args ? pkgs && suppliedPlatform != hostPlatform
    then throw ''${constructor}: "hostPlatform" specifies the platform "${hostPlatform}" while the supplied "pkgs" builds for "${suppliedPlatform}". The two must agree, since the configuration declares the first while drawing its packages and resolving its features from the second. Either omit "pkgs" and let this constructor instantiate a package set for "${hostPlatform}", or supply one built for "${hostPlatform}".''
    else configuration;

  # The arguments the "mkHome" constructor accepts: "modules",
  # "overlays", and "pkgs", which it consumes itself, together with
  # the rest of what the "homeManagerConfiguration" function accepts,
  # per the argument pattern in home-manager's "lib/eval-config.nix"
  # file.
  homeArguments = [
    "check"
    "extraSpecialArgs"
    "lib"
    "minimal"
    "modules"
    "overlays"
    "pkgs"
  ];

  mkHome = {
    overlays ? [],
    modules ? [],
    ...
  } @ args: let
    inherit (args) pkgs;
    finalPkgs = pkgs.extend (
      lib.composeManyExtensions ([dotfilesFlake.overlays.nixpkgs] ++ overlays)
    );
    userDir =
      if finalPkgs.stdenv.hostPlatform.isDarwin
      then "/Users"
      else "/home";
    # Default the "home.username" and "home.homeDirectory" options
    # from the target evaluator's merged
    # "config.dotfiles.identity.name" option. The consumer can
    # override either by assigning the "home.username" /
    # "home.homeDirectory" options directly, or by assigning the
    # "dotfiles.identity.name" option inside a module it passes in.
    #
    # Default the "programs.home-manager.enable" option to true so
    # that the "home-manager" CLI is available in the activated user
    # profile. Consumers managing user profiles centrally (for
    # example, an administrator who deploys home-manager
    # configurations on behalf of users) can override this to false in
    # a module they pass in.
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
    checkArguments {
      constructor = "mkHome";
      accepted = homeArguments;
      required = ["pkgs"];
      inherit args;
    } (
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
      )
    );

  # The arguments the "mkDarwin" constructor accepts: "hostPlatform",
  # "overlays", and "pkgs", which it consumes itself, together with
  # the rest of what the "darwinSystem" function accepts—the "inputs"
  # and "pkgs" arguments, which it handles itself, per its definition
  # in nix-darwin's "flake.nix" file, plus everything it forwards to
  # the "evalConfig" function, per the argument pattern in
  # nix-darwin's "eval-config.nix" file.
  #
  # It does not accept the "system" argument, which the "darwinSystem"
  # function takes as an older way to specify the platform, turning it
  # into the "nixpkgs.system" option. The nixpkgs module ignores that
  # option once the "nixpkgs.hostPlatform" or "nixpkgs.pkgs" option is
  # set, and this constructor sets both, so a "system" argument that
  # nominated another platform would pass the agreement check
  # above—which reads only the "pkgs" argument—and stand in the
  # configuration as a third opinion nothing builds for. The
  # "hostPlatform" argument is the only way to specify the platform.
  darwinArguments = [
    "baseModules"
    "check"
    "enableNixpkgsReleaseCheck"
    "hostPlatform"
    "inputs"
    "lib"
    "modules"
    "overlays"
    "pkgs"
    "specialArgs"
  ];

  mkDarwin = {
    overlays ? [],
    modules ? [],
    ...
  } @ args: let
    inherit (args) hostPlatform;
    pkgs =
      args.pkgs
      or (pkgsFor {
        system = hostPlatform;
        applyOverlays = false;
      });
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
    checkArguments {
      constructor = "mkDarwin";
      accepted = darwinArguments;
      required = ["hostPlatform"];
      inherit args hostPlatform;
    } (
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
      )
    );

  # The arguments the "mkNixOS" constructor accepts: "hostPlatform",
  # "overlays", and "pkgs", which it consumes itself, together with
  # the rest of what the "nixosSystem" function accepts, per its
  # definition in nixpkgs' "flake.nix" file, which forwards everything
  # but the "modules" argument to the argument pattern in nixpkgs'
  # "nixos/lib/eval-config.nix" file.
  #
  # It does not accept the "system" argument either. The "nixosSystem"
  # function turns that argument into the same "nixpkgs.system"
  # option, ignored for the same reason, given with the
  # "darwinArguments" list above.
  nixosArguments = [
    "baseModules"
    "extraModules"
    "hostPlatform"
    "lib"
    "modules"
    "modulesLocation"
    "overlays"
    "pkgs"
    "prefix"
    "specialArgs"
  ];

  mkNixOS = {
    overlays ? [],
    modules ? [],
    ...
  } @ args: let
    inherit (args) hostPlatform;
    pkgs =
      args.pkgs
      or (pkgsFor {
        system = hostPlatform;
        applyOverlays = false;
      });
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
    checkArguments {
      constructor = "mkNixOS";
      accepted = nixosArguments;
      required = ["hostPlatform"];
      inherit args hostPlatform;
    } (
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
      )
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

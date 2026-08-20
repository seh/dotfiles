# Assertions catching mis-selected host entries.
#
# Role-aware assertions are surfaced against the typed activation
# surface: each role ("profiles", "features") gets an unknown-name
# assertion, plus a cross-role mismatch assertion that catches a name
# selected under one role but advertised under another.
#
# Role-mismatch assertions run before the plain "unknown name"
# assertions so that the more specific diagnosis wins when a name was
# simply selected under the wrong role.
#
# The "knownProfiles", "knownFeatures", and "knownInterests"
# registries consulted here are flake-wide by design; see the comment
# block in "modules/_activation.nix" near the declaration of
# "dotfiles._knownProfiles" for the rationale. The preconditions table
# ("dotfiles._featurePreconditions") is consulted as well, to check
# contingent features' precondition edges and their possible misuse in
# a host's selections and in the implication graph.
{
  lib,
  config,
  ...
}: let
  # Define these bindings in the order in which the "assertions" list
  # at the bottom applies them: namespace-disjointness first (every
  # kind-aware check presupposes that each name denotes one kind),
  # then the role-parametric name checks, then the precondition,
  # contingent-feature, and implied-edge checks, then the platform
  # checks. The shared helpers come first, and each assertion's own
  # helpers sit just before it.
  #
  # Shared inputs, used by several assertions below.
  inherit (config.dotfiles) host;
  hostName = toString host.name;
  # The platform is detection-only: "modules/_activation.nix" computes
  # it from the evaluating package set and exposes it here. Host
  # records carry no platform attribute.
  platform = config.dotfiles._host.platform;

  knownProfiles = config.dotfiles._knownProfiles;
  knownFeatures = config.dotfiles._knownFeatures;
  knownInterests = config.dotfiles._knownInterests;
  knownNames = config.dotfiles._knownNames;

  featurePreconditions = config.dotfiles._featurePreconditions;
  contingentNames = builtins.attrNames featurePreconditions;

  # Quote and join names for the messages below.
  quoteNames = names: lib.concatMapStringsSep ", " (n: "\"${n}\"") names;

  # Profiles, features, and interests share one namespace: a name may
  # denote only one kind. The pairwise checks below name the offender
  # and both kinds. An interest-versus-feature collision is the case
  # the "uniq" guard on the module registries cannot catch (an
  # interest contributes no module body, so a body under its name
  # would count as a first definition, not a duplicate).
  namespacePairs = [
    {
      here = {
        kind = "profile";
        names = knownProfiles;
      };
      there = {
        kind = "feature";
        names = knownFeatures;
      };
    }
    {
      here = {
        kind = "profile";
        names = knownProfiles;
      };
      there = {
        kind = "interest";
        names = knownInterests;
      };
    }
    {
      here = {
        kind = "feature";
        names = knownFeatures;
      };
      there = {
        kind = "interest";
        names = knownInterests;
      };
    }
  ];
  namespaceAssertion = {
    here,
    there,
  }: let
    shared = lib.intersectLists here.names there.names;
    describeKind = kind:
      if kind == "interest"
      then "an interest"
      else "a ${kind}";
  in {
    assertion = shared == [];
    message = ''
      Resolving host "${hostName}": the name(s) ${quoteNames shared} are registered as both ${describeKind here.kind} and ${describeKind there.kind}, but profiles, features, and interests share one namespace, so a name may denote only one kind. Rename one of the registrations.
    '';
  };

  # Role-parametric driver. Iterating over this list (rather than
  # hard-coding "profiles" and "features") means that adding a third
  # role later—say, "bundles"—reduces to a single new entry.
  roles = [
    {
      name = "profiles";
      selected = host.profiles;
      excluded = host.excludeProfiles;
      forbidden = host.forbidProfiles;
      known = knownProfiles;
      humanSingular = "profile";
      humanPlural = "profile(s)";
      selectOption = "dotfiles.host.profiles";
      excludeOption = "dotfiles.host.excludeProfiles";
      forbidOption = "dotfiles.host.forbidProfiles";
    }
    {
      name = "features";
      selected = host.features;
      excluded = host.excludeFeatures;
      forbidden = host.forbidFeatures;
      known = knownNames;
      humanSingular = "feature";
      humanPlural = "feature(s)";
      selectOption = "dotfiles.host.features";
      excludeOption = "dotfiles.host.excludeFeatures";
      forbidOption = "dotfiles.host.forbidFeatures";
    }
  ];

  # Pair each role with "the other roles", so that each role's
  # selections can be checked against every other role's known set.
  # For the two-role case this degenerates to one pair per direction.
  crossPairs =
    lib.concatMap (
      here: map (there: {inherit here there;}) (builtins.filter (r: r.name != here.name) roles)
    )
    roles;

  # Role-mismatch assertion: names selected under "here.name" that are
  # actually known as "there.name".
  mismatchAssertion = {
    here,
    there,
  }: let
    misplaced = builtins.filter (n: builtins.elem n there.known) here.selected;
  in {
    assertion = misplaced == [];
    message = ''
      Resolving host "${hostName}": selecting ${lib.concatStringsSep ", " misplaced} under "${here.name}", but these are known ${there.humanPlural}. Move them to "${there.selectOption}".
    '';
  };

  # Unknown-name assertion for a single role. Elements that are
  # neither in "role.known" nor in any other role's "known" set count
  # as unknown; elements present in another role's "known" set are
  # instead caught by the role-mismatch assertion above.
  unknownAssertion = role: let
    otherKnown = lib.concatLists (map (r: r.known) (builtins.filter (r: r.name != role.name) roles));
    unknown =
      builtins.filter (
        n: !(builtins.elem n role.known) && !(builtins.elem n otherKnown)
      )
      role.selected;
  in {
    assertion = unknown == [];
    message = ''
      Resolving host "${hostName}": selecting ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Add the corresponding ${role.humanSingular} module or remove the name(s) from "${role.selectOption}".
    '';
  };

  # Unknown-exclude assertion: any name listed under a role's
  # "exclude" option must also appear in that role's known set. A
  # misspelled exclusion would otherwise silently fail to suppress,
  # leaving the host with an unintended activation set.
  unknownExcludeAssertion = role: let
    unknown = builtins.filter (n: !(builtins.elem n role.known)) role.excluded;
  in {
    assertion = unknown == [];
    message = ''
      Resolving host "${hostName}": excluding ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Correct the spelling or remove the name(s) from "${role.excludeOption}".
    '';
  };

  # Unknown-forbid assertion: any name listed under a role's
  # machine-wide "forbid" option must also appear in that role's known
  # set. Forbidding is an exclusion applied to every selector, so a
  # misspelled name would silently forbid nothing, just as a
  # misspelled exclusion would fail to suppress. A contingent feature
  # may be forbidden (it simply never activates), so no separate check
  # objects to that.
  unknownForbidAssertion = role: let
    unknown = builtins.filter (n: !(builtins.elem n role.known)) role.forbidden;
  in {
    assertion = unknown == [];
    message = ''
      Resolving host "${hostName}": forbidding ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Correct the spelling or remove the name(s) from "${role.forbidOption}".
    '';
  };

  # Every precondition must name a known feature or interest—never a
  # profile, and never a name nothing advertises.
  preconditionEdges =
    lib.concatMap (
      name:
        map (r: {
          feature = name;
          precondition = r;
        })
        featurePreconditions.${name}
    )
    contingentNames;
  describePreconditionEdge = {
    feature,
    precondition,
  }: ''The feature "${feature}" names "${precondition}" as a precondition'';
  preconditionsNamingProfiles =
    builtins.filter ({precondition, ...}: builtins.elem precondition knownProfiles)
    preconditionEdges;
  preconditionProfileAssertion = {
    assertion = preconditionsNamingProfiles == [];
    message = ''
      Resolving host "${hostName}": ${lib.concatMapStringsSep " " (
          edge: "${describePreconditionEdge edge}, which names a known profile, but preconditions may name only features or interests."
        )
        preconditionsNamingProfiles} Remove each profile name from its "preconditions" list.
    '';
  };
  unknownPreconditions =
    builtins.filter (
      {precondition, ...}:
        !(builtins.elem precondition knownNames)
        && !(builtins.elem precondition knownProfiles)
    )
    preconditionEdges;
  preconditionUnknownAssertion = {
    assertion = unknownPreconditions == [];
    message = ''
      Resolving host "${hostName}": ${lib.concatMapStringsSep " " (
          edge: "${describePreconditionEdge edge}, but no imported module advertises that name as a feature or an interest."
        )
        unknownPreconditions} Correct each misspelling or register
      each missing feature or interest.
    '';
  };

  # The preconditions table's keys must themselves be registered
  # features: the "mkFeature" function registers each contingent
  # feature's name alongside its "preconditions" list, so a key that
  # no imported module advertises as a feature was written into
  # "dotfiles.featurePreconditions" directly—whether a name nothing
  # registers or a name registered as a profile or an interest.
  unregisteredContingent =
    builtins.filter (n: !(builtins.elem n knownFeatures)) contingentNames;
  contingentRegistrationAssertion = {
    assertion = unregisteredContingent == [];
    message = ''
      Resolving host "${hostName}": the preconditions table keys the contingent feature(s) ${quoteNames unregisteredContingent}, but no imported module registers these names as features. A contingent feature must be registered via the "mkFeature" function; a name known only as a profile or an interest may not carry preconditions. Register each name with the "mkFeature" function (passing its "preconditions" list there) or remove its entry from "dotfiles.featurePreconditions".
    '';
  };

  # A contingent feature activates only through its preconditions, so
  # naming one in the selected features would activate it in an
  # inconsistent state, without the guarantee its body is written
  # against. Every selector's "features" list is authored, so the
  # check applies uniformly, with nothing suppressing it: the
  # machine's own "dotfiles.host.features" here, and each user's
  # "features" inside that user's nested evaluator. The message names
  # both surfaces because the nested per-user evaluator sees the
  # selection arrive from "dotfiles.users.<name>.features".
  selectedContingent = builtins.filter (n: builtins.elem n contingentNames) host.features;
  contingentSelectionAssertion = {
    assertion = selectedContingent == [];
    message = ''
      Resolving host "${hostName}": the selected features name the contingent feature(s) ${quoteNames selectedContingent} (written in "dotfiles.host.features" or, on a multi-user host, in "dotfiles.users.<name>.features"), but a contingent feature activates automatically exactly when all of its preconditions are met and may not be selected directly. Select its preconditions instead.
    '';
  };

  # For the same reason, no implied edge—a profile's "implies" list
  # included—may target a contingent feature. The implication graph is
  # computed once per evaluator by "modules/_activation.nix" and shared
  # through the "_implications" option.
  implications = config.dotfiles._implications;
  impliedFeatureTargets =
    if implications != null
    then
      lib.unique (
        lib.concatMap (
          edges: lib.concatMap (targets: targets.features or []) (lib.attrValues edges)
        ) (lib.attrValues implications)
      )
    else [];
  contingentImplicationTargets =
    builtins.filter (n: builtins.elem n impliedFeatureTargets) contingentNames;
  contingentImplicationTargetAssertion = {
    assertion = contingentImplicationTargets == [];
    message = ''
      Resolving host "${hostName}": the implication graph targets the contingent feature(s) ${quoteNames contingentImplicationTargets}, but a contingent feature activates automatically exactly when all of its preconditions are met and may not be an implied target. Remove the edge(s) from the implication graph.
    '';
  };

  # An interest edge joins the same implication graph as a feature or
  # profile edge, so the interest-target rule reads that assembled
  # graph directly. Derive every source→target implied-edge pair from
  # it, across each role, each source under a role, and each target
  # under each of that source's target roles.
  impliedEdgePairs =
    if implications != null
    then
      lib.concatMap (
        role:
          lib.concatMap (
            source:
              lib.concatMap (
                targetRole:
                  map (target: {
                    inherit source target;
                  })
                  implications.${role}.${source}.${targetRole}
              ) (builtins.attrNames implications.${role}.${source})
          ) (builtins.attrNames implications.${role})
      ) (builtins.attrNames implications)
    else [];
  isInterest = n: builtins.elem n knownInterests;

  # A feature or a profile may not imply an interest: a want is
  # expressed by a selector, not manufactured by a configuration unit.
  # Another interest may, so the source is exempted here.
  fabricatedInterestEdges =
    builtins.filter (e: isInterest e.target && !(isInterest e.source)) impliedEdgePairs;
  fabricatedInterestTargetAssertion = {
    assertion = fabricatedInterestEdges == [];
    message = ''
      Resolving host "${hostName}": the implication graph has a feature or profile imply the interest(s) ${quoteNames (lib.unique (map (e: e.target) fabricatedInterestEdges))}, but only another interest may imply an interest — a want is expressed by selection, not manufactured by a configuration unit. Remove the edge(s) from the source's "implies" list.
    '';
  };

  # An interest may imply only interests, never a feature or a profile,
  # so the want-world and the configuration-world do not cross through
  # implication.
  interestCrossingEdges =
    builtins.filter (e: isInterest e.source && !(isInterest e.target)) impliedEdgePairs;
  interestCrossingAssertion = {
    assertion = interestCrossingEdges == [];
    message = ''
      Resolving host "${hostName}": the interest(s) ${quoteNames (lib.unique (map (e: e.source) interestCrossingEdges))} imply non-interest names ${quoteNames (lib.unique (map (e: e.target) interestCrossingEdges))}, but an interest may imply only other interests, never a feature or a profile. Remove the crossing edge(s).
    '';
  };

  # A host with a name must know its platform, and detection from the
  # evaluating package set is the only source: host records carry no
  # platform attribute. This assertion fails only when a named host is
  # evaluated without a package set (for example, in a bare
  # instantiation of these modules). Without a platform, the
  # platform-support check below cannot judge anything.
  platformDetectedAssertion = {
    assertion = host.name == null || platform != null;
    message = ''
      Resolving host "${hostName}": the host has a name, but no platform could be detected because this evaluator provides no package set. Evaluate these modules with a package set, as the "mkHome", "mkDarwin", and "mkNixOS" constructors do.
    '';
  };

  # A profile may declare (via the "mkProfile" function's
  # "supportedPlatforms" argument) the platforms on which it may
  # activate; a host qualifies when its platform is one of them. The
  # "all" entry already omits unsupported profiles (see the
  # "implicationsFor" function in "modules/lib/_implications.nix");
  # this assertion rejects a host whose resolved activation reaches
  # one anyway, such as by selecting it directly in
  # "dotfiles.host.profiles". A host whose platform could not be
  # detected (no package set) is not checked, since its operating
  # system is unknown.
  profileSupportedPlatforms = config.dotfiles._profileSupportedPlatforms;
  unsupportedActiveProfiles =
    if platform == null
    then []
    else
      builtins.filter (
        name: let
          supported = profileSupportedPlatforms.${name} or null;
        in
          supported != null && !(builtins.elem platform supported)
      )
      config.dotfiles._host.activeProfiles;
  describeUnsupported = name: ''"${name}" (supports only ${lib.concatStringsSep ", " profileSupportedPlatforms.${name}})'';
  platformSupportAssertion = {
    assertion = unsupportedActiveProfiles == [];
    message = ''
      Resolving host "${hostName}": the profile(s) ${lib.concatMapStringsSep "; " describeUnsupported unsupportedActiveProfiles} may not activate on this host's platform, "${toString platform}". Remove the name(s) from "dotfiles.host.profiles".
    '';
  };
in {
  # Namespace-disjointness assertions run first: every other
  # kind-aware diagnosis presupposes that each name denotes one kind.
  # Mismatch assertions run before unknown-name assertions so that a
  # misplaced name produces the more actionable diagnosis. The
  # definitions above appear in this same order.
  assertions =
    map namespaceAssertion namespacePairs
    ++ map mismatchAssertion crossPairs
    ++ map unknownAssertion roles
    ++ map unknownExcludeAssertion roles
    ++ map unknownForbidAssertion roles
    ++ [
      preconditionProfileAssertion
      preconditionUnknownAssertion
      contingentRegistrationAssertion
      contingentSelectionAssertion
      contingentImplicationTargetAssertion
      fabricatedInterestTargetAssertion
      interestCrossingAssertion
      platformDetectedAssertion
      platformSupportAssertion
    ];
}

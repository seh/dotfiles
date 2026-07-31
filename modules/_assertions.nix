# Assertions catching mis-selected host entries.
#
# Role-aware assertions are surfaced against the typed authoring
# surface: each role ("profiles", "features", "interests") gets an
# unknown-name assertion for each of the three lists a machine or user
# writes under it—its selections, its exclusions, and the machine-wide
# forbid list—plus a cross-role mismatch assertion that catches a name
# written into one role's list while another role advertises it.
#
# Role-mismatch assertions run before the plain "unknown name"
# assertions so that the more specific diagnosis wins when a name was
# simply written under the wrong role.
#
# The "knownProfiles", "knownFeatures", and "knownInterests"
# registries consulted here are flake-wide by design; see the comment
# block in "modules/_activation.nix" near the declaration of
# "dotfiles._knownProfiles" for the rationale. The preconditions table
# ("dotfiles._featurePreconditions") is consulted as well, to check
# contingent features' precondition edges and their possible misuse in
# a host's selections and in the implication graph. The per-class
# module records ("dotfiles._featureClasses" and
# "dotfiles._profileClasses") are consulted too, to hold each feature
# to one side of the home/system divide and to catch a user selecting
# a name that configures the machine alone.
{
  lib,
  config,
  ...
}: let
  # Define these bindings in the order in which the "assertions" list
  # at the bottom applies them: namespace-disjointness first (every
  # kind-aware check presupposes that each name denotes one kind),
  # then the role-parametric name checks, then the precondition,
  # contingent-feature, and implied-edge checks, then the per-class
  # body checks, then the platform checks. The shared helpers come
  # first, and each assertion's own helpers sit just before it.
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

  # Every authoring list named below sits directly under
  # "dotfiles.host" at the machine level; each one but the
  # machine-wide forbid list also has a per-user counterpart under
  # "dotfiles.users.<name>". The messages below build both paths from
  # a list's bare name, since a name arriving at a managed user's
  # nested evaluator may have been written at either surface.
  hostOption = attr: "dotfiles.host.${attr}";
  userOption = attr: "dotfiles.users.<name>.${attr}";

  # Role-parametric driver. Iterating over this list (rather than
  # hard-coding "profiles", "features", and "interests") means that
  # adding a fourth role later—say, "bundles"—reduces to a single new
  # entry. Each role names the three lists a machine or user authors
  # under it and the registry of names it advertises, so that each
  # kind's own registry judges its own lists. These are the authoring
  # roles, one per kind; the activation walk carries two roles and
  # folds the interests into its "features" role (see
  # "modules/_activation.nix").
  roles = [
    {
      name = "profiles";
      excludeName = "excludeProfiles";
      forbidName = "forbidProfiles";
      selected = host.profiles;
      excluded = host.excludeProfiles;
      forbidden = host.forbidProfiles;
      known = knownProfiles;
      humanSingular = "profile";
      humanPlural = "profile(s)";
    }
    {
      name = "features";
      excludeName = "excludeFeatures";
      forbidName = "forbidFeatures";
      selected = host.features;
      excluded = host.excludeFeatures;
      forbidden = host.forbidFeatures;
      known = knownFeatures;
      humanSingular = "feature";
      humanPlural = "feature(s)";
    }
    {
      name = "interests";
      excludeName = "excludeInterests";
      forbidName = "forbidInterests";
      selected = host.interests;
      excluded = host.excludeInterests;
      forbidden = host.forbidInterests;
      known = knownInterests;
      humanSingular = "interest";
      humanPlural = "interest(s)";
    }
  ];

  # Pair each role with "the other roles", so that each role's lists
  # can be checked against every other role's known set.
  crossPairs =
    lib.concatMap (
      here: map (there: {inherit here there;}) (builtins.filter (r: r.name != here.name) roles)
    )
    roles;

  # The three lists each role carries: the names a machine or user
  # selects, the names it prunes from its own walk, and the names a
  # machine forbids outright. Each entry reads its list off a role
  # and spells that list's bare name, so the kind-mismatch check
  # covers all three lists from one definition. Forbidding is
  # machine-wide, so that list alone has no per-user counterpart to
  # name.
  listFamilies = [
    {
      entriesOf = role: role.selected;
      nameOf = role: role.name;
      perUser = true;
    }
    {
      entriesOf = role: role.excluded;
      nameOf = role: role.excludeName;
      perUser = true;
    }
    {
      entriesOf = role: role.forbidden;
      nameOf = role: role.forbidName;
      perUser = false;
    }
  ];

  # Role-mismatch assertion: names written into one of "here"'s lists
  # that are actually known as "there.name". The kinds are not
  # interchangeable—a feature carries configuration and may be
  # implied, while an interest carries none and is only selected or
  # entailed—so a misfiled name is an error naming the list it belongs
  # in.
  #
  # A contingent feature misfiled into an exclusion list draws advice
  # of its own. A machine-level entry in the "excludeFeatures" list is
  # a default an affected user overrides by selecting the same name,
  # and selecting a contingent feature is refused, so no user could
  # ever countermand that entry. A user excludes one in that user's
  # own list; a machine that wants one never to activate forbids it
  # instead.
  mismatchAssertion = family: {
    here,
    there,
  }: let
    misplaced = builtins.filter (n: builtins.elem n there.known) (family.entriesOf here);
    # Contingent features draw the separate advice above only in the
    # exclusion family, the one whose list under the destination role
    # is that role's own "excludeName" field.
    contingent =
      if family.nameOf there == there.excludeName
      then builtins.filter (n: builtins.elem n contingentNames) misplaced
      else [];
    ordinary = lib.subtractLists contingent misplaced;
    destination = let
      attr = family.nameOf there;
    in
      if family.perUser
      then ''"${hostOption attr}" or, on a multi-user host, "${userOption attr}"''
      else ''"${hostOption attr}"'';
    advice =
      lib.optional (ordinary != []) (
        if contingent == []
        then "Move them to ${destination}."
        else "Move ${lib.concatStringsSep ", " ordinary} to ${destination}."
      )
      ++ lib.optional (contingent != []) ''A contingent feature does not belong in a machine's "${hostOption there.excludeName}". A machine-level exclusion yields to a user who selects the same name, and no user may select a contingent feature, so the entry could never yield. Move ${lib.concatStringsSep ", " contingent} to the excluding user's own list ("${hostOption there.excludeName}" where home-manager alone manages the host, "${userOption there.excludeName}" on a multi-user host) or, to keep each name inactive everywhere, to "${hostOption there.forbidName}".'';
  in {
    assertion = misplaced == [];
    message = ''
      Resolving host "${hostName}": "${family.nameOf here}" lists ${lib.concatStringsSep ", " misplaced}, but each of these names is a known ${there.humanSingular}. ${lib.concatStringsSep " " advice}
    '';
  };

  # The names every role other than the given one advertises. A name
  # in one of a role's lists that another role knows is misfiled, not
  # unknown, so the three checks below leave it to the role-mismatch
  # assertions above and report only names nothing advertises.
  otherKnown = role:
    lib.concatLists (map (r: r.known) (builtins.filter (r: r.name != role.name) roles));

  # Unknown-name assertion for a single role.
  unknownAssertion = role: let
    others = otherKnown role;
    unknown =
      builtins.filter (
        n: !(builtins.elem n role.known) && !(builtins.elem n others)
      )
      role.selected;
  in {
    assertion = unknown == [];
    message = ''
      Resolving host "${hostName}": selecting ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Add the corresponding ${role.humanSingular} module or remove the name(s) from "${hostOption role.name}".
    '';
  };

  # Unknown-exclude assertion: any name listed under a role's
  # "exclude" option must also appear in that role's known set. A
  # misspelled exclusion would otherwise silently fail to suppress,
  # leaving the host with an unintended activation set.
  unknownExcludeAssertion = role: let
    others = otherKnown role;
    unknown =
      builtins.filter (
        n: !(builtins.elem n role.known) && !(builtins.elem n others)
      )
      role.excluded;
  in {
    assertion = unknown == [];
    message = ''
      Resolving host "${hostName}": excluding ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Correct the spelling or remove the name(s) from "${hostOption role.excludeName}".
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
    others = otherKnown role;
    unknown =
      builtins.filter (
        n: !(builtins.elem n role.known) && !(builtins.elem n others)
      )
      role.forbidden;
  in {
    assertion = unknown == [];
    message = ''
      Resolving host "${hostName}": forbidding ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Correct the spelling or remove the name(s) from "${hostOption role.forbidName}".
    '';
  };

  # Every precondition must name a known feature or interest—never a
  # profile, and never a name nothing advertises. A group's members
  # are subject to the same hygiene, so flatten each feature's
  # preconditions to every referenced name: each bare entry, plus
  # every member of each "anyOf" group.
  referencedNames = entries:
    lib.concatMap (
      entry:
        if builtins.isString entry
        then [entry]
        else entry.anyOf
    )
    entries;
  preconditionEdges =
    lib.concatMap (
      name:
        map (r: {
          feature = name;
          precondition = r;
        })
        (referencedNames featurePreconditions.${name})
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

  # Every "anyOf" group member must be non-contingent—an ordinary
  # feature or an interest, never a contingent feature. A contingent
  # alternative could form an unresolvable precondition cycle, so
  # keeping groups out of every cycle is what lets the cycle check in
  # the "modules/lib/_implications.nix" file follow only bare-name
  # edges. A bare precondition may still cite a contingent feature;
  # only group members are restricted.
  anyOfMemberEdges =
    lib.concatMap (
      name:
        lib.concatMap (
          entry:
            if builtins.isString entry
            then []
            else
              map (m: {
                feature = name;
                member = m;
              })
              entry.anyOf
        )
        featurePreconditions.${name}
    )
    contingentNames;
  contingentAnyOfMembers =
    builtins.filter ({member, ...}: builtins.elem member contingentNames) anyOfMemberEdges;
  anyOfMemberContingentAssertion = {
    assertion = contingentAnyOfMembers == [];
    message = ''
      Resolving host "${hostName}": the feature(s) ${quoteNames (lib.unique (map (e: e.feature) contingentAnyOfMembers))} list an "anyOf" precondition group naming the contingent feature(s) ${quoteNames (lib.unique (map (e: e.member) contingentAnyOfMembers))}, but a group's alternatives must be ordinary features or interests, never contingent features. A contingent alternative could form an unresolvable cycle; name a non-contingent feature or an interest instead.
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
  # naming one among the selections would activate it in an
  # inconsistent state, without the guarantee its body is written
  # against. Both selection lists that feed the walk's "features" role
  # are checked, since a contingent name written under "interests"
  # would enter the walk just the same; the role-mismatch assertion
  # above objects to the misfiling, and this one to the selection.
  # Every selection is authored, so the check applies uniformly, with
  # nothing suppressing it: the machine's own lists in a system
  # evaluator, and the machine's layered with that user's inside a
  # managed user's evaluator. The message names both surfaces because
  # a name arriving at a nested per-user evaluator may have been
  # written at either.
  selectedContingent =
    builtins.filter (n: builtins.elem n contingentNames) (host.features ++ host.interests);
  contingentSelectionAssertion = {
    assertion = selectedContingent == [];
    message = ''
      Resolving host "${hostName}": the selections name the contingent feature(s) ${quoteNames selectedContingent} (written in "dotfiles.host.features" or "dotfiles.host.interests" or, on a multi-user host, in the matching "dotfiles.users.<name>" list), but a contingent feature activates automatically exactly when all of its preconditions are met and may not be selected directly. Select its preconditions instead.
    '';
  };

  # Kind legality is platform-independent: whether an edge crosses
  # kinds does not depend on which platform a host runs, so an edge
  # that is illegal on one platform is illegal on every platform.
  # These checks therefore read the unfiltered raw edge registry
  # "dotfiles._impliedEdges" rather than the per-host,
  # platform-filtered implication graph. An edge hidden behind a
  # "supportedPlatforms" record for a platform this host does not run
  # would otherwise be dropped from the graph and escape the checks,
  # though its structure is illegal everywhere. Each registry entry is
  # a bare target name or a record "{ name = "<t>"; supportedPlatforms
  # = [...]; }"; the pair derivation takes the bare string or the
  # record's "name" field, ignoring the platform record.
  rawImpliedEdges = config.dotfiles._impliedEdges;
  rawEdgeName = entry:
    if builtins.isString entry
    then entry
    else entry.name;
  rawEdgePairs = lib.concatMap (
    source:
      map (entry: {
        inherit source;
        target = rawEdgeName entry;
      })
      rawImpliedEdges.${source}
  ) (builtins.attrNames rawImpliedEdges);
  # Classify a name by kind. A name among the known profiles is a
  # profile; among the known interests, an interest; otherwise it is a
  # feature. A contingent feature is one the preconditions registry
  # keys.
  isProfile = n: builtins.elem n knownProfiles;
  isInterest = n: builtins.elem n knownInterests;
  isContingent = n: builtins.elem n contingentNames;

  # No implied edge—a profile's "implies" list included—may target a
  # contingent feature, which activates automatically exactly when all
  # of its preconditions are met.
  contingentImplicationTargets =
    lib.unique (map (e: e.target) (builtins.filter (e: isContingent e.target) rawEdgePairs));
  contingentImplicationTargetAssertion = {
    assertion = contingentImplicationTargets == [];
    message = ''
      Resolving host "${hostName}": the implication graph targets the contingent feature(s) ${quoteNames contingentImplicationTargets}, but a contingent feature activates automatically exactly when all of its preconditions are met and may not be an implied target. Remove the edge(s) from the implication graph.
    '';
  };

  # A feature or a profile may not imply an interest: a want is
  # expressed by a selector, not manufactured by a configuration unit.
  # Another interest may, so the source is exempted here.
  fabricatedInterestEdges =
    builtins.filter (e: isInterest e.target && !(isInterest e.source)) rawEdgePairs;
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
    builtins.filter (e: isInterest e.source && !(isInterest e.target)) rawEdgePairs;
  interestCrossingAssertion = {
    assertion = interestCrossingEdges == [];
    message = ''
      Resolving host "${hostName}": the interest(s) ${quoteNames (lib.unique (map (e: e.source) interestCrossingEdges))} imply non-interest names ${quoteNames (lib.unique (map (e: e.target) interestCrossingEdges))}, but an interest may imply only other interests, never a feature or a profile. Remove the crossing edge(s).
    '';
  };

  # A feature may not imply a profile: profiles are coarser than
  # features, and reversing that would make host records misleading. A
  # profile source may imply a profile, and an interest source is
  # judged by the interest-crossing rule above, so only a feature-kind
  # source (neither profile nor interest) is caught here.
  featureImpliesProfileEdges =
    builtins.filter (e: !(isProfile e.source) && !(isInterest e.source) && isProfile e.target) rawEdgePairs;
  featureImpliesProfileAssertion = {
    assertion = featureImpliesProfileEdges == [];
    message = ''
      Resolving host "${hostName}": the feature(s) ${quoteNames (lib.unique (map (e: e.source) featureImpliesProfileEdges))} imply the profile(s) ${quoteNames (lib.unique (map (e: e.target) featureImpliesProfileEdges))}, but a feature may not imply a profile — profiles are coarser than features, and reversing that would make host records misleading. Remove the edge(s) or make ${quoteNames (lib.unique (map (e: e.source) featureImpliesProfileEdges))} a profile.
    '';
  };

  # A feature registers a body per module class, and those classes
  # divide between the machine and a user's home: a "nixDarwin" or
  # "nixOS" body configures the machine and follows the machine's own
  # selections, while a "homeManager" body configures one user's home
  # and follows that user's. One feature carrying both configures the
  # machine and a user's home at once, so a user who selects it
  # activates the home half alone. Registering one name across both
  # system classes stays legal—the "nix" feature does exactly that—and
  # this check covers features alone: the "essential" profile spans a
  # home class and a system class deliberately.
  #
  # The classes come from the "dotfiles._featureClasses" record,
  # derived from the module registry itself, so the record stays
  # faithful to the bodies actually present rather than to what a
  # registration claims.
  featureClasses = config.dotfiles._featureClasses;
  homeClass = "homeManager";
  systemClasses = ["nixDarwin" "nixOS"];
  systemClassesOf = classes: builtins.filter (c: builtins.elem c systemClasses) classes;
  mixedBodyFeatures =
    map (feature: {
      inherit feature;
      system = systemClassesOf featureClasses.${feature};
    })
    (builtins.filter (
      feature: let
        classes = featureClasses.${feature};
      in
        builtins.elem homeClass classes && systemClassesOf classes != []
    ) (builtins.attrNames featureClasses));
  # Name the bodies a mixed feature carries, leading with the home
  # body: "both a home-manager body and a nixDarwin body" when one
  # system class registers, and a serial enumeration when both do.
  describeMixedBodies = system:
    if lib.length system == 1
    then ''both a home-manager body and a ${lib.head system} body''
    else ''a home-manager body, ${lib.concatMapStringsSep ", " (c: "a ${c} body") (lib.init system)}, and a ${lib.last system} body'';
  describeMixedFeature = {
    feature,
    system,
  }: ''The feature "${feature}" carries ${describeMixedBodies system}'';
  mixedBodyAssertion = {
    assertion = mixedBodyFeatures == [];
    message = ''
      Resolving host "${hostName}": ${lib.concatMapStringsSep " " (
          entry: "${describeMixedFeature entry}, but a feature configures the machine or a user's home, never both. A system body follows the machine's own selections while a home body follows each user's, so a mixed feature activates only partially."
        )
        mixedBodyFeatures} Split each one into a machine-only feature
      and a home feature.
    '';
  };

  # A feature or profile whose every registered body is system-class
  # configures the machine and nothing else, so a user selecting it
  # receives nothing at all. Each user's own lists are judged, never
  # the resolved activation in force for that user: the machine's
  # selections layer into every user's walk, so a machine-nominated
  # system-only name legitimately appears there. That distinction
  # matters because the layering is an intended pattern—an
  # administrator nominates a system-only feature and a user's
  # contingent feature takes it as a precondition—and only the user's
  # own authoring is at fault.
  #
  # A name absent from the class record carries no bodies at all: a
  # name-only registration, an interest, or a misspelling that the
  # unknown-name checks above already diagnose. None is flagged here.
  profileClasses = config.dotfiles._profileClasses;
  userRecords = config.dotfiles.users;
  systemOnlyClassesOf = registry: name: let
    classes = registry.${name} or [];
  in
    if classes != [] && !(builtins.elem homeClass classes)
    then classes
    else [];
  systemOnlySelections = registry: entriesOf:
    lib.concatMap (
      user:
        lib.concatMap (
          name: let
            classes = systemOnlyClassesOf registry name;
          in
            lib.optional (classes != []) {inherit user name classes;}
        )
        (entriesOf userRecords.${user})
    )
    (builtins.attrNames userRecords);
  # Both lists that a user writes into the walk's "features" role are
  # judged, since a system-only feature name written under "interests"
  # enters that role just the same; the role-mismatch assertion above
  # objects to the misfiling, and this one to the selection.
  userSystemOnlyFeatures =
    systemOnlySelections featureClasses (record: record.features ++ record.interests);
  userSystemOnlyProfiles = systemOnlySelections profileClasses (record: record.profiles);
  describeSystemOnlyClasses = classes:
    if lib.length classes == 1
    then ''only for the ${lib.head classes} class''
    else ''only for the ${lib.concatStringsSep " and " classes} classes'';
  describeSystemOnlySelection = kind: destination: {
    user,
    name,
    classes,
  }: ''the user "${user}" selects the ${kind} "${name}", which carries configuration ${describeSystemOnlyClasses classes} and so does nothing for a user; select it in "${destination}" so the machine applies it.'';
  userSystemOnlyFeatureAssertion = {
    assertion = userSystemOnlyFeatures == [];
    message = ''
      Resolving host "${hostName}": ${
        lib.concatMapStringsSep " " (describeSystemOnlySelection "feature" (hostOption "features"))
        userSystemOnlyFeatures
      }
    '';
  };
  userSystemOnlyProfileAssertion = {
    assertion = userSystemOnlyProfiles == [];
    message = ''
      Resolving host "${hostName}": ${
        lib.concatMapStringsSep " " (describeSystemOnlySelection "profile" (hostOption "profiles"))
        userSystemOnlyProfiles
      }
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
  # this assertion rejects a host whose resolved activation includes
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
      Resolving host "${hostName}": the profile(s) ${lib.concatMapStringsSep "; " describeUnsupported unsupportedActiveProfiles} may not activate on this host's platform, "${toString platform}". Remove the name(s) from "dotfiles.host.profiles" or, on a multi-user host, from "dotfiles.users.<name>.profiles".
    '';
  };
in {
  # Namespace-disjointness assertions run first: every other
  # kind-aware diagnosis presupposes that each name denotes one kind.
  # The mismatch assertions—one per list family, per ordered pair of
  # roles—run before the unknown-name assertions so that a misfiled
  # name produces the more actionable diagnosis. The definitions above
  # appear in this same order.
  assertions =
    map namespaceAssertion namespacePairs
    ++ lib.concatMap (family: map (mismatchAssertion family) crossPairs) listFamilies
    ++ map unknownAssertion roles
    ++ map unknownExcludeAssertion roles
    ++ map unknownForbidAssertion roles
    ++ [
      preconditionProfileAssertion
      preconditionUnknownAssertion
      anyOfMemberContingentAssertion
      contingentRegistrationAssertion
      contingentSelectionAssertion
      contingentImplicationTargetAssertion
      fabricatedInterestTargetAssertion
      interestCrossingAssertion
      featureImpliesProfileAssertion
      mixedBodyAssertion
      userSystemOnlyFeatureAssertion
      userSystemOnlyProfileAssertion
      platformDetectedAssertion
      platformSupportAssertion
    ];
}

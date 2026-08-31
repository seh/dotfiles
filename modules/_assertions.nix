# Assertions catching mis-selected host entries.
#
# Role-aware assertions are surfaced against the typed authoring
# surface: each role ("features", "interests") gets an unknown-name
# assertion for each of three things a machine or user writes under
# it—its selections, its exclusions, and what it forbids
# machine-wide—plus a cross-role mismatch assertion that catches a
# name written into one role's list while the other role advertises
# it.
#
# Role-mismatch assertions run before the plain "unknown name"
# assertions so that the more specific diagnosis wins when a name was
# simply written under the wrong role.
#
# The "knownFeatures" and "knownInterests" registries consulted here
# are flake-wide by design; see the comment block in the
# "modules/_activation.nix" file near the declaration of the
# "dotfiles._knownFeatures" option for the rationale. The
# preconditions table ("dotfiles._featurePreconditions") is consulted
# as well, to check contingent features' precondition edges and their
# possible misuse in a host's selections and in the implication graph.
# The per-class module record ("dotfiles._featureClasses") is
# consulted too, to keep each feature on one side of the home/system
# divide and to catch a user selecting a name that configures the
# machine alone.
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
  inherit (import ./lib/_diagnostics.nix) describeHost;
  # Every message below opens with this phrase, so that a host lacking
  # a name reads as prose.
  hostLabel = describeHost host.name;
  # The platform is detection-only: the "modules/_activation.nix" file
  # computes it from the evaluating package set and exposes it here.
  # Host records have no platform attribute.
  platform = config.dotfiles._host.platform;

  knownFeatures = config.dotfiles._knownFeatures;
  knownInterests = config.dotfiles._knownInterests;
  knownNames = config.dotfiles._knownNames;

  featurePreconditions = config.dotfiles._featurePreconditions;
  contingentNames = builtins.attrNames featurePreconditions;

  # Quote and join names for the messages below.
  quoteNames = names: lib.concatMapStringsSep ", " (n: "\"${n}\"") names;

  # Features and interests share one namespace: a name may belong to
  # only one kind. The check below cites the offender and both kinds.
  # This collision is the case the "uniq" guard on the module
  # registries cannot catch (an interest contributes no module body,
  # so a body under its name would count as a first definition, not a
  # duplicate).
  namespacePairs = [
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
  in {
    assertion = shared == [];
    message = ''
      Resolving ${hostLabel}: both the ${here.kind} registry and the
      ${there.kind} registry contain the name(s) ${quoteNames shared},
      but features and interests share one namespace: a name may
      belong to only one kind. Rename one of the two registrations.
    '';
  };

  # Every authoring list below sits directly under the "dotfiles.host"
  # record at the machine level; each one but "forbidFeatures" and
  # "forbidInterests" also has a per-user counterpart under the
  # "dotfiles.users.<name>" record. The messages below build both
  # paths from a list's bare name, since a name arriving at a managed
  # user's nested evaluator may have been written at either surface.
  hostOption = attr: "dotfiles.host.${attr}";
  userOption = attr: "dotfiles.users.<name>.${attr}";

  # Role-parametric driver. Iterating over this list (rather than
  # hard-coding "features" and "interests") means that adding a third
  # role later—say, "bundles"—reduces to a single new entry. Each role
  # identifies the three lists a machine or user authors under it and
  # the registry of names it advertises, so that each kind's own
  # registry judges its own lists. These are the authoring roles, one
  # per kind; the activation walk keeps one list and folds both kinds
  # into it (see the "modules/_activation.nix" file).
  roles = [
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

  # Pair each role with the other role, so that each role's lists can
  # be checked against the other's known set.
  crossPairs =
    lib.concatMap (
      here: map (there: {inherit here there;}) (builtins.filter (r: r.name != here.name) roles)
    )
    roles;

  # The three lists each role declares: the names a machine or user
  # selects, the names it prunes from its own walk, and the names a
  # machine forbids outright. Each entry reads its list off a role and
  # spells that list's bare name, so the kind-mismatch check covers
  # all three lists from one definition. Forbidding is machine-wide,
  # so that list alone has no per-user counterpart.
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

  # Role-mismatch assertion: names written into one of the "here"
  # role's lists that are actually known under the "there.name" role.
  # The kinds are not interchangeable—a feature declares configuration
  # and may be implied, while an interest declares none and is only
  # selected or entailed—so a misfiled name is an error that cites the
  # list it belongs in.
  #
  # A contingent feature misfiled into an exclusion list draws advice
  # of its own. A machine-level entry in the "excludeFeatures" list is
  # a default an affected user overrides by selecting the same name,
  # and selecting a contingent feature fails the build, so no user
  # could ever countermand that entry. A user excludes one in that
  # user's own list; a machine that wants one never to activate
  # forbids it instead.
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
      then ''the "${hostOption attr}" list or, on a multi-user host, the "${userOption attr}" list''
      else ''the "${hostOption attr}" list'';
    advice =
      lib.optional (ordinary != []) (
        if contingent == []
        then "Move them to ${destination}."
        else "Move ${lib.concatStringsSep ", " ordinary} to ${destination}."
      )
      ++ lib.optional (contingent != []) ''A contingent feature does not belong in a machine's "${hostOption there.excludeName}" list. A machine-level exclusion yields to a user who selects the same name, and no user may select a contingent feature, so the entry could never yield. Move ${lib.concatStringsSep ", " contingent} to the excluding user's own list (the "${hostOption there.excludeName}" list where home-manager alone manages the host, the "${userOption there.excludeName}" list on a multi-user host) or, to keep each name inactive everywhere, to the "${hostOption there.forbidName}" list.'';
  in {
    assertion = misplaced == [];
    message = ''
      Resolving ${hostLabel}: "${family.nameOf here}" lists
      ${lib.concatStringsSep ", " misplaced}, but each of these names
      is a known ${there.humanSingular}.
      ${lib.concatStringsSep " " advice}
    '';
  };

  # The names every role other than the given one advertises. A name
  # in one of a role's lists that the other role knows is misfiled,
  # not unknown, so the three checks below leave it to the
  # role-mismatch assertions above and report only names nothing
  # advertises.
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
      Resolving ${hostLabel}: this configuration selects
      ${role.humanPlural} that no imported ${role.humanSingular}
      module advertises: ${lib.concatStringsSep ", " unknown}. Add the
      corresponding ${role.humanSingular} module, or remove the
      name(s) from the "${hostOption role.name}" list (on a multi-user host,
      from the matching "dotfiles.users.<name>" list).
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
      Resolving ${hostLabel}: this configuration excludes
      ${role.humanPlural} that no imported ${role.humanSingular}
      module advertises: ${lib.concatStringsSep ", " unknown}. Each
      such entry suppresses nothing. Correct the spelling or remove
      the name(s) from the "${hostOption role.excludeName}" list (on a
      multi-user host, from the matching "dotfiles.users.<name>"
      list).
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
      Resolving ${hostLabel}: this machine forbids ${role.humanPlural}
      that no imported ${role.humanSingular} module advertises:
      ${lib.concatStringsSep ", " unknown}. Each such entry forbids
      nothing. Correct the spelling or remove the name(s) from
      the "${hostOption role.forbidName}" list.
    '';
  };

  # Every precondition must cite a known feature or interest—never a
  # name nothing advertises. A group's members are subject to the same
  # hygiene, so flatten each feature's preconditions to every
  # referenced name: each bare entry, plus every member of each
  # "anyOf" group.
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
  }: ''The feature "${feature}" lists "${precondition}" as a precondition'';
  unknownPreconditions =
    builtins.filter ({precondition, ...}: !(builtins.elem precondition knownNames))
    preconditionEdges;
  preconditionUnknownAssertion = {
    assertion = unknownPreconditions == [];
    message = ''
      Resolving ${hostLabel}: ${lib.concatMapStringsSep " " (
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
      Resolving ${hostLabel}: the feature(s)
      ${quoteNames (lib.unique (map (e: e.feature) contingentAnyOfMembers))}
      list an "anyOf" precondition group that contains the contingent
      feature(s)
      ${quoteNames (lib.unique (map (e: e.member) contingentAnyOfMembers))},
      but a group's alternatives must be ordinary features or
      interests, never contingent features. A contingent alternative
      could form an unresolvable cycle; list a non-contingent feature
      or an interest instead.
    '';
  };

  # The preconditions table's keys must themselves be registered
  # features: the "mkFeature" function registers each contingent
  # feature's name alongside its "preconditions" list, so a key that
  # no imported module advertises as a feature was written into the
  # "dotfiles.featurePreconditions" registry directly—whether a name
  # nothing registers or a name registered as an interest.
  unregisteredContingent =
    builtins.filter (n: !(builtins.elem n knownFeatures)) contingentNames;
  contingentRegistrationAssertion = {
    assertion = unregisteredContingent == [];
    message = ''
      Resolving ${hostLabel}: "dotfiles.featurePreconditions" contains
      entries for the contingent feature(s)
      ${quoteNames unregisteredContingent}, but no imported module
      registers these names as features. A contingent feature comes
      from the "mkFeature" function, and an interest may not have
      preconditions. Pass each name's "preconditions" list to the
      "mkFeature" function, or remove its entry from
      "dotfiles.featurePreconditions".
    '';
  };

  # A contingent feature activates only through its preconditions, so
  # selecting one would activate it in an inconsistent state, without
  # the guarantee its body is written against. Both selection lists
  # that feed the walk are checked, since a contingent name written
  # under the "interests" list would enter the walk just the same; the
  # role-mismatch assertion above objects to the misfiling, and this
  # one to the selection. Every selection is authored, so the check
  # applies uniformly, with nothing suppressing it: the machine's own
  # lists in a system evaluator, and the machine's layered with that
  # user's inside a managed user's evaluator. The message specifies
  # both surfaces because a name arriving at a nested per-user
  # evaluator may have been written at either.
  #
  # The message states the preconditions this evaluator leaves unmet,
  # since those are what a reader selects in the rejected feature's
  # place. Each one arrives with its kind, because a feature and an
  # interest are selected under different lists, and a member of an
  # "anyOf" group stands apart from a plain conjunct, because
  # activating any one member of a group satisfies it.
  selectedContingent =
    builtins.filter (n: builtins.elem n contingentNames) (host.features ++ host.interests);
  # Every name this evaluator's walk brought into effect, active
  # features and expressed interests alike, since a precondition may
  # cite either kind. Each precondition below answers to this set.
  activeNames =
    config.dotfiles._host.activeFeatures
    ++ config.dotfiles._host.expressedInterests;
  # A precondition entry this evaluator's own activation does not
  # satisfy: a bare name whose feature or interest is inactive, or a
  # group none of whose members is active.
  unmetEntry = entry:
    if builtins.isString entry
    then !(builtins.elem entry activeNames)
    else !(lib.any (m: builtins.elem m activeNames) entry.anyOf);
  # Describe a precondition name together with the kind it is
  # registered under, since the two kinds are selected under different
  # lists.
  describeKindedName = name:
    if builtins.elem name knownInterests
    then ''the interest "${name}"''
    else ''the feature "${name}"'';
  # Join described phrases as an English enumeration with a serial
  # comma: one phrase stands alone, two join with the conjunction, and
  # three or more separate with commas before a final conjunction.
  joinPhrases = conjunction: phrases:
    if lib.length phrases == 1
    then lib.head phrases
    else if lib.length phrases == 2
    then "${lib.head phrases} ${conjunction} ${lib.last phrases}"
    else "${lib.concatStringsSep ", " (lib.init phrases)}, ${conjunction} ${lib.last phrases}";
  # A group reads as a disjunction, so its alternatives join with "or"
  # under a phrase saying that one of them suffices.
  describeUnmetEntry = entry:
    if builtins.isString entry
    then describeKindedName entry
    else "any one of ${joinPhrases "or" (map describeKindedName entry.anyOf)}";
  describeSelectedContingent = name: let
    unmet = builtins.filter unmetEntry featurePreconditions.${name};
  in
    if unmet == []
    then ''The feature "${name}" is contingent, and this configuration already meets every one of its preconditions, so it activates without the selection.''
    else ''The feature "${name}" is contingent, and this configuration does not meet these preconditions: ${joinPhrases "and" (map describeUnmetEntry unmet)}.'';
  contingentSelectionAssertion = {
    assertion = selectedContingent == [];
    message = ''
      Resolving ${hostLabel}:
      ${lib.concatMapStringsSep " " describeSelectedContingent selectedContingent}
      A contingent feature activates on its own exactly when every one
      of its preconditions is satisfied. You may not select one
      directly: not in "dotfiles.host.features", not in
      "dotfiles.host.interests", and not, on a multi-user host, in
      the matching "dotfiles.users.<name>" list. Remove each such
      selection and select the missing preconditions in its place.
    '';
  };

  # A machine that provisions users may not exclude a contingent
  # feature. An entry in the machine's "excludeFeatures" list is one
  # an affected user countermands by asking for the same
  # name—selecting it directly, or selecting a bundle that entails
  # it—and that chance to opt back in is the whole of what separates
  # it from the "forbidFeatures" list; selecting a contingent feature
  # fails the build and no implied edge may target one, so every way
  # in is closed and the exclusion applies absolutely while sitting in
  # the list that promises otherwise. A machine that wants a
  # contingent feature never to activate writes it in
  # "forbidFeatures", the list whose entries already apply absolutely.
  #
  # Only the machine's "excludeFeatures" list is judged: the walk
  # consults each exclusion list for its own kind alone, so a
  # contingent feature's name in the "excludeInterests" list withholds
  # nothing, and the role-mismatch assertion alone objects to that
  # misfiling.
  #
  # A user's own exclusion is legitimate: it always applies, for that
  # user alone, and nothing needs to countermand it. The check
  # therefore runs only where the "config.dotfiles.users" registry is
  # non-empty—the evaluator that provisions other people. A managed
  # user's nested evaluator never receives that registry's
  # declaration, and neither does a home configuration serving one
  # person alone, so a user's own exclusion stands. Gating on it also
  # surfaces one machine-level mistake once rather than once per
  # managed user, and lets the message point at the machine's own list
  # exactly.
  #
  # Deferred alternative: an administrator may legitimately want
  # features A and B but not C, where C activates automatically from
  # them, without imposing that on every user. Accommodating that wish
  # needs a way for a user to relax a machine-level exclusion that is
  # not selection—an un-exclusion. Removing a suppression adds no way
  # for a name to come into effect, so the guarantee that a contingent
  # feature is active exactly when its preconditions are satisfied
  # would stand. The cost is a second mechanism for relaxing an
  # exclusion, used only by contingent features, beside the existing
  # one in which selecting a name relaxes it.
  excludedContingent =
    lib.optionals ((config.dotfiles.users or {}) != {})
    (lib.unique (builtins.filter (n: builtins.elem n contingentNames) host.excludeFeatures));
  contingentExclusionAssertion = {
    assertion = excludedContingent == [];
    message = ''
      Resolving ${hostLabel}: the "${hostOption "excludeFeatures"}"
      list contains the contingent feature(s)
      ${quoteNames excludedContingent}. This machine provisions users,
      and a machine-level exclusion yields only to a user who asks for
      the name. Nobody can ask for a contingent feature: no user may
      select one, and no implied edge may target one, so this entry
      could never yield. Remove each name; to keep the feature
      inactive everywhere, list it in the
      "${hostOption "forbidFeatures"}" list instead.
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
  # Classify a name by kind. A name among the known interests is an
  # interest; every other name is a feature. A contingent feature is
  # one the preconditions registry keys.
  isInterest = n: builtins.elem n knownInterests;
  isContingent = n: builtins.elem n contingentNames;

  # No implied edge may target a contingent feature, which activates
  # automatically exactly when all of its preconditions are met.
  contingentImplicationTargets =
    lib.unique (map (e: e.target) (builtins.filter (e: isContingent e.target) rawEdgePairs));
  contingentImplicationTargetAssertion = {
    assertion = contingentImplicationTargets == [];
    message = ''
      Resolving ${hostLabel}: an "implies" list cites the contingent
      feature(s) ${quoteNames contingentImplicationTargets} as
      targets, but a contingent feature activates on its own exactly
      when every one of its preconditions is satisfied, and no implied
      edge may target one. Remove the name(s) from each such "implies"
      list.
    '';
  };

  # A feature may not imply an interest: a want is expressed by a
  # selector, not manufactured by a configuration unit. Another
  # interest may, so the source is exempted here.
  fabricatedInterestEdges =
    builtins.filter (e: isInterest e.target && !(isInterest e.source)) rawEdgePairs;
  fabricatedInterestTargetAssertion = {
    assertion = fabricatedInterestEdges == [];
    message = ''
      Resolving ${hostLabel}: the feature(s)
      ${quoteNames (lib.unique (map (e: e.source) fabricatedInterestEdges))}
      imply the interest(s)
      ${quoteNames (lib.unique (map (e: e.target) fabricatedInterestEdges))},
      but only an interest may imply an interest: an interest is a
      want, a selector expresses a want, and a feature cannot
      manufacture one. Remove each such edge from the source's
      "implies" list.
    '';
  };

  # An interest may imply only interests, never a feature, so the
  # want-world and the configuration-world do not cross through
  # implication.
  interestCrossingEdges =
    builtins.filter (e: isInterest e.source && !(isInterest e.target)) rawEdgePairs;
  interestCrossingAssertion = {
    assertion = interestCrossingEdges == [];
    message = ''
      Resolving ${hostLabel}: the interest(s)
      ${quoteNames (lib.unique (map (e: e.source) interestCrossingEdges))}
      imply the non-interest name(s)
      ${quoteNames (lib.unique (map (e: e.target) interestCrossingEdges))},
      but an interest may imply only other interests, never a feature.
      Remove each crossing edge from its source's "implies" list.
    '';
  };

  # A feature registers a body per module class, and those classes
  # divide between the machine and a user's home: a "nixDarwin" or
  # "nixOS" body configures the machine and follows the machine's own
  # selections, while a "homeManager" body configures one user's home
  # and follows that user's. One feature registering both configures
  # the machine and a user's home at once, so a user who selects it
  # activates the home half alone. Registering one name across both
  # system classes stays legal—the "nix" feature and the
  # "shell/zsh/integration" feature each do exactly that.
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
  # Describe the bodies a mixed feature registers, leading with the
  # home body: "both a home-manager body and a nixDarwin body" when
  # one system class registers, and a serial enumeration when both do.
  describeMixedBodies = system:
    if lib.length system == 1
    then ''both a home-manager body and a ${lib.head system} body''
    else ''a home-manager body, ${lib.concatMapStringsSep ", " (c: "a ${c} body") (lib.init system)}, and a ${lib.last system} body'';
  describeMixedFeature = {
    feature,
    system,
  }: ''The feature "${feature}" registers ${describeMixedBodies system}'';
  mixedBodyAssertion = {
    assertion = mixedBodyFeatures == [];
    message = ''
      Resolving ${hostLabel}: ${lib.concatMapStringsSep " " (
          entry: "${describeMixedFeature entry}, but a feature configures the machine or a user's home, never both. A system body follows the machine's own selections while a home body follows each user's, so a mixed feature activates only partially."
        )
        mixedBodyFeatures} Split each one into a machine-only feature
      and a home feature.
    '';
  };

  # A feature whose every registered body is system-class configures
  # the machine and nothing else, so a user selecting it receives
  # nothing at all. Each user's own lists are judged, never the
  # resolved activation in force for that user: the machine's
  # selections layer into every user's walk, so a machine-nominated
  # system-only name legitimately appears there. That distinction
  # matters because the layering is an intended pattern—an
  # administrator nominates a system-only feature and a user's
  # contingent feature takes it as a precondition—and only the user's
  # own authoring is at fault.
  #
  # A name absent from the class record has no bodies at all: a
  # name-only registration, an interest, or a misspelling that the
  # unknown-name checks above already diagnose. None is flagged here.
  userRecords = config.dotfiles.users or {};
  systemOnlyClassesOf = name: let
    classes = featureClasses.${name} or [];
  in
    if classes != [] && !(builtins.elem homeClass classes)
    then classes
    else [];
  # Both lists that a user writes into the walk are judged, since a
  # system-only feature name written under the "interests" list enters
  # the walk just the same; the role-mismatch assertion above objects
  # to the misfiling, and this one to the selection.
  userSystemOnlyFeatures =
    lib.concatMap (
      user:
        lib.concatMap (
          name: let
            classes = systemOnlyClassesOf name;
          in
            lib.optional (classes != []) {inherit user name classes;}
        )
        (userRecords.${user}.features ++ userRecords.${user}.interests)
    )
    (builtins.attrNames userRecords);
  describeSystemOnlyClasses = classes:
    if lib.length classes == 1
    then ''only for the ${lib.head classes} class''
    else ''only for the ${lib.concatStringsSep " and " classes} classes'';
  describeSystemOnlySelection = {
    user,
    name,
    classes,
  }: ''the user "${user}" selects the feature "${name}", which declares configuration ${describeSystemOnlyClasses classes} and so does nothing for a user; select it in the "${hostOption "features"}" list so the machine applies it.'';
  userSystemOnlyFeatureAssertion = {
    assertion = userSystemOnlyFeatures == [];
    message = ''
      Resolving ${hostLabel}: ${
        lib.concatMapStringsSep " " describeSystemOnlySelection userSystemOnlyFeatures
      }
    '';
  };

  # A host with a name must know its platform, and detection from the
  # evaluating package set is the only source: host records have no
  # platform attribute. This assertion fails only when a named host is
  # evaluated without a package set (for example, in a bare
  # instantiation of these modules). Without a platform, the
  # platform-support check below cannot judge anything.
  platformDetectedAssertion = {
    assertion = host.name == null || platform != null;
    message = ''
      Resolving ${hostLabel}: the host has a name, but this evaluator
      provides no package set, and only a package set can tell this
      flake the host's platform. Evaluate these modules with a package
      set, as the "mkHome", "mkDarwin", and "mkNixOS" constructors do.
    '';
  };

  # A feature may declare (via the "supportedPlatforms" argument of
  # the "mkFeature" function) the platforms on which it may activate;
  # a host qualifies when its platform is one of them. The implication
  # graph already drops an unsupported name from every target list it
  # assembles (see the "implicationsFor" function in the
  # "modules/lib/_implications.nix" file); this assertion rejects a
  # host whose resolved activation includes one anyway, such as by
  # selecting it directly. A host whose platform could not be detected
  # (no package set) is not checked, since its operating system is
  # unknown.
  supportedPlatforms = config.dotfiles._supportedPlatforms;
  unsupportedActiveNames =
    if platform == null
    then []
    else
      builtins.filter (
        name: let
          supported = supportedPlatforms.${name} or null;
        in
          supported != null && !(builtins.elem platform supported)
      )
      config.dotfiles._host.activeFeatures;
  describeUnsupported = name: ''the feature "${name}" (supports only ${lib.concatStringsSep ", " supportedPlatforms.${name}})'';
  # A platform constraint belongs to a feature, never to an interest.
  # An interest is a want, and a want applies wherever a person has
  # it; what runs on some platforms and not others is the feature that
  # satisfies the want, which declares its own platforms. The
  # "mkInterest" function fails the build for the key already, so this
  # check closes the remaining way in: a hand-written entry in the
  # flake-level registry, whose keys are bare names. Without it the
  # constraint would sit in two places at once and could disagree with
  # itself.
  platformConstrainedInterests =
    builtins.filter (name: builtins.elem name knownInterests)
    (builtins.attrNames supportedPlatforms);
  interestPlatformAssertion = {
    assertion = platformConstrainedInterests == [];
    message = ''
      Resolving ${hostLabel}: "dotfiles.supportedPlatforms" has
      entries for the interest(s)
      ${quoteNames platformConstrainedInterests}, but an interest
      applies on every platform: a platform limits the feature that
      satisfies a want, never the want itself. Declare
      "supportedPlatforms" on each such feature instead, and remove
      these entries from "dotfiles.supportedPlatforms".
    '';
  };

  platformSupportAssertion = {
    assertion = unsupportedActiveNames == [];
    message = ''
      Resolving ${hostLabel}:
      ${lib.concatMapStringsSep "; " describeUnsupported unsupportedActiveNames}
      may not activate on this host's platform,
      "${toString platform}". Remove the name(s) from
      the "${hostOption "features"}" list or, on a multi-user host, from the
      matching "dotfiles.users.<name>" list.
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
      preconditionUnknownAssertion
      anyOfMemberContingentAssertion
      contingentRegistrationAssertion
      contingentSelectionAssertion
      contingentExclusionAssertion
      contingentImplicationTargetAssertion
      fabricatedInterestTargetAssertion
      interestCrossingAssertion
      mixedBodyAssertion
      userSystemOnlyFeatureAssertion
      platformDetectedAssertion
      interestPlatformAssertion
      platformSupportAssertion
    ];
}

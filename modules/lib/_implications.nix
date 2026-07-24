{lib}: let
  # Assemble the per-host implication graph from the co-located
  # "implies" declarations that each profile, feature, and interest
  # carries. An implied edge is a property of its source, so it
  # lives in the source's own file; the "impliedEdges" argument
  # carries those declarations, keyed by source name, and this
  # function classifies every target by role to produce the
  # role-keyed graph that the "expandClosure" function consumes.
  #
  # The graph is role-keyed ("profiles" / "features") so that a
  # future role (e.g. "bundles") becomes a purely additive change:
  # add a new top-level key and the "expandClosure" function walks it
  # unchanged, since its walk iterates over whatever roles the graph
  # advertises.
  #
  # This function runs per-host so that the per-profile platform
  # filtering described below can vary with the host's platform.
  #
  # This function filters every profile entry's "profiles" list
  # against the profiles' declared "supportedPlatforms" (see the
  # "mkProfile" function): a profile the host's platform does not
  # support never arrives through the graph, whether via the "all"
  # entry or via a narrower one such as "desktop". The assertion in
  # "modules/_assertions.nix" rejects a host that would activate
  # such a profile anyway (e.g. by selecting it directly).
  #
  # This function includes a record-form edge ("{ name = "<target>";
  # supportedPlatforms = [<systems>]; }") only when the host's
  # platform is one of the listed systems; this is how a source brings
  # along a target on some platforms and not others.
  #
  # This function computes the "all" entry's targets from
  # "knownProfiles" rather than reading a declared edge, since "all"
  # is not a co-located edge: introducing a new profile folds it into
  # "all" automatically, and hosts that want to skip one of the
  # profiles it brings along list that profile under
  # "excludeProfiles".
  #
  # The graph across all roles MUST form a DAG; the "expandClosure"
  # function rejects cycles at runtime.
  implicationsFor = {
    knownProfiles,
    # Nixpkgs system string (e.g. "aarch64-darwin"), or null when
    # the host record does not carry one.
    platform ? null,
    # Per-profile platform support, keyed by profile name; each
    # value lists the platforms on which that profile may activate.
    # A profile absent from this record may activate anywhere; a
    # profile present in it requires the host's platform to be one
    # of the listed values, so a null platform errs toward omitting
    # every platform-constrained profile.
    profileSupportedPlatforms ? {},
    # Co-located implied edges, keyed by source name; each value is
    # the source's "implies" list, whose entries are bare target
    # names or record-form platform-conditional edges. This function
    # assembles them into the role-keyed graph below.
    impliedEdges ? {},
    ...
  }: let
    availableOnHost = name: let
      supported = profileSupportedPlatforms.${name} or null;
    in
      supported == null || (platform != null && builtins.elem platform supported);
    filterEntryProfiles = entry:
      entry
      // lib.optionalAttrs (entry ? profiles) {
        profiles = builtins.filter availableOnHost entry.profiles;
      };
    # A name among the known profiles denotes a profile; every other
    # name denotes a feature. The same test classifies both a source
    # (which role-bucket it declares under) and each of its targets.
    isProfile = name: builtins.elem name knownProfiles;
    # An entry applies when it is a bare name (an unconditional edge)
    # or a record whose "supportedPlatforms" list contains the host's
    # platform.
    edgeApplies = entry:
      builtins.isString entry
      || (platform != null && builtins.elem platform entry.supportedPlatforms);
    edgeName = entry:
      if builtins.isString entry
      then entry
      else entry.name;
    # Partition one source's applicable targets by role. A target
    # among the known profiles is a profile edge; every other target
    # is a feature edge—an unknown target falls here so that
    # "expandClosure" reports it as a dangling feature edge. A feature
    # naming a profile also falls under "profiles"; the feature-to-
    # profile rule is judged not here but by an assertion in
    # "modules/_assertions.nix" against the raw edge registry, so such
    # an edge is inert in this graph. The "profiles" key appears only
    # when the source names a profile target, so a feature source that
    # names none carries no "profiles" field.
    targetsFor = entries: let
      names = map edgeName (builtins.filter edgeApplies entries);
      profileTargets = lib.unique (builtins.filter isProfile names);
      featureTargets = lib.unique (builtins.filter (n: !(isProfile n)) names);
    in
      {features = featureTargets;}
      // lib.optionalAttrs (profileTargets != []) {profiles = profileTargets;};
    # Group the sources under the role of the source itself, so that
    # a profile's edges sit under "profiles" and a feature's under
    # "features".
    edgesForRole = wantProfiles:
      lib.listToAttrs (
        map (source: {
          name = source;
          value = targetsFor impliedEdges.${source};
        })
        (builtins.filter (source: isProfile source == wantProfiles) (builtins.attrNames impliedEdges))
      );
    # The "all" profile brings along every other profile; this
    # function computes its targets rather than reading a declared
    # edge (see this function's header).
    allEntry = {
      profiles = lib.subtractLists ["all"] knownProfiles;
      features = [];
    };
    profileEntries = edgesForRole true // {all = allEntry;};
  in {
    profiles = lib.mapAttrs (_name: filterEntryProfiles) profileEntries;
    features = edgesForRole false;
  };

  # Role-parametric transitive closure over the typed implication
  # graph. Takes the record produced by the "implicationsFor"
  # function, a "knownByRole" attrset naming the identifiers each
  # role advertises (keyed by role name, value is a list of known
  # names), and the selected names "{ profiles = [...]; features =
  # [...]; }" (or any other roles the graph advertises). Returns a
  # record of the same form with each list fully expanded along
  # the implied edges.
  #
  # The implementation iterates over "builtins.attrNames
  # implications" rather than hard-coding role names, so adding a
  # new role is a pure data-level change.
  #
  # One static check runs before the transitive-closure walk: every
  # edge target named in "implications.<role>.<source>" under a
  # "<targetRole>" key must appear in "knownByRole.<targetRole>".
  # Dangling edges (typically typos) are rejected with a message that
  # names the edge.
  #
  # This function does not judge kind legality — that a feature may
  # not imply a profile, most relevantly. Kind-crossing is
  # platform-independent, so the assertions in
  # "modules/_assertions.nix" judge it against the unfiltered raw edge
  # registry; judging it against this per-host, platform-filtered
  # graph would let an edge hidden behind a "supportedPlatforms"
  # record for another platform escape. A feature-source "profiles"
  # edge is therefore inert here rather than an error.
  expandClosure = implications: knownByRole: selected: let
    roles = builtins.attrNames implications;
    # "Profiles" from "profiles", "Features" from "features", etc.
    # Used only in dangling-edge error messages so that the option
    # name "flake.known<TargetRole>" reads naturally.
    capitalize = s: let
      head = builtins.substring 0 1 s;
      tail = builtins.substring 1 (builtins.stringLength s) s;
    in
      lib.toUpper head + tail;
    # Walk every declared edge and collect errors. Each error is a
    # pre-formatted string; the first, if any, is thrown below.
    edgeErrors =
      lib.concatMap (
        role: let
          edgeRecord = implications.${role};
          sources = builtins.attrNames edgeRecord;
        in
          lib.concatMap (
            source: let
              targetsByRole = edgeRecord.${source};
              targetRoles = builtins.attrNames targetsByRole;
              danglingErrors =
                lib.concatMap (
                  targetRole: let
                    known = knownByRole.${targetRole} or [];
                    targets = targetsByRole.${targetRole};
                    missing = lib.filter (t: !(builtins.elem t known)) targets;
                  in
                    map (t: ''
                      implicationsFor: dangling edge ${role}.${source} -> ${targetRole}.${t} (no such ${targetRole} is known via "flake.known${capitalize targetRole}")
                    '')
                    missing
                )
                targetRoles;
            in
              danglingErrors
          )
          sources
      )
      roles;
    _edgeCheck =
      if edgeErrors != []
      then throw (lib.head edgeErrors)
      else null;
    # Map each role to a single-character key prefix used inside
    # "builtins.genericClosure". Two roles must not share a prefix.
    rolePrefix = role: builtins.substring 0 1 role;
    prefixes = map rolePrefix roles;
    _prefixCollision =
      if lib.length (lib.unique prefixes) != lib.length prefixes
      then
        throw ''
          Implication role names must have distinct first characters;
          got: ${lib.concatStringsSep ", " roles}.
        ''
      else null;
    mkKey = role: name: "${rolePrefix role}:${name}";
    # Parse a "<prefix>:<name>" key back into {role, name} by
    # matching the prefix to one of the known roles.
    roleByPrefix = lib.listToAttrs (
      map (r: {
        name = rolePrefix r;
        value = r;
      })
      roles
    );
    parseKey = key: let
      prefix = builtins.substring 0 1 key;
      rest = builtins.substring 2 (builtins.stringLength key) key;
    in {
      role = roleByPrefix.${prefix};
      name = rest;
    };
    # Toposort over every (role, name) pair the table mentions, so
    # that cycles anywhere in the graph are rejected.
    mentionedPairs =
      lib.concatMap (
        role: let
          edges = implications.${role};
          declaredHere = lib.attrNames edges;
          reachedHere = lib.concatMap (
            targets:
              lib.concatMap (
                targetRole: let
                  names = targets.${targetRole} or [];
                in
                  map (n: {
                    role = targetRole;
                    name = n;
                  })
                  names
              )
              roles
          ) (lib.attrValues edges);
        in
          map (n: {
            inherit role;
            name = n;
          })
          declaredHere
          ++ reachedHere
      )
      roles;
    mentionedKeys = lib.unique (
      map (
        {
          role,
          name,
        }:
          mkKey role name
      )
      mentionedPairs
    );
    edgesFrom = key: let
      p = parseKey key;
      targets = implications.${p.role}.${p.name} or {};
    in
      lib.concatMap (targetRole: map (n: mkKey targetRole n) (targets.${targetRole} or [])) roles;
    sorted = lib.lists.toposort (a: b: builtins.elem b (edgesFrom a)) mentionedKeys;
    # Build the start set by mapping each role's selected names
    # through "mkKey". Roles missing from "selected" default to [].
    startSet = lib.concatMap (role: map (n: {key = mkKey role n;}) (selected.${role} or [])) roles;
    closure = builtins.genericClosure {
      inherit startSet;
      operator = {key, ...}: map (k: {key = k;}) (edgesFrom key);
    };
    # Project the closure back into a per-role record of name lists.
    emptyByRole = lib.listToAttrs (
      map (r: {
        name = r;
        value = [];
      })
      roles
    );
    grouped =
      lib.foldl' (
        acc: entry: let
          p = parseKey entry.key;
        in
          acc
          // {
            ${p.role} = (acc.${p.role} or []) ++ [p.name];
          }
      )
      emptyByRole
      closure;
  in
    if sorted ? cycle
    then
      lib.seq _prefixCollision (
        lib.seq _edgeCheck (throw ''
          The implication graph contains a cycle involving: ${lib.concatStringsSep ", " sorted.loops}.
          The implication graph must form a DAG across all roles.
        '')
      )
    else lib.seq _prefixCollision (lib.seq _edgeCheck grouped);

  # Format a list of names as a quoted, comma-separated English
  # enumeration with a serial comma: one name renders as "a", two as
  # "a" and "b", and three or more as "a", "b", and "c".
  enumerateNames = names: let
    quoted = map (n: "\"${n}\"") names;
    count = lib.length quoted;
  in
    if count == 1
    then lib.head quoted
    else if count == 2
    then "${lib.head quoted} and ${lib.last quoted}"
    else "${lib.concatStringsSep ", " (lib.init quoted)}, and ${lib.last quoted}";

  # Expand the selected names to their full activation: everything the
  # implication graph brings along, plus every contingent feature
  # whose preconditions the result meets.
  #
  # "preconditions" maps each contingent feature's name to its list of
  # precondition entries, all of which must be satisfied for it to
  # activate. A bare-name entry is satisfied when that name is active;
  # a group entry "{ anyOf = [...]; }" is satisfied when at least one
  # member is active. Preconditions have no pulling power: a listed
  # name never becomes active by being named as a precondition.
  # Exclusion trumps
  # activation twice over: a contingent feature named in
  # "excludeFeatures" never activates even when its preconditions hold
  # (its entry is dropped from the table here), and one whose
  # precondition is excluded never activates because the pruned walk
  # can never bring that precondition into effect (the caller prunes
  # the implication graph and filters "selected" as usual).
  #
  # The walk is a fixpoint: each pass runs the "expandClosure"
  # function over the implication graph, then activates every
  # not-yet-active contingent feature whose preconditions are all
  # present, feeding the grown set back until no further feature
  # activates. The set of known names is finite and each pass only
  # adds names, so this terminates.
  #
  # Before the fixpoint runs, precondition cycles among contingent
  # features are rejected with a hard error. A member of such a cycle
  # could never activate at all — contingent features activate only
  # through their preconditions, and hosts may not select them — so
  # the definitions themselves are invalid. Only bare-name edges
  # between contingent features can close a cycle (an edge to a
  # non-contingent feature or an interest terminates there, and a
  # group's members are non-contingent by rule, so a group contributes
  # no cycle edge), so the check walks the bare-name precondition
  # edges alone: a contingent feature lies on a cycle exactly when it
  # can reach itself through them. The full table is checked, before
  # the exclusion filtering above: excluding a cycle member hides a
  # symptom, not the defect.
  #
  # A second preamble check rejects a contingent feature as the SOURCE
  # of an implied edge. The assertion in the "modules/_assertions.nix"
  # file already forbids an implied edge that TARGETS a contingent
  # feature; this is the mirror rule. Were a contingent feature to
  # carry outgoing edges, its activation would activate further
  # features, and every activation must be explainable by selection or
  # a precondition alone.
  expandActivation = {
    implications,
    knownByRole,
    preconditions,
    excludeFeatures ? [],
    selected,
  }: let
    contingentNames = builtins.attrNames preconditions;
    # Only bare-name entries contribute cycle edges: a group's members
    # are non-contingent by rule (an assertion in
    # "modules/_assertions.nix" enforces it), so a group can never
    # close a precondition cycle. Skip the group entries, then keep
    # the bare names that name other contingent features.
    preconditionEdgesFrom = name:
      builtins.filter (r: preconditions ? ${r}) (
        builtins.filter builtins.isString preconditions.${name}
      );
    reachableFrom = name:
      map (entry: entry.key) (builtins.genericClosure {
        startSet = map (k: {key = k;}) (preconditionEdgesFrom name);
        operator = {key}: map (k: {key = k;}) (preconditionEdgesFrom key);
      });
    # Members of precondition cycles: contingent features that can
    # reach themselves through precondition edges.
    cyclicNames =
      builtins.filter (name: builtins.elem name (reachableFrom name))
      (lib.sort lib.lessThan contingentNames);
    # Group the members by cycle: two members belong to the same
    # cycle exactly when each can reach the other. Each group
    # keeps the sorted order of "cyclicNames".
    cycleGroups =
      lib.foldl' (
        groups: name:
          if lib.any (group: builtins.elem name group) groups
          then groups
          else
            groups
            ++ [
              (builtins.filter (
                  member:
                    builtins.elem member (reachableFrom name)
                    && builtins.elem name (reachableFrom member)
                )
                cyclicNames)
            ]
      ) []
      cyclicNames;
    # One ruled sentence per cycle: a one-member cycle names the
    # feature that names itself as a precondition, and a larger cycle
    # enumerates its members.
    describeCycle = group:
      if lib.length group == 1
      then "The feature \"${lib.head group}\" names itself as a precondition, which cannot be satisfied."
      else "The following features name one another as preconditions in a cycle that cannot be satisfied: ${enumerateNames group}.";
    _cycleCheck =
      if cyclicNames != []
      then
        throw (
          "expandActivation: "
          + lib.concatMapStringsSep "\n" describeCycle cycleGroups
        )
      else null;
    # Contingent features that appear as sources of implied edges;
    # see the mirror-rule paragraph in this function's header
    # comment.
    contingentSources = builtins.filter (name: preconditions ? ${name}) (
      lib.unique (
        lib.concatMap (role: builtins.attrNames implications.${role}) (builtins.attrNames implications)
      )
    );
    _contingentSourceCheck =
      if contingentSources != []
      then throw ''expandActivation: the implication graph gives the contingent feature(s) ${enumerateNames contingentSources} outgoing edges, but a contingent feature activates automatically exactly when all of its preconditions are met and may not activate other features. Express each relationship as a precondition instead.''
      else null;
    activatable =
      lib.filterAttrs (name: _: !(builtins.elem name excludeFeatures)) preconditions;
    # One precondition entry against the currently-active names: a
    # bare name is satisfied when it is active; a group is satisfied
    # when at least one member is active. A feature activates when
    # every one of its entries is satisfied. Testing membership only
    # adds names, so the fixpoint stays monotone.
    entrySatisfied = active: entry:
      if builtins.isString entry
      then builtins.elem entry active
      else lib.any (m: builtins.elem m active) entry.anyOf;
    step = current: let
      expanded = expandClosure implications knownByRole current;
      newlyActive = builtins.attrNames (
        lib.filterAttrs (
          name: needed:
            !(builtins.elem name expanded.features)
            && lib.all (entrySatisfied expanded.features) needed
        )
        activatable
      );
    in
      if newlyActive == []
      then expanded
      else
        step (expanded
          // {
            features = expanded.features ++ newlyActive;
          });
  in
    lib.seq _cycleCheck (lib.seq _contingentSourceCheck (step selected));

  # Delete a set of vertices from the implication graph. Excluded
  # vertices lose their out-edges (their adjacency entries are
  # removed) and their in-edges (every surviving adjacency list is
  # filtered to drop them as targets). The result is a record of
  # the same form, suitable for passing to the "expandClosure"
  # function.
  #
  # Excluding a profile (or feature) is therefore equivalent to
  # deleting that vertex from the DAG: anything reachable only
  # through the excluded vertex falls out of the transitive closure
  # automatically, without the consumer having to enumerate the
  # downstream vertices in "excludeFeatures".
  pruneImplications = implications: excluded: let
    isExcluded = role: name: builtins.elem name (excluded.${role} or []);
    withoutExcludedSources =
      lib.mapAttrs (
        role: edges: lib.filterAttrs (name: _: !(isExcluded role name)) edges
      )
      implications;
    pruneTargets = targetsByRole:
      lib.mapAttrs (
        targetRole: targetNames: lib.filter (n: !(isExcluded targetRole n)) targetNames
      )
      targetsByRole;
  in
    lib.mapAttrs (_role: edges: lib.mapAttrs (_source: pruneTargets) edges) withoutExcludedSources;

  # Resolve a host's activation in one step: delete the excluded
  # names from the implication graph (see the "pruneImplications"
  # function above), drop them from the selected names, and run the
  # activation fixpoint on what remains. "excluded" follows the
  # exclusion-record form "{ profiles = [...]; features = [...];
  # }"; a role it omits keeps all of its names.
  resolveActivation = {
    implications,
    knownByRole,
    preconditions,
    selected,
    excluded ? {},
  }: let
    remainingSelected =
      lib.mapAttrs (role: names: lib.subtractLists (excluded.${role} or []) names)
      selected;
  in
    expandActivation {
      implications = pruneImplications implications excluded;
      inherit knownByRole preconditions;
      excludeFeatures = excluded.features or [];
      selected = remainingSelected;
    };
in {
  inherit
    expandActivation
    expandClosure
    implicationsFor
    pruneImplications
    resolveActivation
    ;
}

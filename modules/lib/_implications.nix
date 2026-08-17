{lib}: let
  # Assemble the per-host implication graph from the co-located
  # "implies" declarations that each feature and interest makes. An
  # implied edge is a property of its source, so it lives in the
  # source's own file; the "impliedEdges" argument holds those
  # declarations, keyed by source name, and this function turns them
  # into the graph that the "expandClosure" function consumes: a map
  # from each source name to the names it brings along.
  #
  # Features and interests share this one graph. A feature declares
  # configuration and an interest is a payload-free want, yet both
  # enter one activation walk, so an edge here targets a name without
  # regard to which kind it denotes. The rules that do turn on
  # kind—that a feature may not imply an interest, nor an interest a
  # feature—are the business of the assertions in the
  # "modules/_assertions.nix" file, which judge the raw edge registry.
  #
  # This function runs per-host so that the per-name platform
  # filtering described below can vary with the host's platform.
  #
  # This function filters every target list against the target's
  # declared "supportedPlatforms" list (see the "mkFeature" function):
  # a name the host's platform does not support never arrives through
  # the graph, whether via the "all" feature or via a narrower source
  # such as "desktop". A name with no declaration is available
  # everywhere. The assertion in the "modules/_assertions.nix" file
  # rejects a host whose activation includes an unsupported name
  # anyway (e.g. by selecting it directly).
  #
  # This function includes a record-form edge ("{ name = "<target>";
  # supportedPlatforms = [<systems>]; }") only when the host's
  # platform is one of the listed systems; this is how a source brings
  # along a target on some platforms and not others.
  #
  # This function computes the "all" feature's targets rather than
  # reading a declared edge: "all" targets every feature that has no
  # class body, is not contingent, and no other feature implies. A
  # body-less bundle therefore folds into "all" the moment it is
  # registered, while a feature some bundle already implies stays out,
  # arriving through that bundle instead. A feature that configures
  # something of its own stays out as well, so that "all" covers the
  # body-less bundles alone and a feature with a body is one a host
  # asks for deliberately; the "featureClasses" argument reports which
  # names have a body. A contingent feature stays out because its
  # preconditions are its only way in, and an interest stays out
  # because a feature's edges may target only features—so the
  # computation upholds on its own the two rules that the assertions
  # in "modules/_assertions.nix" police for declared edges, which
  # never see these. Hosts decline what "all" brings along by listing
  # it under "excludeFeatures": declining is an exclusion, not an
  # absence.
  #
  # The graph MUST form a DAG. This function rejects a name that
  # implies itself, and the "expandClosure" function rejects every
  # longer cycle at runtime.
  implicationsFor = {
    # Nixpkgs system string (e.g. "aarch64-darwin"), or null when the
    # host record has none.
    platform ? null,
    # Per-name platform support, keyed by feature name; each value
    # lists the platforms on which that name may activate. A name
    # absent from this record may activate anywhere; a name present in
    # it activates only where the host's platform is one of the listed
    # values, so a null platform errs toward omitting every
    # platform-constrained name.
    supportedPlatforms ? {},
    # Co-located implied edges, keyed by source name; each value is
    # the source's "implies" list, whose entries are bare target names
    # or record-form platform-conditional edges. This function
    # assembles them into the graph below.
    impliedEdges ? {},
    # Every registered feature name, interests excluded. The full set
    # the "all" feature draws its computed targets from, so a name
    # missing here never arrives at a host through "all".
    knownFeatures ? [],
    # The module classes that register a body for each feature, keyed
    # by feature name and holding only the names that have one. Read
    # here only to recognize which features configure something, since
    # the "all" feature targets the body-less bundles alone; a name
    # absent from this record has no body and so qualifies.
    featureClasses ? {},
    # Per-feature preconditions, keyed by contingent feature name.
    # Read here only to recognize which names are contingent, since
    # the "all" feature may not target one; the activation fixpoint in
    # the "expandActivation" function reads the entries themselves.
    preconditions ? {},
    ...
  }: let
    availableOnHost = name: let
      supported = supportedPlatforms.${name} or null;
    in
      supported == null || (platform != null && builtins.elem platform supported);
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
    # One source's applicable targets, each of them supported on this
    # host. An unknown target falls here too, so that the
    # "expandClosure" function reports it as a dangling edge.
    targetsFor = entries:
      builtins.filter availableOnHost (
        lib.unique (map edgeName (builtins.filter edgeApplies entries))
      );
    # The name of the aggregate whose targets this function computes.
    # Bound once so that the two tests below—excluding it from its own
    # targets, and excluding its own declarations from the "implied
    # elsewhere" reading—denote the same feature.
    aggregateName = "all";
    # A bundle: a registered feature that declares no body of its own
    # and is not contingent. This one predicate decides both which
    # names the aggregate can cover and which sources count against
    # them, so the two readings cannot drift apart. Membership in the
    # "knownFeatures" list is part of it because an interest may be a
    # source too, and an interest is not a bundle.
    isBundle = name:
      (name != aggregateName)
      && builtins.elem name knownFeatures
      && !(featureClasses ? ${name})
      && !(preconditions ? ${name});
    # Every name another bundle brings along, read from the raw
    # declarations before platform filtering. Only bundle sources
    # count. A feature with a body of its own never arrives through
    # the aggregate, so counting it as a source would leave its target
    # with no way to arrive at all: the aggregate would drop the
    # target, and nothing would stand in its place. Reading the raw
    # declarations keeps membership the same on every platform, since
    # a target that only a record-form edge for another platform names
    # still counts, so a host's platform decides which of the
    # aggregate's targets survive, never which names it holds.
    impliedByBundle = lib.unique (
      lib.concatMap (source: map edgeName impliedEdges.${source}) (
        builtins.filter isBundle (builtins.attrNames impliedEdges)
      )
    );
    # The aggregate's computed targets, in the registration order of
    # the "knownFeatures" list: the bundles no other bundle brings
    # along, which are the roots of the graph induced on the bundles.
    # Every other bundle arrives through one of those roots, since
    # following the chain of sources upward ends at one in an acyclic
    # graph, so every bundle can arrive. A contingent feature stays
    # out because nobody may select one; it activates from its
    # preconditions instead. Any "implies" list the aggregate itself
    # declares is discarded by the override below, since this
    # computation is the sole authority on what the "all" feature
    # brings along.
    aggregateTargets =
      builtins.filter (
        name: isBundle name && !(builtins.elem name impliedByBundle)
      )
      knownFeatures;
    # A name that implies itself. The "lib.lists.toposort" call in the
    # "expandClosure" function cannot see this one: it asks whether one
    # name precedes another, and never asks that of a name against
    # itself. This test reads the raw declarations rather than the
    # graph below for the reason the dangling-edge comment gives—a
    # name implying itself is illegal on every platform, so an edge
    # hidden behind a "supportedPlatforms" record for a platform this
    # host does not run must not escape it. The kind stays out of the
    # sentence because this function holds no interest registry, and
    # an interest may imply itself just as a feature may.
    selfImplying =
      builtins.filter (
        source: builtins.elem source (map edgeName impliedEdges.${source})
      )
      (builtins.attrNames impliedEdges);
    _selfEdgeCheck =
      if selfImplying != []
      then
        throw (
          lib.concatMapStringsSep "\n" (
            name: "The name \"${name}\" implies itself, so the implication graph is not a DAG."
          )
          selfImplying
        )
      else null;
  in
    lib.seq _selfEdgeCheck (
      lib.mapAttrs (_source: targetsFor) (impliedEdges // {${aggregateName} = aggregateTargets;})
    );

  # Transitive closure over the implication graph. Takes the graph the
  # "implicationsFor" function produces, the list of every name the
  # flake advertises, and the list of selected names. Returns those
  # selected names expanded along the implied edges, in the order the
  # walk visits them.
  #
  # One static check runs before the transitive-closure walk: every
  # edge target listed under the "implications.<source>" entry must
  # appear among the known names. Dangling edges (typically typos) are
  # rejected with a message that specifies the edge.
  #
  # This function does not judge kind legality — that a feature may
  # not imply an interest, most relevantly. Kind-crossing is
  # platform-independent, so the assertions in the
  # "modules/_assertions.nix" file judge it against the unfiltered raw
  # edge registry; judging it against this per-host, platform-filtered
  # graph would let an edge hidden behind a "supportedPlatforms"
  # record for another platform escape. A kind-crossing edge is
  # therefore inert here rather than an error.
  expandClosure = implications: known: selected: let
    # Walk every declared edge and collect errors. Each error is a
    # pre-formatted string; the first, if any, is thrown below.
    edgeErrors = lib.concatMap (
      source:
        map (
          target: ''
            implicationsFor: dangling edge ${source} -> ${target} (no such feature or interest is registered under the "dotfiles.knownFeatures" and "dotfiles.knownInterests" options, which the "mkFeature" and "mkInterest" functions populate)
          ''
        ) (lib.filter (target: !(builtins.elem target known)) implications.${source})
    ) (builtins.attrNames implications);
    _edgeCheck =
      if edgeErrors != []
      then throw (lib.head edgeErrors)
      else null;
    edgesFrom = name: implications.${name} or [];
    # Toposort over every name the graph mentions, as a source or as a
    # target, so that a cycle anywhere in it is rejected.
    mentionedNames = lib.unique (
      builtins.attrNames implications ++ lib.concatLists (builtins.attrValues implications)
    );
    sorted = lib.lists.toposort (a: b: builtins.elem b (edgesFrom a)) mentionedNames;
    closure = builtins.genericClosure {
      startSet = map (name: {key = name;}) selected;
      operator = {key, ...}: map (name: {key = name;}) (edgesFrom key);
    };
  in
    if sorted ? cycle
    then
      lib.seq _edgeCheck (throw ''
        The implication graph contains a cycle involving: ${lib.concatStringsSep ", " sorted.loops}.
        The implication graph must form a DAG.
      '')
    else lib.seq _edgeCheck (map (entry: entry.key) closure);

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
  # name never becomes active by appearing in a precondition.
  # Exclusion trumps activation twice over: a contingent feature
  # listed in "excludeFeatures" never activates even when its
  # preconditions hold (its entry is dropped from the table here), and
  # one whose precondition is excluded never activates because the
  # pruned walk can never bring that precondition into effect (the
  # caller prunes the implication graph and filters "selected" as
  # usual).
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
  # declare outgoing edges, its activation would activate further
  # features, and every activation must be explainable by selection or
  # a precondition alone.
  expandActivation = {
    implications,
    known,
    preconditions,
    excludeFeatures ? [],
    platform ? null,
    supportedPlatforms ? {},
    selected,
  }: let
    contingentNames = builtins.attrNames preconditions;
    # Only bare-name entries contribute cycle edges: a group's members
    # are non-contingent by rule (an assertion in
    # "modules/_assertions.nix" enforces it), so a group can never
    # close a precondition cycle. Skip the group entries, then keep
    # the bare names that refer to other contingent features.
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
    # Group the members by cycle: two members belong to the same cycle
    # exactly when each can reach the other. Each group keeps the
    # sorted order of "cyclicNames".
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
    # One ruled sentence per cycle: a one-member cycle identifies the
    # feature that lists itself as a precondition, and a larger cycle
    # enumerates its members.
    describeCycle = group:
      if lib.length group == 1
      then "The feature \"${lib.head group}\" lists itself as a precondition, which cannot be satisfied."
      else "The following features list one another as preconditions in a cycle that cannot be satisfied: ${enumerateNames group}.";
    _cycleCheck =
      if cyclicNames != []
      then
        throw (
          "expandActivation: "
          + lib.concatMapStringsSep "\n" describeCycle cycleGroups
        )
      else null;
    # Contingent features that appear as sources of implied edges; see
    # the mirror-rule paragraph in this function's header comment.
    contingentSources =
      builtins.filter (name: preconditions ? ${name}) (builtins.attrNames implications);
    _contingentSourceCheck =
      if contingentSources != []
      then throw ''expandActivation: the implication graph gives the contingent feature(s) ${enumerateNames contingentSources} outgoing edges, but a contingent feature activates automatically exactly when all of its preconditions are met and may not activate other features. Express each relationship as a precondition instead.''
      else null;
    # A contingent feature activates only where the host's platform
    # supports it. Support is tested here as well as on implied edges,
    # because a contingent feature is never the target of one: its
    # preconditions are its whole way into effect, so a platform
    # declaration it makes would otherwise go unread and the feature
    # would activate where it cannot work. A name declaring no
    # platforms activates anywhere. A name that declares them needs
    # the host's platform among them, so an undetected platform
    # withholds every such name, matching how the "implicationsFor"
    # function treats an edge target.
    supportedHere = name: let
      supported = supportedPlatforms.${name} or null;
    in
      supported == null || (platform != null && builtins.elem platform supported);
    activatable =
      lib.filterAttrs (
        name: _: !(builtins.elem name excludeFeatures) && supportedHere name
      )
      preconditions;
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
      expanded = expandClosure implications known current;
      newlyActive = builtins.attrNames (
        lib.filterAttrs (
          name: needed:
            !(builtins.elem name expanded)
            && lib.all (entrySatisfied expanded) needed
        )
        activatable
      );
    in
      if newlyActive == []
      then expanded
      else step (expanded ++ newlyActive);
  in
    lib.seq _cycleCheck (lib.seq _contingentSourceCheck (step selected));

  # Delete a set of vertices from the implication graph. Excluded
  # vertices lose their out-edges (their adjacency entries are
  # removed) and their in-edges (every surviving adjacency list is
  # filtered to drop them as targets). The result is a record of the
  # same form, suitable for passing to the "expandClosure" function.
  #
  # Excluding a feature is therefore equivalent to deleting that
  # vertex from the DAG: anything reachable only through the excluded
  # vertex falls out of the transitive closure automatically, without
  # the consumer having to enumerate the downstream vertices in the
  # "excludeFeatures" list.
  pruneImplications = implications: excluded: let
    isExcluded = name: builtins.elem name excluded;
  in
    lib.mapAttrs (_source: targets: lib.filter (target: !(isExcluded target)) targets) (
      lib.filterAttrs (source: _: !(isExcluded source)) implications
    );

  # Resolve a host's activation in one step: delete the excluded names
  # from the implication graph (see the "pruneImplications" function
  # above), drop them from the selected names, and run the activation
  # fixpoint on what remains.
  resolveActivation = {
    implications,
    known,
    preconditions,
    selected,
    excluded ? [],
    platform ? null,
    supportedPlatforms ? {},
  }:
    expandActivation {
      implications = pruneImplications implications excluded;
      inherit known platform preconditions supportedPlatforms;
      excludeFeatures = excluded;
      selected = lib.subtractLists excluded selected;
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

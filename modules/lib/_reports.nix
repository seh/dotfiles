# The latent-feature report: which contingent features are inactive
# for each evaluator — the machine's own, or one managed or standalone
# user's — and why.
#
# The leading underscore in the filename excludes this file from the
# "import-tree" call in the "../../flake.nix" file; the
# "./_assembly.nix" file imports it.
{lib}: let
  # The selection list a person writes a name under, by kind. A
  # contingent feature belongs on neither list, and draws null below.
  optionForKind = {
    feature = "features";
    interest = "interests";
  };

  # The findings for one evaluator, in feature-name order: one per
  # contingent feature that the "partlyMet" predicate below admits,
  # plus one per feature something no selection moves stands over,
  # which enters the report on that obstacle alone whatever the
  # predicate makes of its preconditions.
  #
  # The verdict on each precondition entry comes from the "missing"
  # list that the evaluator's own "dotfiles._host.latentFeatures"
  # record already holds, so this report and the activation walk agree
  # by construction. Each candidate's "inEffect" flag reads that same
  # evaluator's own predicate, which is what tells the reader of a
  # satisfied group which alternative satisfied it.
  findingsFor = {
    class,
    contingentFeatures,
    featureClasses,
    inEffect,
    knownFeatures,
    knownInterests,
    latentFeatures,
    managed,
    obstructedNames,
    platform,
    supportedPlatforms,
  }: let
    # The registries as sets, so each candidate's kind and contingency
    # tests cost a lookup rather than a scan of a whole registry.
    featureSet = lib.genAttrs knownFeatures (_: null);
    interestSet = lib.genAttrs knownInterests (_: null);
    # One name that would satisfy a precondition entry, described so
    # that a reader can act on it. A contingent name, a name nothing
    # advertises, and a name only the machine may select go under no
    # list here, and each draws a null "option" field.
    candidateFor = inGroup: name: let
      kind =
        if interestSet ? ${name}
        then "interest"
        else if featureSet ? ${name}
        then "feature"
        else "unknown";
      # What stands over this candidate for this evaluator, or null
      # where nothing does. A reader is meant to act on a candidate by
      # selecting it, so a candidate whose selection here cannot pay
      # off must say so. The tests run most immovable first, the same
      # precedence the one-obstacle finding forms apply. A machine's
      # exclusion is absent on purpose, because a managed user
      # countermands one by selecting the name, which is exactly the
      # advice the finding gives; that countermand does not exist for
      # a name only the machine may select, nor for a contingent name,
      # which nobody may select, so there a standing exclusion counts
      # and reads "excluded".
      declaredPlatforms = supportedPlatforms.${name} or null;
      # A managed user may not act on a candidate whose every
      # registered body configures the machine: that selection is
      # refused, and only the machine's own applies it. A standalone
      # home configuration writes the machine's own selection list,
      # where the same selection is accepted, so the mark belongs to
      # managed users alone.
      bodyClasses = featureClasses.${name} or [];
      machineOnly =
        managed
        && bodyClasses != []
        && !(builtins.elem "homeManager" bodyClasses);
      # The list of an obstruction pair that can hold this candidate.
      # The walk holds each authoring list to its own kind, so only
      # the kind-matching list is consulted: a candidate's name in the
      # other kind's list prunes nothing and so stands over nothing. A
      # name no registry advertises sits in neither.
      listFor = pair: pair.${kind} or [];
      obstruction =
        if
          declaredPlatforms
          != null
          && !(platform != null && builtins.elem platform declaredPlatforms)
        then "unsupported"
        else if builtins.elem name (listFor obstructedNames.forbidden)
        then "forbidden"
        else if builtins.elem name (listFor obstructedNames.ownExclusions)
        then "excluded"
        else if
          (machineOnly || contingent)
          && builtins.elem name (listFor obstructedNames.hostExclusions)
        then "excluded"
        else if machineOnly
        then "machine-only"
        else null;
      contingent = contingentFeatures ? ${name};
      # A contingent name may not sit in a group—the
      # "anyOfMemberContingentAssertion" assertion refuses it—so a
      # group member that is one marks the defect. The verdict still
      # follows the member's own record, because either fixed answer
      # reads wrong: an open claim when every way to bring the member
      # into effect is closed, or a closed one when a way stays open.
      misplaced = contingent && inGroup;
    in {
      inherit contingent kind misplaced name obstruction;
      inEffect = inEffect name;
      option =
        if contingent || kind == "unknown" || obstruction == "machine-only"
        then null
        else optionForKind.${kind};
      # Whether this candidate's own record closes every way to bring
      # it into effect, so the line rendered for it says why the entry
      # citing it is unsatisfiable.
      unreachable = contingent && !(contingentReachable name);
    };
    # Whether a candidate leaves open a way to satisfy the entry
    # citing it. An obstruction closes it, except "machine-only",
    # which awaits a selection the machine may still make. A name
    # nothing registers closes it, since no selection list accepts it
    # and the "preconditionUnknownAssertion" assertion refuses it. A
    # contingent candidate closes it when its own record leaves it
    # unreachable.
    candidateOpen = candidate:
      (candidate.obstruction == null || candidate.obstruction == "machine-only")
      && candidate.kind != "unknown"
      && !candidate.unreachable;
    # Whether a contingent feature could still come into effect for
    # this evaluator: it is active already, or every precondition it
    # declares remains satisfiable.
    contingentReachable = name:
      !(latentFeatures ? ${name}) || reachableByName.${name};
    # One precondition entry beside the verdict on it and the
    # candidates that would satisfy it. The entry arrives already
    # normalized by the "preconditionSet" type, so a group's members
    # stand—and render—sorted, not in declaration order. A group
    # yields one candidate per alternative and a bare name exactly
    # one, and the "disjunctive" flag tells the two apart where the
    # candidate count cannot: a single-member group is legal. An entry
    # is satisfiable while it is met or some candidate leaves a way
    # open.
    entryFor = missing: entry: let
      disjunctive = !(builtins.isString entry);
      met = !(builtins.elem entry missing);
      satisfiedBy = map (candidateFor disjunctive) (
        if disjunctive
        then entry.anyOf
        else [entry]
      );
    in {
      inherit disjunctive entry met satisfiedBy;
      satisfiable = met || lib.any candidateOpen satisfiedBy;
    };
    # One verdict per contingent feature per evaluator, in a lazy
    # table keyed by name, so however many precondition entries cite a
    # feature, its verdict is computed once. The first force still
    # descends a precondition chain link by link, so the evaluator's
    # call-depth limit bounds every case: a cycle, or a legal chain
    # deep enough to exhaust that limit, ends evaluation with its
    # error rather than an answer. Every constructor-built
    # configuration refuses a cycle of bare edges earlier, in the
    # activation walk these records force. A group may not cite a
    # contingent name, so a cycle closed through a group marks an
    # invalid configuration; it enters this recursion from a system
    # configuration or a bare instantiation, while a standalone home
    # configuration forces its assertions on any read and the
    # group-member assertion fires first.
    reachableByName =
      lib.mapAttrs (
        _name: record:
          lib.all (entry: (entryFor record.missing entry).satisfiable)
          record.preconditions
      )
      latentFeatures;
    # A feature with an unsatisfiable precondition cannot come into
    # effect at all, however the rest of its preconditions fare.
    findingFor = feature: record: let
      preconditions = map (entryFor record.missing) record.preconditions;
      metCount = lib.count (entry: entry.met) preconditions;
    in {
      inherit feature metCount preconditions;
      inherit (record) excludedBy forbidden unsupportedOn;
      unmetCount = builtins.length preconditions - metCount;
      unreachable = lib.any (entry: !entry.satisfiable) preconditions;
    };
    # A group counts once, on whichever side its own verdict falls,
    # because an entry is the unit of obligation: a group offering
    # three alternatives is one obligation with three ways to
    # discharge it, not three obligations. Counting its members
    # instead would place a feature one selection away far from
    # satisfaction.
    #
    # For a feature declaring N preconditions, N at least one since
    # the "preconditionSet" type rejects an empty list, partly met
    # means at most N-1 met and at least "min (N - 1) 1" met.
    #
    # The upper bound keeps out a feature every precondition of which
    # holds, because its preconditions have nothing left to say about
    # it. Such a feature is inactive for some other standing obstacle,
    # which this record states in a field of its own and the filter
    # beside this test admits on its own account.
    #
    # The lower bound is the signal of an effort under way: one
    # precondition already met. It relaxes to zero at N of one, where
    # a feature cannot exhibit partial progress at all. A single
    # precondition is either met, activating the feature and keeping
    # it out of the "latentFeatures" record altogether, or unmet, and
    # demanding progress of it would hide it forever rather than leave
    # it dormant.
    partlyMet = finding: let
      total = finding.metCount + finding.unmetCount;
      atMost = total - 1;
      atLeast = lib.min atMost 1;
    in
      finding.metCount <= atMost && finding.metCount >= atLeast;
    # A feature enters the report when a further selection could still
    # advance it—some precondition it declares is unsatisfied—or when
    # something no selection moves stands over it. The standing
    # obstacles are separate reasons, so such a feature enters the
    # report whatever the predicate makes of its preconditions, and
    # reports that obstacle alone.
    # A feature this evaluator could never activate, whatever anyone
    # writes here. Each evaluator resolves the bodies of its own class
    # alone, so a machine's evaluator cannot activate a feature that
    # registers a home-manager body and nothing else. Such a feature
    # is every managed user's to act on, and each user's own evaluator
    # reports it against the list that user would edit, so stating it
    # here as well would advise an author who cannot act on the
    # advice. A feature registering no body at all stays, since
    # nothing marks it as belonging to another evaluator.
    unresolvable = finding: let
      bodyClasses = featureClasses.${finding.feature} or [];
    in
      bodyClasses != [] && !(builtins.elem class bodyClasses);
  in
    builtins.filter (
      finding:
        !(unresolvable finding)
        && (
          partlyMet finding
          || finding.forbidden
          || finding.excludedBy != null
          || finding.unsupportedOn != null
          || finding.unreachable
        )
    )
    (lib.mapAttrsToList findingFor latentFeatures);

  # The record for one evaluator: which evaluator it is, the host and
  # platform it resolves for, the option path its selections are
  # written at, and its findings.
  evaluatorFor = {
    class,
    evaluated,
    kind,
    managed,
    selectionPath,
    user,
  }: {
    inherit class kind selectionPath user;
    host = evaluated.dotfiles.host.name;
    platform = evaluated.dotfiles._host.platform;
    withheld = evaluated.dotfiles._host.withheldFeatures;
    findings = findingsFor {
      inherit class;
      contingentFeatures = evaluated.dotfiles._featurePreconditions;
      featureClasses = evaluated.dotfiles._featureClasses;
      knownFeatures = evaluated.dotfiles._knownFeatures;
      knownInterests = evaluated.dotfiles._knownInterests;
      inherit (evaluated.dotfiles._host) inEffect latentFeatures platform;
      inherit managed;
      supportedPlatforms = evaluated.dotfiles._supportedPlatforms;
      # Each obstruction set keeps the two kinds' lists apart, since
      # the walk holds each list to its own kind and a candidate
      # consults only the list matching its own.
      obstructedNames = {
        forbidden = {
          feature = evaluated.dotfiles.host.forbidFeatures;
          interest = evaluated.dotfiles.host.forbidInterests;
        };
        ownExclusions = let
          own = declared: fallback:
            if declared != null
            then declared
            else fallback;
        in {
          feature =
            own evaluated.dotfiles._ownExcludeFeatures
            evaluated.dotfiles.host.excludeFeatures;
          interest =
            own evaluated.dotfiles._ownExcludeInterests
            evaluated.dotfiles.host.excludeInterests;
        };
        # The exclusions this evaluator's walk applies, every author's
        # together. Consulted only for a name whose selection this
        # evaluator may not make, where no countermand exists.
        hostExclusions = {
          feature = evaluated.dotfiles.host.excludeFeatures;
          interest = evaluated.dotfiles.host.excludeInterests;
        };
      };
    };
  };

  # One of this flake's constructors builds a configuration from this
  # flake's own modules, which declare the "dotfiles" options this
  # report reads. A configuration built otherwise declares none of
  # them, and yields an empty list rather than halting the report with
  # a missing-attribute error. Reading the "config" attribute costs
  # nothing for a system configuration, whose assertions live in the
  # system it builds; home-manager instead gates its own "config"
  # behind an assertion check, so a foreign home configuration whose
  # assertions fail halts the report with that flake's own complaint
  # before the probe can answer. The probe looks for the computed
  # record that this flake's activation module alone declares, because
  # a bare "dotfiles" namespace is a name any flake might use for its
  # own options. The test sits inside the per-configuration
  # computation, not in a filter over the output set, so that reading
  # one configuration's entry leaves every other configuration in that
  # set unforced.
  evaluatorsWhenBuiltHere = collect: configuration:
    if
      configuration
      ? config
      && configuration.config ? dotfiles
      && configuration.config.dotfiles ? _host
    then collect configuration
    else [];

  # The evaluators a system configuration holds: the machine's own,
  # followed by one for each managed user's nested home evaluator, in
  # username order. The machine resolves its own class's bodies while
  # every user resolves home-manager bodies, so the class travels with
  # each record rather than with the configuration.
  systemEvaluators = class: configuration: let
    inherit (configuration) config;
  in
    [
      (evaluatorFor {
        inherit class;
        evaluated = config;
        kind = "machine";
        managed = false;
        selectionPath = "dotfiles.host";
        user = null;
      })
    ]
    ++ map (
      user:
        evaluatorFor {
          class = "homeManager";
          evaluated = config.home-manager.users.${user};
          kind = "user";
          managed = true;
          selectionPath = "dotfiles.users.${user}";
          inherit user;
        }
    ) (builtins.attrNames config.dotfiles.users);

  # The sole evaluator a home configuration serving one person has:
  # that user's own, whose selections are written at the
  # "dotfiles.host" record because such a configuration has no
  # "dotfiles.users" registry.
  homeEvaluators = configuration: let
    inherit (configuration) config;
  in [
    (evaluatorFor {
      class = "homeManager";
      evaluated = config;
      kind = "user";
      managed = false;
      selectionPath = "dotfiles.host";
      user = config.home.username;
    })
  ];

  # Rendering. Each function below yields its lines unindented and
  # indents what it nests, so no literal begins with whitespace and
  # every level's depth is decided in one place.
  indent = lines: map (line: "  ${line}") lines;

  # The column each stanza's field values begin at: the widest label
  # that kind of stanza allows, plus two spaces. The width belongs to
  # the stanza kind rather than to the fields one instance happens to
  # hold, so a field appearing or disappearing never shifts the values
  # beside it.
  configurationWidth = 11;
  evaluatorWidth = 13;
  # The widest label under a withheld feature, "arrives only through",
  # plus its colon.
  withheldWidth = 21;
  findingWidth = 15;

  # One field: its label, the padding that sets its value at the
  # stanza's column, and the value. A label wider than its stanza's
  # column still gets one space, so an unforeseen label degrades the
  # alignment rather than running into its value.
  field = width: label: value: let
    text = "${label}:";
    padding = lib.max 1 (width - builtins.stringLength text);
  in "${text}${lib.concatStrings (lib.genList (_: " ") padding)}${value}";

  # An attribute path segment as a reader could paste it: bare when
  # the name is an identifier, quoted otherwise, since a home
  # configuration commonly answers to "<user>@<host>". A quoted name
  # has its backslashes, quotation marks, and dollar signs escaped, so
  # that a name holding any of them still pastes back as the one
  # attribute it came from.
  pathSegment = name:
    if builtins.match "[A-Za-z_][A-Za-z0-9_'-]*" name == null
    then ''"${builtins.replaceStrings ["\\" "\"" "$"] ["\\\\" "\\\"" "\\$"] name}"''
    else name;

  # One name a reader could write on a selection list, with its kind
  # in parentheses. A contingent name adds "contingent", since
  # satisfying its own preconditions is the only way to bring it into
  # effect; one sitting illegally in a group adds "misplaced", and one
  # whose own record closes every such way adds "unreachable", so the
  # line justifies the verdict it sits under. A name no module
  # registers reads "unregistered" in place of a kind: the
  # "preconditionUnknownAssertion" assertion in the
  # "modules/_assertions.nix" file already rejects it, so it marks a
  # defect rather than a third kind to choose among.
  candidateLine = candidate: let
    kind =
      if candidate.kind == "unknown"
      then "unregistered"
      else candidate.kind;
    qualifiers =
      lib.optional candidate.contingent "contingent"
      ++ lib.optional candidate.misplaced "misplaced"
      ++ lib.optional (candidate.obstruction != null) candidate.obstruction
      ++ lib.optional candidate.unreachable "unreachable";
    note = builtins.concatStringsSep ", " ([kind] ++ qualifiers);
  in "- ${candidate.name} (${note})";

  # One precondition entry as lines. A group nests its alternatives
  # under "any of" whether or not it is satisfied, so that one
  # obligation occupies one item of its list and the items count up to
  # the total the heading states. A satisfied group nests the names in
  # effect, all of them where several are in effect at once, since
  # nothing marks one rather than another as the one that discharged
  # the obligation; a group still wanted nests every alternative,
  # those being what a reader chooses among. The fallback covers an
  # entry whose candidates all report themselves out of effect, which
  # the records forbid but which would otherwise render a heading with
  # nothing beneath it.
  entryLines = entry: let
    inEffect = builtins.filter (candidate: candidate.inEffect) entry.satisfiedBy;
    members =
      if entry.met && inEffect != []
      then inEffect
      else entry.satisfiedBy;
  in
    if entry.disjunctive
    then ["- any of:"] ++ indent (indent (map candidateLine members))
    else map candidateLine members;

  # Findings in two runs: those a further selection would advance,
  # ordered by how much each still wants and then by name, followed by
  # the obstructed ones, ordered by name alone. The first comparator
  # states a distance, and an obstacle is not a distance: no selection
  # moves a feature past one, and an obstructed finding shows no count
  # for a reader to read an order from. The name breaks every tie
  # because it is the one key no selection changes, which keeps an
  # evaluator's order the same from reading to reading and leaves two
  # renderings of the same report diffable against each other.
  orderFindings = findings: let
    obstructed = finding:
      finding.forbidden
      || finding.excludedBy != null
      || finding.unsupportedOn != null
      || finding.unreachable;
    byName = a: b: a.feature < b.feature;
    byWhatRemains = a: b:
      if a.unmetCount != b.unmetCount
      then a.unmetCount < b.unmetCount
      else a.feature < b.feature;
  in
    lib.sort byWhatRemains (builtins.filter (finding: !(obstructed finding)) findings)
    ++ lib.sort byName (builtins.filter obstructed findings);

  # One finding as lines, headed by the feature's own name. An
  # obstructed feature states its one obstacle and nothing else: while
  # the obstacle stands, satisfying a precondition would not activate
  # the feature, so listing what remains would offer a reader work
  # that does not pay off. Prohibition takes precedence over
  # exclusion, because it is the obstacle the reader cannot clear. A
  # forbidden name renders under the "dotfiles.host" option path for
  # every evaluator, since forbidding is the machine's alone, and an
  # exclusion under the option path of whoever wrote it. The walk
  # holds each list to its own kind, so the list holding a feature
  # back is always "forbidFeatures" or "excludeFeatures", and each
  # line cites it under the right owner. An unreachable feature
  # states that verdict and, beneath it, only the entries no selection
  # can satisfy: their candidates' qualifiers justify the verdict, and
  # the open entries stay unlisted so nothing beneath the heading
  # reads as an invitation. Every other finding states how many
  # preconditions remain out of how many the feature declares, then
  # those already satisfied and those still wanted, with an empty side
  # absent rather than stated as empty.
  findingLines = selectionPath: finding: let
    verdict = met: builtins.filter (entry: entry.met == met) finding.preconditions;
    unmet = verdict false;
    total = builtins.length finding.preconditions;
    group = label: entries:
      lib.optionals (entries != []) (
        ["${label}:"] ++ indent (lib.concatMap entryLines entries)
      );
    excludingList =
      if finding.excludedBy == "machine"
      then "dotfiles.host.excludeFeatures"
      else "${selectionPath}.excludeFeatures";
  in
    ["${finding.feature}:"]
    ++ indent (
      if finding.unsupportedOn != null
      then [
        (field findingWidth "supported on" (
          builtins.concatStringsSep ", " finding.unsupportedOn
        ))
      ]
      else if finding.forbidden
      then [(field findingWidth "forbidden by" "dotfiles.host.forbidFeatures")]
      else if finding.excludedBy != null
      then [(field findingWidth "excluded by" excludingList)]
      else if finding.unreachable
      then
        ["unreachable:"]
        ++ indent (
          lib.concatMap entryLines (
            builtins.filter (entry: !entry.satisfiable) finding.preconditions
          )
        )
      else
        ["preconditions (${toString (builtins.length unmet)}/${toString total}) unmet:"]
        ++ indent (group "met" (verdict true) ++ group "unmet" unmet)
    );

  # One evaluator as lines, headed by which one it is: the machine's
  # own, or the named user's. An evaluator with no finding states its
  # selection path and class alone, since an empty "features" block
  # would say nothing a reader could act on.
  # The withheld features one loss lies beyond, as a reader could scan
  # them: quoted and comma-separated, in the order the record holds.
  enumerate = names: lib.concatMapStringsSep ", " (n: ''"${n}"'') names;

  evaluatorLines = evaluator: let
    heading =
      if evaluator.kind == "machine"
      then "machine"
      else "user ${evaluator.user}";
    ordered = orderFindings evaluator.findings;
    # The features a withholding removed, the ones nobody wrote down
    # first: those are the losses a reader has no other way to learn
    # about, while a feature its author withheld on purpose only
    # confirms what that author already knows.
    withheldNames = builtins.attrNames evaluator.withheld;
    consequences =
      builtins.filter (n: evaluator.withheld.${n}.beyond != []) withheldNames;
    onPurpose =
      builtins.filter (n: evaluator.withheld.${n}.beyond == []) withheldNames;
    withheldLines = name: let
      entry = evaluator.withheld.${name};
      cause =
        if entry.beyond != []
        then field withheldWidth "arrives only through" (enumerate entry.beyond)
        else if entry.forbidden
        # Forbidding is the machine's alone, whichever evaluator reads
        # this, so the path cites the machine. A user has no option of
        # their own to cite.
        then field withheldWidth "forbidden by" "dotfiles.host.forbidFeatures"
        else
          field withheldWidth "excluded by" (
            if entry.excludedBy == "own"
            then "${evaluator.selectionPath}.excludeFeatures"
            else "dotfiles.host.excludeFeatures"
          );
    in
      ["${name}:"] ++ indent [cause];
  in
    ["${heading}:"]
    ++ indent (
      [
        (field evaluatorWidth "selections" evaluator.selectionPath)
        (field evaluatorWidth "class" evaluator.class)
      ]
      ++ lib.optionals (ordered != []) (
        ["features:"]
        ++ indent (lib.concatMap (findingLines evaluator.selectionPath) ordered)
      )
      ++ lib.optionals (withheldNames != []) (
        ["withheld:"]
        ++ indent (lib.concatMap withheldLines (consequences ++ onPurpose))
      )
    );

  # One configuration as lines, headed by its own attribute path.
  # Every evaluator of a configuration shares its host and platform,
  # so the first record supplies both, and a host with no name of its
  # own omits that field rather than standing in a phrase for it. A
  # configuration this flake's constructors did not build has no
  # record to read and yields no lines at all: the only thing the
  # report could say about it is that it knows nothing about it, which
  # is not a finding.
  configurationLines = aliases: entry:
    if entry.resolvedFor == []
    then []
    else let
      first = builtins.head entry.resolvedFor;
    in
      ["${entry.output}.${pathSegment entry.name}:"]
      ++ indent (
        lib.optional (first.host != null) (field configurationWidth "host" first.host)
        ++ lib.optional (first.platform != null) (
          field configurationWidth "platform" first.platform
        )
        ++ lib.optional (aliases != []) (
          field configurationWidth "aka" (
            lib.concatMapStringsSep ", " (name: "${entry.output}.${pathSegment name}") aliases
          )
        )
        ++ ["resolved for:"]
        ++ indent (lib.concatMap evaluatorLines entry.resolvedFor)
      );

  outputNames = [
    "darwinConfigurations"
    "homeConfigurations"
    "nixosConfigurations"
  ];

  # The whole report as text, read from the evaluator records alone,
  # so that a value holding rendered text already renders no
  # differently from one holding records and nothing else. A
  # configuration yielding no lines contributes no block, and a report
  # with no block at all is empty text.
  # The entries one output publishes, with those that describe the
  # same configuration collected under the first of their names. A
  # flake may publish one configuration under several names—nix-darwin
  # looks for one matching the host's name, so a flake often publishes
  # a second under "local"—and the report has nothing further to say
  # about the second name. Entries match when their evaluator records
  # are equal, which covers the host name, the platform, and every
  # finding, so two that match render identically anyway.
  groupedEntries = entries: let
    named = lib.imap0 (index: name: {inherit index name;}) (builtins.attrNames entries);
    sameAs = one: other: entries.${one}.resolvedFor == entries.${other}.resolvedFor;
    firstOf = item: let
      earlier = builtins.filter (other: other.index < item.index && sameAs other.name item.name) named;
    in
      if earlier == []
      then item.name
      else (builtins.head earlier).name;
    leaders = builtins.filter (item: firstOf item == item.name) named;
  in
    map (leader: {
      entry = entries.${leader.name};
      aliases = map (item: item.name) (
        builtins.filter (item: item.name != leader.name && firstOf item == leader.name) named
      );
    })
    leaders;

  reportText = collected: let
    blocks = builtins.filter (block: block != []) (
      lib.concatMap (
        output:
          map (
            group: configurationLines group.aliases group.entry
          ) (groupedEntries (collected.${output} or {}))
      )
      outputNames
    );
  in
    builtins.concatStringsSep "\n\n" (
      map (block: builtins.concatStringsSep "\n" block) blocks
    );
in {
  # The structured latent-feature report over a flake's own
  # configurations, keyed by the flake output each came from and then
  # by configuration name, so that asking after one configuration
  # forces that one alone.
  #
  # Each configuration's entry holds its name, its output, and the
  # evaluators it resolves for. The flake module in the
  # "modules/_latent-feature-report.nix" file grafts a "report"
  # rendering onto each entry and publishes a whole-flake rendering
  # beside them; this value holds no rendered text of its own.
  collectLatentFeatures = {
    darwinConfigurations ? {},
    homeConfigurations ? {},
    nixosConfigurations ? {},
  }: let
    gather = output: evaluatorsOf:
      lib.mapAttrs (name: configuration: {
        inherit name output;
        resolvedFor = evaluatorsWhenBuiltHere evaluatorsOf configuration;
      });
  in {
    darwinConfigurations =
      gather "darwinConfigurations" (systemEvaluators "nixDarwin") darwinConfigurations;
    homeConfigurations = gather "homeConfigurations" homeEvaluators homeConfigurations;
    nixosConfigurations =
      gather "nixosConfigurations" (systemEvaluators "nixOS") nixosConfigurations;
  };

  renderConfiguration = entry: builtins.concatStringsSep "\n" (configurationLines [] entry);

  renderLatentFeatures = reportText;
}

# Helpers that build feature modules with their activation gating
# wired in.
#
# A "feature" file using these helpers takes the form:
#
#   {flakeLib, ...}:
#     flakeLib.mkFeature "<name>" {
#       homeManager = {pkgs, ...}: {
#         home.packages = [pkgs.foo];
#       };
#       nixDarwin = ...;
#       nixOS = ...;
#     }
#
# The "bodies" attrset is keyed by class name as recognized by this
# flake's class aggregators ("homeManager", "nixDarwin", "nixOS").
# That key set is closed: a key matching no class (and not one of the
# constructors' reserved keys) throws at registration rather than
# storing a body that nothing ever reads. Each value is one of two
# forms:
#
#   1. A deferred module returning the *contents* of a "config" block
#      — not a full module with its own "config = {...}" key. The
#      "mkFeature" function inserts the "config = lib.mkIf (inEffect
#      name) (...)" wrapper, so a body that itself wraps in "config =
#      {...}" would produce "config.config = {...}" and silently drop
#      its contributions.
#
#   2. A structured attrset "{options? = ...; config? = ...;}" for
#      features that declare their own options. The "options" half is
#      a normal module function (e.g. "{lib, ...}: {options = ...;}")
#      passed through verbatim — option declarations cannot be
#      conditional. The "config" half follows the same contract as
#      form 1 and gets the same activation gate. Either key may be
#      omitted.
#
# Conditional sub-contributions inside a config body should use
# "lib.mkMerge" / "lib.mkIf" at the value level, not at the module
# level.
#
# An empty bodies attrset ("{}") is allowed and produces a name-only
# registration (under the "knownFeatures" registry) with no module
# contributions. Useful when a name is meaningful only as a
# cross-feature reference, as a bundle whose whole substance is its
# "implies" list does.
{lib}: let
  # Build a deferred module that gates the "body" argument on
  # "host.inEffect name", where "host" is the resolved
  # "config.dotfiles._host" record.
  #
  # The wrapper's outer function destructures every module argument
  # that bodies might rely on, so the module system's argument
  # injection mechanism (driven by "builtins.functionArgs") can fill
  # them in from "_module.args". A body that destructures an argument
  # missing from this list will fail with "called without required
  # argument".
  #
  # Currently covers what the home-manager, nix-darwin, and NixOS
  # class evaluators provide for the features in this repository. If a
  # future feature body needs "inputs", "specialArgs", or any other
  # module argument, add it here as "name ? null".
  wrap = name: body: {
    config,
    lib,
    # deadnix: skip
    pkgs ? null,
    # deadnix: skip
    osConfig ? null,
    ...
  } @ args: {
    config = lib.mkIf (config.dotfiles._host.inEffect name) (body args);
  };

  # Build the per-class deferred module for one body, dispatching on
  # its form. A function body is the plain form (config-only, gated).
  # An attrset with at least one of "options"/"config" is the
  # structured form (options pass through, config gated); its parts
  # combine through a single "imports"-bearing module. The module
  # system expands a merge-valued definition into multiple definition
  # values before the option type's merge runs, so the
  # single-imports-module form keeps one registration counting as one
  # definition under the "uniq"-wrapped registry options in
  # "modules/module-schema.nix".
  buildClassModule = name: body:
    if lib.isFunction body
    then wrap name body
    else if lib.isAttrs body && (body ? options || body ? config)
    then {
      imports =
        lib.optional (body ? options) body.options
        ++ lib.optional (body ? config) (wrap name body.config);
    }
    else throw "mkFeature: body for \"${name}\" must be a function or an attrset with \"options\" and/or \"config\"";

  # Register a name under "knownFeatures" and, for each class body it
  # declares, a gated deferred module under "featureModules".
  mkFeatureRegistration = name: bodies: {
    dotfiles =
      {
        knownFeatures = [name];
      }
      // lib.optionalAttrs (bodies != {}) {
        featureModules =
          lib.mapAttrs (_class: body: {
            ${name} = buildClassModule name body;
          })
          bodies;
      };
  };

  # Argument validation shared by the constructors below. Each
  # rejected argument throws in the author's vocabulary, citing the
  # offending registration, so that no bare Nix coercion error escapes
  # without citing the culprit.
  checkName = constructor: name:
    if !(builtins.isString name)
    then throw ''${constructor}: a registration's name must be a string, but a value of type "${builtins.typeOf name}" was passed.''
    else if name == ""
    then throw ''${constructor}: a registration's name must not be the empty string; a nameless registration could never be selected, cited as a precondition, or excluded.''
    else null;
  isListOfStrings = value: builtins.isList value && lib.all builtins.isString value;

  # A "preconditions" list is a conjunction whose entries are each
  # either a bare feature or interest name, or a group "{ anyOf = [
  # "<name>" ... ]; }" satisfied when any one member is active. A
  # group entry is a closed attrset: "anyOf" is its only key, holding
  # a list of names.
  isPreconditionEntry = entry:
    builtins.isString entry
    || (
      builtins.isAttrs entry
      && builtins.attrNames entry == ["anyOf"]
      && isListOfStrings entry.anyOf
    );
  isPreconditionList = value: builtins.isList value && lib.all isPreconditionEntry value;

  # An "implies" list holds the features a source brings along. Each
  # entry is either a bare target name or a record "{ name =
  # "<target>"; supportedPlatforms = [<systems>]; }" for an edge
  # present only when the host's platform is one of the listed
  # systems.
  isImpliesEntry = entry:
    builtins.isString entry
    || (
      builtins.isAttrs entry
      && entry ? name
      && builtins.isString entry.name
      && entry ? supportedPlatforms
      && isListOfStrings entry.supportedPlatforms
    );
  isImpliesList = value: builtins.isList value && lib.all isImpliesEntry value;

  # An "unfreePackages" entry specifies one unfree package by the name
  # that "lib.getName" yields for it, spelled out literally. A name
  # computed from a package—"lib.getName pkgs.orbstack", say—would
  # demand a package set at registration time, where none is
  # available; such an attempt leaves its mark as string context, so
  # an entry holding any is rejected along with the empty name, which
  # matches no package. Callers apply this only once the value is
  # known to be a list of strings.
  isLiteralPackageName = entry: entry != "" && !(builtins.hasContext entry);
  nonLiteralPackageNames = value: builtins.filter (entry: !(isLiteralPackageName entry)) value;

  # The platform identifiers in a list that nixpkgs does not
  # recognize: those absent from the "lib.systems.doubles.all" list.
  # Callers apply this only once the value is known to be a list of
  # strings. Shared by every platform-bearing key so that one
  # membership test judges them all: the "supportedPlatforms" key that
  # constrains a name, and the "supportedPlatforms" field of a
  # record-form "implies" entry that conditions an edge.
  unknownPlatforms = platforms:
    builtins.filter (p: !(builtins.elem p lib.systems.doubles.all)) platforms;

  # Collect, deduplicated, the platform identifiers listed across a
  # valid "implies" list's record-form entries that nixpkgs does not
  # recognize. Callers apply this only after "isImpliesList" accepts
  # the value, so every record entry holds a string list under
  # "supportedPlatforms"; a bare-name entry lists no platform and
  # contributes nothing.
  impliesUnknownPlatforms = value:
    lib.unique (
      unknownPlatforms (
        lib.concatMap (
          entry:
            if builtins.isAttrs entry
            then entry.supportedPlatforms
            else []
        )
        value
      )
    );

  # Format a list of names as a quoted, comma-separated English
  # enumeration with a serial comma, matching the "enumerateNames"
  # helper in the "modules/lib/_implications.nix" file: one name
  # renders as "a", two as "a" and "b", and three or more as "a", "b",
  # and "c".
  enumerateNames = names: let
    quoted = map (n: "\"${n}\"") names;
    count = lib.length quoted;
  in
    if count == 1
    then lib.head quoted
    else if count == 2
    then "${lib.head quoted} and ${lib.last quoted}"
    else "${lib.concatStringsSep ", " (lib.init quoted)}, and ${lib.last quoted}";

  # The module-class names that this flake's class aggregators
  # recognize. The set of keys a registration accepts is closed: once
  # a constructor splits its reserved keys off, every remaining key of
  # the argument attrset must be one of these names. A key outside the
  # set — a misspelled class name, most likely — would otherwise
  # register a body under a name that nothing ever reads, silently
  # discarding the whole body.
  classNames = ["homeManager" "nixDarwin" "nixOS"];

  # The keys the "mkFeature" function claims for itself, split off the
  # argument attrset before the remainder is read as class bodies.
  reservedKeys = ["implies" "preconditions" "supportedPlatforms" "unfreePackages"];
  checkBodyKeys = name: bodies: let
    unknownKeys = builtins.filter (key: !(builtins.elem key classNames)) (builtins.attrNames bodies);
  in
    if unknownKeys == []
    then null
    else throw ''mkFeature: the feature "${name}" passes ${
        if lib.length unknownKeys == 1
        then "the key ${enumerateNames unknownKeys}, which matches no module class"
        else "the keys ${enumerateNames unknownKeys}, which match no module classes"
      }; a body stored under such a key would never be read. The accepted keys are ${enumerateNames classNames}, plus the reserved ${enumerateNames reservedKeys} keys.'';
in {
  # In addition to the per-class bodies described in this file's
  # header, the "mkFeature" function recognizes four reserved keys:
  #
  #   preconditions: a conjunction of entries whose joint satisfaction
  #   this feature's activation is conditioned on; listing a name here
  #   never activates it. Each entry is either a bare feature or
  #   interest name (satisfied when that name is active) or a group "{
  #   anyOf = [ "<name>" ... ]; }" (satisfied when at least one member
  #   is active); the feature activates when every entry is satisfied,
  #   so the list reads as a conjunction of disjunctions. Every
  #   "anyOf" member must be a non-contingent name — an ordinary
  #   feature or an interest, never a contingent feature — which keeps
  #   groups out of every precondition cycle; an assertion in
  #   "modules/_assertions.nix" enforces this. A bare entry may still
  #   list a contingent feature. A feature declaring this key is a
  #   "contingent feature": it activates automatically exactly when
  #   all of its preconditions are met, and that is its only
  #   activation path—no host and no implied edge may list it directly
  #   (an assertion in "modules/_assertions.nix" enforces this). The
  #   registry option's type treats the list as a set — its merge
  #   normalizes each definition — so order and duplication are
  #   immaterial, equal declarations merge, and unequal ones are
  #   rejected. A null value is identical to omitting the key — an
  #   explicit "no preconditions" — so callers building the value
  #   programmatically need no special case. An empty list is an
  #   error: a contingent feature with no preconditions would be
  #   unconditionally active; an empty "anyOf" group is likewise an
  #   error. A single-member "anyOf" group is accepted and behaves as
  #   the bare name.
  #
  #   implies: a list of the features this feature brings along—the
  #   implied edges whose source is this feature. Each entry is either
  #   a bare target name (an unconditional edge) or a record "{ name =
  #   "<target>"; supportedPlatforms = [<systems>]; }" (an edge
  #   present only when the host's platform is one of the listed
  #   systems); every platform a record lists must be one that nixpkgs
  #   recognizes (a member of the "lib.systems.doubles.all" list), so
  #   a misspelling fails here at registration. The "implicationsFor"
  #   function in "modules/lib/_implications.nix" assembles these into
  #   the implication graph; a feature's edges may target only other
  #   features, never an interest. A null value is identical to
  #   omitting the key.
  #
  #   supportedPlatforms: a list of Nixpkgs system identifiers (e.g.
  #   ["aarch64-darwin" "aarch64-linux"]) on which this feature may
  #   activate. A host qualifies when its platform is one of them.
  #   Omit the key for a feature that may activate on every platform.
  #   The implication graph drops an unsupported name from every
  #   target list it assembles, and an assertion in the
  #   "modules/_assertions.nix" file rejects a host whose activation
  #   includes one anyway. Every entry must be a platform that nixpkgs
  #   recognizes—a member of the "lib.systems.doubles.all" list—so a
  #   misspelled identifier fails here at registration instead of
  #   silently constraining the feature away on every host.
  #
  #   unfreePackages: the unfree packages this feature installs, each
  #   spelled as the string that "lib.getName" yields for the package
  #   ("1password-cli" for "pkgs._1password-cli", "zoom" for
  #   "pkgs.zoom-us"). Every entry must be that name written out
  #   literally: computing one from a package would demand a package
  #   set at registration time, where none is available. The names
  #   accumulate across every feature into the "unfreePackages"
  #   registry, which "modules/nixpkgs-config.nix" publishes as
  #   "flake.allowUnfreePackages" and both nixpkgs instantiation sites
  #   hand to nixpkgs' own "allowUnfreePackages" option. List a
  #   package here whenever this feature can install it, even on one
  #   platform alone and even under a condition the host may not meet:
  #   the toleration list is one flat set that every instantiation
  #   receives, and tolerating a package that nothing installs costs
  #   nothing while installing one without toleration halts
  #   evaluation. An empty list, like an omitted key, specifies
  #   nothing.
  mkFeature = name: args: let
    bodies = builtins.removeAttrs args reservedKeys;
    declaredBadPlatforms =
      if args ? supportedPlatforms && isListOfStrings args.supportedPlatforms
      then unknownPlatforms args.supportedPlatforms
      else [];
    impliesBadPlatforms =
      if args ? implies && args.implies != null && isImpliesList args.implies
      then impliesUnknownPlatforms args.implies
      else [];
    # The "anyOf" groups among a well-formed "preconditions" value,
    # ready for the empty-group check below. Computed only once the
    # value is known to be a valid precondition list, so a malformed
    # value trips the earlier check instead.
    preconditionGroups =
      if args ? preconditions && args.preconditions != null && isPreconditionList args.preconditions
      then builtins.filter builtins.isAttrs args.preconditions
      else [];
    emptyGroup = lib.findFirst (g: g.anyOf == []) null preconditionGroups;
    # The "unfreePackages" entries that are not literal package names,
    # ready for the check below. Computed only once the value is known
    # to be a list of strings, so a malformed value trips the earlier
    # check instead.
    unfreeNonLiteralNames =
      if args ? unfreePackages && isListOfStrings args.unfreePackages
      then nonLiteralPackageNames args.unfreePackages
      else [];
    checks = lib.seq (checkName "mkFeature" name) (
      if args ? supportedPlatforms && !(isListOfStrings args.supportedPlatforms)
      then throw ''mkFeature: the feature "${name}" passes a "supportedPlatforms" value that is not a list of strings. Pass the Nixpkgs system identifiers on which the feature may activate.''
      else if declaredBadPlatforms != []
      then throw ''mkFeature: the feature "${name}" declares support for ${enumerateNames declaredBadPlatforms}, which ${
          if lib.length declaredBadPlatforms == 1
          then "matches no platform"
          else "match no platforms"
        } that nixpkgs recognizes (the "lib.systems.doubles.all" list).''
      else if args ? preconditions && args.preconditions != null && !(isPreconditionList args.preconditions)
      then throw ''mkFeature: the feature "${name}" passes a "preconditions" value that is not a list of preconditions. Each entry is either a bare feature or interest name, or a group "{ anyOf = [ "<name>" ... ]; }" satisfied when any one member is active.''
      else if emptyGroup != null
      then throw ''mkFeature: the feature "${name}" passes an empty "anyOf" group; a group must offer at least one alternative.''
      else if args ? implies && args.implies != null && !(isImpliesList args.implies)
      then throw ''mkFeature: the feature "${name}" passes an "implies" value that is not a list of edge declarations. Each entry identifies a target feature, written either as a bare name string or as a record "{ name = "<target>"; supportedPlatforms = [<systems>]; }" for an edge present only on the listed platforms.''
      else if impliesBadPlatforms != []
      then throw ''mkFeature: the feature "${name}" declares an "implies" edge supporting ${enumerateNames impliesBadPlatforms}, which ${
          if lib.length impliesBadPlatforms == 1
          then "matches no platform"
          else "match no platforms"
        } that nixpkgs recognizes (the "lib.systems.doubles.all" list).''
      else if args ? unfreePackages && !(isListOfStrings args.unfreePackages)
      then throw ''mkFeature: the feature "${name}" passes an "unfreePackages" value that is not a list of strings. Pass the unfree packages this feature installs, each spelled as the string that "lib.getName" yields for the package.''
      else if unfreeNonLiteralNames != []
      then throw ''mkFeature: the feature "${name}" lists ${enumerateNames unfreeNonLiteralNames} among its unfree packages, which ${
          if lib.length unfreeNonLiteralNames == 1
          then "is not a literal package name"
          else "are not literal package names"
        }. Spell out each name: the empty name matches no package, and a name interpolated from a package demands a package set at registration time, where none is available.''
      else checkBodyKeys name bodies
    );
    registration = mkFeatureRegistration name bodies;
    extraDotfiles =
      lib.optionalAttrs (args ? supportedPlatforms) {
        supportedPlatforms.${name} = args.supportedPlatforms;
      }
      // lib.optionalAttrs (args ? preconditions && args.preconditions != null) {
        featurePreconditions.${name} =
          if args.preconditions == []
          then throw ''mkFeature: the feature "${name}" passes an empty "preconditions" list, but a contingent feature with no preconditions would be unconditionally active. Omit the "preconditions" key (or set it to null) to define an ordinary feature.''
          else args.preconditions;
      }
      // lib.optionalAttrs (args ? implies && args.implies != null && args.implies != []) {
        impliedEdges.${name} = args.implies;
      }
      # The toleration registry is one flat set rather than a record
      # keyed by feature name, so this contribution is a bare list
      # that the option's accumulating merge unions with every other
      # feature's.
      // lib.optionalAttrs (args ? unfreePackages) {
        inherit (args) unfreePackages;
      };
  in
    lib.seq checks (
      registration
      // lib.optionalAttrs (extraDotfiles != {}) {
        dotfiles = registration.dotfiles // extraDotfiles;
      }
    );

  # Register an interest: a named want that participates in activation
  # exactly as a feature does — a host may select or exclude it, and a
  # contingent feature may list it as a precondition — but that
  # declares no configuration of its own. Called with a closed attrset
  # pattern:
  #
  #   flakeLib.mkInterest {
  #     name = "dev/language-servers";
  #     # Optional prose for future diagnostic or documentation
  #     # surfaces; nothing consumes it yet.
  #     description = "Editor-facing language servers.";
  #   }
  #
  # The closed pattern lets Nix itself reject any unexpected attribute
  # — a "homeManager" body, say — with its precise unexpected-argument
  # error, keeping configuration out of interests by construction. The
  # name enters the "knownInterests" registry, which the activation
  # machinery folds in beside the feature names; a non-null
  # description enters the "interestDescriptions" registry.
  #
  # The optional "implies" key lists other interests this interest
  # brings along — a bundle. Selecting the bundle activates its
  # members through the implication closure, the way a feature bundle
  # fans out to the finer ones; the edges join the "impliedEdges"
  # registry beside those a feature declares. Each entry is a bare
  # interest name — the record form that the "mkFeature" function
  # accepts for a platform-conditional edge has no place here. An
  # interest may imply only interests, never a feature, which an
  # assertion in the "modules/_assertions.nix" file enforces at
  # resolve time. A null or omitted value, or an empty list, brings
  # nothing along.
  mkInterest = {
    name,
    description ? null,
    implies ? null,
  }: let
    checks = lib.seq (checkName "mkInterest" name) (
      if implies != null && !(isListOfStrings implies)
      then throw ''mkInterest: the interest "${name}" passes an "implies" value that is not a list of interest names. Pass the names of the interests this interest brings along, or null for none.''
      else null
    );
  in
    lib.seq checks {
      dotfiles =
        {
          knownInterests = [name];
        }
        // lib.optionalAttrs (description != null) {
          interestDescriptions.${name} = description;
        }
        // lib.optionalAttrs (implies != null && implies != []) {
          impliedEdges.${name} = implies;
        };
    };

  # Value-level combinator for a fragment INSIDE a feature body that
  # applies only when some further features or interests are active,
  # beyond the file's own activation gate. Intended use:
  #
  #   flakeLib.mkFeature "shell/zsh" {
  #     homeManager = {config, ...}: {
  #       programs.zsh.enable = true;
  #       programs.zsh.initContent = flakeLib.onlyWhen config ["kubernetes"] ''
  #         # ... kubernetes-flavored shell additions ...
  #       '';
  #     };
  #   }
  #
  # The result is a "lib.mkIf"-wrapped value, so it composes with the
  # "inEffect" gate that the "mkFeature" function already wraps around
  # the whole body: the fragment takes effect exactly when the
  # enclosing feature AND every named feature or interest are active.
  # Declare a contingent feature (via the "preconditions" argument of
  # the "mkFeature" function) instead when the pairing deserves its
  # own name, file, and excludability; this combinator suits pairings
  # too slight for that.
  #
  # Every name is validated against the known names readable from the
  # caller's "config" argument: a name registered nowhere throws —
  # with the same wording as the precondition-hygiene assertions in
  # the "modules/_assertions.nix" file — rather than leaving a
  # fragment that never applies. The validation runs whenever the gate
  # is consulted, so it does not stop at the first inactive name.
  onlyWhen = config: names: fragment: let
    checkOne = name:
      if builtins.elem name config.dotfiles._knownNames
      then null
      else throw ''onlyWhen: the fragment cites the precondition "${name}", but no imported module advertises that name as a feature or an interest.'';
    checks =
      if !(isListOfStrings names)
      then throw "onlyWhen: the names argument must be a list of feature or interest names."
      else lib.foldl' (acc: name: lib.seq (checkOne name) acc) null names;
  in
    lib.mkIf (lib.seq checks (lib.all config.dotfiles._host.inEffect names)) fragment;
}

# Helpers that build feature and profile modules with their
# activation gating wired in.
#
# A "feature" or "profile" file using these helpers takes the form:
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
# That key set is closed: a key naming no class (and not one of the
# constructors' reserved keys) throws at registration rather than
# storing a body that nothing ever reads. Each value is one of two
# forms:
#
#   1. A deferred module returning the *contents* of a "config"
#      block — not a full module with its own "config = {...}" key.
#      The helper inserts the
#      "config = lib.mkIf (inEffect name) (...)" wrapper, so
#      a body that itself wraps in "config = {...}" would produce
#      "config.config = {...}" and silently drop its contributions.
#
#   2. A structured attrset "{options? = ...; config? = ...;}" for
#      features that declare their own options. The "options" half
#      is a normal module function (e.g. "{lib, ...}: {options = ...;}")
#      passed through verbatim — option declarations cannot be
#      conditional. The "config" half follows the same contract as
#      form 1 and gets the same activation gate. Either key may be
#      omitted.
#
# Conditional sub-contributions inside a config body should use
# "lib.mkMerge" / "lib.mkIf" at the value level, not at the module
# level.
#
# An empty bodies attrset ("{}") is allowed and produces a
# name-only registration (under "knownFeatures" or "knownProfiles")
# with no module contributions. Useful when a name is meaningful
# only as a cross-feature reference.
{lib}: let
  # Build a deferred module that gates "body" on "host.<predicate>
  # name", where "host" is the resolved "config.dotfiles._host" and
  # "predicate" is one of "inEffect" / "activatesProfile".
  #
  # The wrapper's outer function destructures every module argument
  # that bodies might rely on, so the module system's argument
  # injection mechanism (driven by "builtins.functionArgs") can fill
  # them in from "_module.args". A body that destructures an
  # argument missing from this list will fail with "called without
  # required argument".
  #
  # Currently covers what the home-manager, nix-darwin, and NixOS
  # class evaluators provide for the features in this repository.
  # If a future feature body needs "inputs", "specialArgs", or any
  # other module argument, add it here as "name ? null".
  wrap = predicate: name: body: {
    config,
    lib,
    # deadnix: skip
    pkgs ? null,
    # deadnix: skip
    osConfig ? null,
    ...
  } @ args: {
    config = lib.mkIf (predicate config.dotfiles._host name) (body args);
  };

  # Build the per-class deferred module for one body, dispatching
  # on its form. A function body is the plain form (config-only,
  # gated). An attrset with at least one of "options"/"config" is
  # the structured form (options pass through, config gated); its
  # parts combine through a single "imports"-bearing module. The
  # module system expands a merge-valued definition into multiple
  # definition values before the option type's merge runs, so the
  # single-imports-module form keeps one registration counting as
  # one definition under the "uniq"-wrapped registry options in
  # "modules/module-schema.nix".
  buildClassModule = predicate: name: body:
    if lib.isFunction body
    then wrap predicate name body
    else if lib.isAttrs body && (body ? options || body ? config)
    then {
      imports =
        lib.optional (body ? options) body.options
        ++ lib.optional (body ? config) (wrap predicate name body.config);
    }
    else throw "mkFeature/mkProfile: body for \"${name}\" must be a function or an attrset with \"options\" and/or \"config\"";

  # Common builder for both helpers. "knownKey" is the
  # accumulator-list option ("knownFeatures" / "knownProfiles");
  # "modulesKey" is the per-class deferred-module option
  # ("featureModules" / "profileModules"); "predicate" is the
  # activation predicate to apply at evaluation time.
  mk = {
    knownKey,
    modulesKey,
    predicate,
  }: name: bodies: {
    dotfiles =
      {
        ${knownKey} = [name];
      }
      // lib.optionalAttrs (bodies != {}) {
        ${modulesKey} =
          lib.mapAttrs (_class: body: {
            ${name} = buildClassModule predicate name body;
          })
          bodies;
      };
  };
  mkProfileRegistration = mk {
    knownKey = "knownProfiles";
    modulesKey = "profileModules";
    predicate = host: name: host.activatesProfile name;
  };
  mkFeatureRegistration = mk {
    knownKey = "knownFeatures";
    modulesKey = "featureModules";
    predicate = host: name: host.inEffect name;
  };

  # Argument validation shared by the constructors below. Each
  # rejected argument throws in the author's vocabulary, naming the
  # offending registration, so that no bare Nix coercion error
  # escapes without naming the culprit.
  checkName = constructor: name:
    if !(builtins.isString name)
    then throw ''${constructor}: a registration's name must be a string, but a value of type "${builtins.typeOf name}" was passed.''
    else if name == ""
    then throw ''${constructor}: a registration's name must not be the empty string; a nameless registration could never be selected, named as a precondition, or excluded.''
    else null;
  isListOfStrings = value: builtins.isList value && lib.all builtins.isString value;

  # A "preconditions" list is a conjunction whose entries are each
  # either a bare feature or interest name, or a group
  # "{ anyOf = [ "<name>" ... ]; }" satisfied when any one member is
  # active. A group entry is a closed attrset: "anyOf" is its only
  # key, holding a list of names.
  isPreconditionEntry = entry:
    builtins.isString entry
    || (
      builtins.isAttrs entry
      && builtins.attrNames entry == ["anyOf"]
      && isListOfStrings entry.anyOf
    );
  isPreconditionList = value: builtins.isList value && lib.all isPreconditionEntry value;

  # An "implies" list names the profiles or features a source
  # brings along. Each entry is either a bare target name or a
  # record "{ name = "<target>"; supportedPlatforms = [<systems>];
  # }" naming an edge present only when the host's platform is one
  # of the listed systems.
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

  # The platform identifiers in a list that nixpkgs does not
  # recognize: those absent from the "lib.systems.doubles.all" list.
  # Callers apply this only once the value is known to be a list of
  # strings. Shared by every platform-bearing key so that one
  # membership test judges them all: the "supportedPlatforms" key that
  # constrains a name, and the "supportedPlatforms" field of a
  # record-form "implies" entry that conditions an edge.
  unknownPlatforms = platforms:
    builtins.filter (p: !(builtins.elem p lib.systems.doubles.all)) platforms;

  # Collect, deduplicated, the platform identifiers named across a
  # valid "implies" list's record-form entries that nixpkgs does not
  # recognize. Callers apply this only after "isImpliesList" accepts
  # the value, so every record entry carries a string list under
  # "supportedPlatforms"; a bare-name entry names no platform and
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
  # renders as "a", two as "a" and "b", and three or more as "a",
  # "b", and "c".
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
  # recognize. The set of keys a registration accepts is closed:
  # once a constructor splits its reserved key off, every remaining
  # key of the argument attrset must be one of these names. A key
  # outside the set — a misspelled class name, most likely — would
  # otherwise register a body under a name that nothing ever reads,
  # silently discarding the whole body.
  classNames = ["homeManager" "nixDarwin" "nixOS"];
  checkBodyKeys = constructor: kind: reservedKeys: name: bodies: let
    unknownKeys = builtins.filter (key: !(builtins.elem key classNames)) (builtins.attrNames bodies);
  in
    if unknownKeys == []
    then null
    else throw ''${constructor}: the ${kind} "${name}" passes ${
        if lib.length unknownKeys == 1
        then "the key ${enumerateNames unknownKeys}, which names no module class"
        else "the keys ${enumerateNames unknownKeys}, which name no module classes"
      }; a body stored under such a key would never be read. The accepted keys are ${enumerateNames classNames}, plus the reserved ${
        if lib.length reservedKeys == 1
        then "${enumerateNames reservedKeys} key"
        else "${enumerateNames reservedKeys} keys"
      }.'';
in {
  # In addition to the per-class bodies described in this file's
  # header, the "mkFeature" function recognizes three reserved keys:
  #
  #   preconditions: a conjunction of entries whose joint satisfaction
  #   this feature's activation is conditioned on; listing a name here
  #   never activates it. Each entry is either a bare feature or
  #   interest name (satisfied when that name is active) or a group
  #   "{ anyOf = [ "<name>" ... ]; }" (satisfied when at least one
  #   member is active); the feature activates when every entry is
  #   satisfied, so the list reads as a conjunction of disjunctions.
  #   Every "anyOf" member must be a non-contingent name — an ordinary
  #   feature or an interest, never a contingent feature — which keeps
  #   groups out of every precondition cycle; an assertion in
  #   "modules/_assertions.nix" enforces this. A bare entry may still
  #   name a contingent feature. A feature carrying this key is a
  #   "contingent feature": it activates automatically exactly when
  #   all of its preconditions are met, and that is its only
  #   activation path—no host, profile, or implied edge may name it
  #   directly (an assertion in "modules/_assertions.nix" enforces
  #   this). The registry option's type treats the list as a set — its
  #   merge normalizes each definition — so order and duplication are
  #   immaterial, equal declarations merge, and unequal ones are
  #   rejected. A null value is identical to omitting the key — an
  #   explicit "no preconditions" — so callers building the value
  #   programmatically need no special case. An empty list is an
  #   error: a contingent feature with no preconditions would be
  #   unconditionally active; an empty "anyOf" group is likewise an
  #   error. A single-member "anyOf" group is accepted and behaves as
  #   the bare name.
  #
  #   implies: a list naming the features this feature brings
  #   along—the implied edges whose source is this feature. Each entry
  #   is either a bare target name (an unconditional edge) or a record
  #   "{ name = "<target>"; supportedPlatforms = [<systems>]; }" (an
  #   edge present only when the host's platform is one of the listed
  #   systems); every platform a record names must be one that nixpkgs
  #   recognizes (a member of the "lib.systems.doubles.all" list), so
  #   a misspelling fails here at registration. The "implicationsFor"
  #   function in "modules/lib/_implications.nix" assembles these into
  #   the implication graph; a feature's edges may target only other
  #   features, never profiles or interests. A null value is identical
  #   to omitting the key.
  #
  #   supportedPlatforms: a list of Nixpkgs system identifiers (e.g.
  #   ["aarch64-darwin" "x86_64-darwin"]) on which this feature may
  #   activate. A host qualifies when its platform is one of them.
  #   Omit the key for a feature that may activate on every platform.
  #   The implication graph drops an unsupported name from every
  #   target list it assembles, and an assertion in
  #   "modules/_assertions.nix" rejects a host whose activation
  #   includes one anyway. Every entry must be a platform that nixpkgs
  #   recognizes—a member of the "lib.systems.doubles.all" list—so a
  #   misspelled identifier fails here at registration instead of
  #   silently constraining the feature away on every host.
  mkFeature = name: args: let
    bodies = builtins.removeAttrs args ["preconditions" "implies" "supportedPlatforms"];
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
    checks = lib.seq (checkName "mkFeature" name) (
      if args ? supportedPlatforms && !(isListOfStrings args.supportedPlatforms)
      then throw ''mkFeature: the feature "${name}" passes a "supportedPlatforms" value that is not a list of strings. Pass the Nixpkgs system identifiers on which the feature may activate.''
      else if declaredBadPlatforms != []
      then throw ''mkFeature: the feature "${name}" declares support for ${enumerateNames declaredBadPlatforms}, which ${
          if lib.length declaredBadPlatforms == 1
          then "names no platform"
          else "name no platforms"
        } that nixpkgs recognizes (the "lib.systems.doubles.all" list).''
      else if args ? preconditions && args.preconditions != null && !(isPreconditionList args.preconditions)
      then throw ''mkFeature: the feature "${name}" passes a "preconditions" value that is not a list of preconditions. Each entry is either a bare feature or interest name, or a group "{ anyOf = [ "<name>" ... ]; }" satisfied when any one member is active.''
      else if emptyGroup != null
      then throw ''mkFeature: the feature "${name}" passes an empty "anyOf" group; a group must offer at least one alternative.''
      else if args ? implies && args.implies != null && !(isImpliesList args.implies)
      then throw ''mkFeature: the feature "${name}" passes an "implies" value that is not a list of edge declarations. Each entry names a target feature, written either as a bare name string or as a record "{ name = "<target>"; supportedPlatforms = [<systems>]; }" for an edge present only on the listed platforms.''
      else if impliesBadPlatforms != []
      then throw ''mkFeature: the feature "${name}" declares an "implies" edge supporting ${enumerateNames impliesBadPlatforms}, which ${
          if lib.length impliesBadPlatforms == 1
          then "names no platform"
          else "name no platforms"
        } that nixpkgs recognizes (the "lib.systems.doubles.all" list).''
      else checkBodyKeys "mkFeature" "feature" ["implies" "preconditions" "supportedPlatforms"] name bodies
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
  # contingent feature may name it as a precondition — but that
  # carries no configuration of its own. Called with a closed attrset
  # pattern:
  #
  #   flakeLib.mkInterest {
  #     name = "dev/language-servers";
  #     # Optional prose for future diagnostic or documentation
  #     # surfaces; nothing consumes it yet.
  #     description = "Editor-facing language servers.";
  #   }
  #
  # The closed pattern lets Nix itself reject any unexpected
  # attribute — a "homeManager" body, say — with its precise
  # unexpected-argument error, keeping configuration out of
  # interests by construction. The name enters the "knownInterests"
  # registry, which the activation machinery folds in beside the
  # feature names; a non-null description enters the
  # "interestDescriptions" registry.
  #
  # The optional "implies" key names other interests this interest
  # brings along — a bundle. Selecting the bundle activates its
  # members through the implication closure, the way a profile fans
  # out to its features; the edges join the "impliedEdges" registry
  # beside those a feature or a profile declares. Each entry is a
  # bare interest name — the record form that the "mkFeature" and
  # "mkProfile" functions accept for a platform-conditional edge has
  # no place here. An interest may imply only interests, never a
  # feature or a profile, which an assertion in the
  # "modules/_assertions.nix" file enforces at resolve time. A null
  # or omitted value, or an empty list, brings nothing along.
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
  # Every name is validated against the registries reachable from the
  # caller's "config": a name registered as a profile, or registered
  # nowhere, throws — with the same wording as the
  # precondition-hygiene assertions in
  # "modules/_assertions.nix" — rather than leaving a fragment that
  # never applies. The validation runs whenever the gate is consulted,
  # so it does not stop at the first inactive name.
  onlyWhen = config: names: fragment: let
    checkOne = name:
      if builtins.elem name config.dotfiles._knownNames
      then null
      else if builtins.elem name config.dotfiles._knownProfiles
      then throw ''onlyWhen: the fragment's precondition "${name}" names a known profile, but a precondition may name only a feature or an interest.''
      else throw ''onlyWhen: the fragment names the precondition "${name}", but no imported module advertises that name as a feature or an interest.'';
    checks =
      if !(isListOfStrings names)
      then throw "onlyWhen: the names argument must be a list of feature or interest names."
      else lib.foldl' (acc: name: lib.seq (checkOne name) acc) null names;
  in
    lib.mkIf (lib.seq checks (lib.all config.dotfiles._host.inEffect names)) fragment;

  # In addition to the per-class bodies that the "mkFeature"
  # function accepts, the "mkProfile" function recognizes two
  # reserved keys:
  #
  #   supportedPlatforms: a list of Nixpkgs system identifiers (e.g.
  #   ["aarch64-darwin" "x86_64-darwin"]) on which this profile may
  #   activate. A host qualifies when its platform is one of them.
  #   Omit the key for a profile that may activate on every platform.
  #   The key carries the same meaning for a feature, and the
  #   "mkFeature" function above accepts it as well. The implication
  #   graph drops an unsupported name from every target list it
  #   assembles, and an assertion in "modules/_assertions.nix" rejects
  #   a host whose activation includes one anyway. Every entry must be
  #   a platform that nixpkgs recognizes—a member of the
  #   "lib.systems.doubles.all" list—so a misspelled identifier fails
  #   here at registration instead of silently constraining the
  #   profile away on every host.
  #
  #   implies: a list naming the profiles or features this profile
  #   brings along—the implied edges whose source is this profile.
  #   Each entry is either a bare target name (an unconditional edge)
  #   or a record "{ name = "<target>"; supportedPlatforms =
  #   [<systems>]; }" (an edge present only when the host's platform
  #   is one of the listed systems); every platform a record names
  #   must be one that nixpkgs recognizes (a member of the
  #   "lib.systems.doubles.all" list), so a misspelling fails here at
  #   registration. The "implicationsFor" function in
  #   "modules/lib/_implications.nix" assembles these into the
  #   implication graph; a profile's edges may target only profiles or
  #   features, never interests. A null value is identical to omitting
  #   the key.
  #
  # The "preconditions" key belongs to the "mkFeature" function alone;
  # passing it here is an error rather than a silently discarded class
  # body.
  mkProfile = name: args: let
    bodies = builtins.removeAttrs args ["supportedPlatforms" "implies"];
    declaredBadPlatforms =
      if args ? supportedPlatforms && isListOfStrings args.supportedPlatforms
      then unknownPlatforms args.supportedPlatforms
      else [];
    impliesBadPlatforms =
      if args ? implies && args.implies != null && isImpliesList args.implies
      then impliesUnknownPlatforms args.implies
      else [];
    checks = lib.seq (checkName "mkProfile" name) (
      if args ? preconditions
      then throw ''mkProfile: the profile "${name}" passes a "preconditions" key, but the "mkFeature" function reserves that key; only features may be contingent.''
      else if args ? supportedPlatforms && !(isListOfStrings args.supportedPlatforms)
      then throw ''mkProfile: the profile "${name}" passes a "supportedPlatforms" value that is not a list of strings. Pass the Nixpkgs system identifiers on which the profile may activate.''
      else if declaredBadPlatforms != []
      then throw ''mkProfile: the profile "${name}" declares support for ${enumerateNames declaredBadPlatforms}, which ${
          if lib.length declaredBadPlatforms == 1
          then "names no platform"
          else "name no platforms"
        } that nixpkgs recognizes (the "lib.systems.doubles.all" list).''
      else if args ? implies && args.implies != null && !(isImpliesList args.implies)
      then throw ''mkProfile: the profile "${name}" passes an "implies" value that is not a list of edge declarations. Each entry names a target profile or feature, written either as a bare name string or as a record "{ name = "<target>"; supportedPlatforms = [<systems>]; }" for an edge present only on the listed platforms.''
      else if impliesBadPlatforms != []
      then throw ''mkProfile: the profile "${name}" declares an "implies" edge supporting ${enumerateNames impliesBadPlatforms}, which ${
          if lib.length impliesBadPlatforms == 1
          then "names no platform"
          else "name no platforms"
        } that nixpkgs recognizes (the "lib.systems.doubles.all" list).''
      else checkBodyKeys "mkProfile" "profile" ["implies" "supportedPlatforms"] name bodies
    );
    registration = mkProfileRegistration name bodies;
    extraDotfiles =
      lib.optionalAttrs (args ? supportedPlatforms) {
        supportedPlatforms.${name} = args.supportedPlatforms;
      }
      // lib.optionalAttrs (args ? implies && args.implies != null && args.implies != []) {
        impliedEdges.${name} = args.implies;
      };
  in
    lib.seq checks (
      registration
      // lib.optionalAttrs (extraDotfiles != {}) {
        dotfiles = registration.dotfiles // extraDotfiles;
      }
    );
}

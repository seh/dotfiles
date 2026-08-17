# Option types shared by the registry options in the
# "modules/module-schema.nix" file. They live in this library file so
# that probes and future consumers can construct every variant
# directly, rather than settling for the instances embedded in the
# schema's option declarations.
{lib}: let
  # Predicate for a single precondition entry: either a bare name (a
  # string) or an "anyOf" group "{ anyOf = [ "<name>" ... ]; }" whose
  # sole key contains a non-empty list of names. A single-member group
  # is accepted (a filtered-down candidate set may legitimately yield
  # one member); only an empty group, an extra key, or a non-string
  # member is rejected. Shared by the "preconditionEntry" and
  # "preconditionSet" types below so the group form is enforced
  # identically wherever a preconditions value appears.
  isPreconditionEntry = entry:
    builtins.isString entry
    || (
      builtins.isAttrs entry
      && builtins.attrNames entry == ["anyOf"]
      && builtins.isList entry.anyOf
      && entry.anyOf != []
      && lib.all builtins.isString entry.anyOf
    );
in {
  # Build the option type for a set of names that several modules may
  # define on one option. Each definition is written as a Nix list—the
  # only ergonomic literal for a set—and is normalized (sorted,
  # deduplicated) before combining, so order and duplication have no
  # meaning, within one definition and across definitions. The empty
  # list is accepted by default: an option whose empty set is itself a
  # meaningful value (say, "no forbidding" for the "forbidFeatures"
  # option, which defaults empty) needs no ceremony, and the empty
  # list then serves as both the authored value and the default. A
  # strict option passes "allowEmpty = false" to reject the empty list
  # per definition, expressing "no constraint" by omitting the
  # attribute entirely so that an accidental "[]" cannot pass silently
  # while meaning something else.
  #
  # The "merge" parameter selects how multiple definitions of one
  # option combine. The name deliberately echoes the module system's
  # own term, since it selects exactly that behavior. The accepted
  # values:
  #
  #   "agreement": the normalized definitions must be equal, and
  #   unequal definitions are a conflicting-definitions error rather
  #   than a union.
  #
  #   "union": the union of the normalized definitions.
  #
  # An unknown value throws at type-construction time, listing the
  # accepted values. The type's name and description state the chosen
  # policy so that a type-mismatch error reports it.
  setOfNames = {
    merge,
    allowEmpty ? true,
  }: let
    normalize = value: lib.sort lib.lessThan (lib.unique value);
    policies = {
      agreement = {
        phrase = "definitions must agree";
        combine = loc: defs:
          lib.options.mergeEqualOption loc (
            map (def: def // {value = normalize def.value;}) defs
          );
      };
      union = {
        phrase = "definitions combine as their union";
        combine = _loc: defs: normalize (lib.concatMap (def: def.value) defs);
      };
    };
    policy =
      if policies ? ${merge}
      then policies.${merge}
      else throw ''setOfNames: unknown "merge" policy "${merge}". The accepted values are "agreement" and "union".'';
  in
    lib.seq policy (lib.mkOptionType {
      name =
        if allowEmpty
        then "setOfNames-${merge}"
        else "nonEmptySetOfNames-${merge}";
      description = "set of names (${policy.phrase}), written as a ${
        if allowEmpty
        then "list"
        else "non-empty list"
      } of strings; order and duplication have no meaning";
      check = v: builtins.isList v && (allowEmpty || v != []) && lib.all builtins.isString v;
      merge = policy.combine;
    });

  # The option type for a single precondition entry. Reusable for list
  # types that permit emptiness (e.g. the unmet-subset "missing" field
  # of the "latentFeatures" diagnostic, an "attrsOf preconditionEntry"
  # list that may be empty), where "preconditionSet"'s non-empty-list
  # constraint would be wrong. Declares no merge of its own; a
  # consumer wraps it in "listOf" (or similar) and inherits that
  # combinator's merge.
  preconditionEntry = lib.mkOptionType {
    name = "preconditionEntry";
    description = ''precondition entry: a name or an "{anyOf=[...];}" group'';
    check = isPreconditionEntry;
  };

  # Build the option type for a feature's "preconditions": a set of
  # precondition entries that several modules may define on one
  # option. Each entry satisfies "preconditionEntry": a bare name (a
  # string) or an "anyOf" group "{ anyOf = [ "<name>" ... ]; }" whose
  # sole key holds a non-empty list of names. Like "setOfNames", each
  # definition is normalized to a canonical form before combining, so
  # order and duplication have no meaning—within one definition and
  # across definitions—and the merge demands agreement: definitions
  # denoting the same set merge, and ones denoting different sets are
  # a conflicting-definitions error. Takes no policy arguments; the
  # agreement merge and the non-empty list are fixed, since the
  # "featurePreconditions" registry and the "_activation.nix" mirror
  # are its only consumers.
  preconditionSet = {}: let
    isGroup = entry: builtins.isAttrs entry;
    # Canonical form of one group: its members sorted and
    # deduplicated.
    normalizeGroup = group: {anyOf = lib.sort lib.lessThan (lib.unique group.anyOf);};
    # Canonical form of a whole list: the bare names sorted and
    # deduplicated, then the deduplicated groups ordered by their
    # comma-joined member string, with all bare names presented ahead
    # of all groups. Two precondition lists are equivalent exactly
    # when their canonical forms are equal.
    normalize = value: let
      bare = lib.sort lib.lessThan (lib.unique (builtins.filter builtins.isString value));
      groups = lib.unique (
        lib.sort (
          a: b:
            lib.lessThan (lib.concatStringsSep "," a.anyOf) (lib.concatStringsSep "," b.anyOf)
        )
        (map normalizeGroup (builtins.filter isGroup value))
      );
    in
      bare ++ groups;
  in
    lib.mkOptionType {
      name = "preconditionSet";
      description = ''set of preconditions (definitions must agree), each entry a name or an "{anyOf=[...];}" group, written as a non-empty list; order and duplication have no meaning'';
      check = v:
        builtins.isList v
        && v != []
        && lib.all isPreconditionEntry v;
      merge = loc: defs:
        lib.options.mergeEqualOption loc (
          map (def: def // {value = normalize def.value;}) defs
        );
    };
}

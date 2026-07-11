# Option types shared by the registry options in the
# "modules/module-schema.nix" file. They live in this library file so
# that probes and future consumers can construct every variant
# directly, rather than reaching only the instances embedded in the
# schema's option declarations.
{lib}: {
  # Build the option type for a set of names that several modules
  # may define on one option. Each definition is written as a Nix
  # list — the only ergonomic literal for a set — and is normalized
  # (sorted, deduplicated) before combining, so order and
  # duplication carry no meaning, within one definition and across
  # definitions. The empty list is rejected per definition: each
  # option using this type expresses "no constraint" by omitting the
  # attribute entirely, and an accidental "[]" would otherwise pass
  # silently while meaning something else. A combined result may
  # still be empty (see the "intersection" policy below): that is a
  # computed value, not an authored one.
  #
  # The "merge" parameter selects how multiple definitions of one
  # option combine. The name deliberately echoes the module system's
  # own term, since it selects exactly that behavior; it is
  # provisional pending the user's naming. The accepted values:
  #
  #   "agreement": the normalized definitions must be equal, and
  #   unequal definitions are a conflicting-definitions error rather
  #   than a union.
  #
  #   "union": the union of the normalized definitions.
  #
  #   "intersection": the intersection of the normalized
  #   definitions, which may legitimately be empty.
  #
  # An unknown value throws at type-construction time, listing the
  # accepted values. The type's name and description carry the
  # chosen policy so that a type-mismatch error names it.
  setOfNames = {merge}: let
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
      intersection = {
        phrase = "definitions combine as their intersection, which may be empty";
        combine = _loc: defs: let
          values = map (def: normalize def.value) defs;
        in
          lib.foldl' lib.intersectLists (lib.head values) (lib.tail values);
      };
    };
    policy =
      if policies ? ${merge}
      then policies.${merge}
      else throw ''setOfNames: unknown "merge" policy "${merge}". The accepted values are "agreement", "union", and "intersection".'';
  in
    lib.seq policy (lib.mkOptionType {
      name = "setOfNames-${merge}";
      description = "set of names (${policy.phrase}), written as a non-empty list of strings; order and duplication carry no meaning";
      check = v: builtins.isList v && v != [] && lib.all builtins.isString v;
      merge = policy.combine;
    });
}

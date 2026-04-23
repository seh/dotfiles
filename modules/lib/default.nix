{lib, ...}: let
  flatMapAttrs = f: attrs:
    lib.foldlAttrs (
      acc: name: value:
        lib.warnIf (lib.hasAttr name acc) "flatMapAttrs: conflicting definitions for attribute ${name}" acc
        // f name value
    ) {}
    attrs;

  flattenAttrs = flatMapAttrs (
    name: value:
      if lib.isDerivation value || !lib.isAttrs value
      then {${name} = value;}
      else value
  );

  filterNonDrvAttrsRecursive = predicate:
    flatMapAttrs (
      name: value:
        lib.optionalAttrs (predicate name value) {
          "${name}" =
            if (lib.isAttrs value && !lib.isDerivation value)
            then filterNonDrvAttrsRecursive predicate value
            else value;
        }
    );

  collectLegacyPackages = attrs @ {pkgs, ...}: packagesFn: let
    autoCalledPkgs = self:
      lib.packagesFromDirectoryRecursive (
        {inherit (self) callPackage;} // lib.removeAttrs attrs ["pkgs"]
      );

    packagesFn' = self: {callPackages = lib.callPackagesWith (pkgs // self);} // packagesFn self;

    overlay = lib.flip (_: packagesFn');

    isAvailable = drv:
      lib.any (pred: pred drv) [
        (lib.meta.availableOn {inherit (pkgs.stdenv.hostPlatform) system;})
        (drv: !lib.isDerivation drv)
      ];

    allPackages = lib.makeScope pkgs.newScope (lib.extends overlay autoCalledPkgs);
  in
    filterNonDrvAttrsRecursive (_: isAvailable) allPackages;

  collectPackages = attrs: packagesFn: let
    allPackages = collectLegacyPackages attrs packagesFn;
  in
    lib.filterAttrs (_: lib.isDerivation) (flattenAttrs allPackages);

  # Cascade table describing, per role, the directed edges from an
  # aggregate name to the names it implies. The table is role-keyed
  # so that a future role (e.g. "bundles") becomes a purely additive
  # change: add a new top-level key alongside "profiles" / "features"
  # and extend "expandClosure" nowhere — the closure walk iterates
  # over whatever roles the table advertises.
  #
  # Evaluated per-host so that platform-conditional rules (e.g.
  # "desktop" implies "macos" only on Darwin hosts) can vary with
  # the host's framework.
  #
  # The graph across all roles MUST form a DAG; cycles are rejected
  # at runtime by "expandClosure" (and by "expandTagClosure", which
  # is a thin wrapper for the legacy flat surface).
  cascadesFor = {
    framework,
    isDarwin ? framework == "nixDarwin",
    ...
  }: {
    profiles = {
      all = {
        profiles = [
          "essential"
          "development"
          "desktop"
        ];
        features = [];
      };
      essential = {
        profiles = ["minimal"];
        features = [];
      };
      development = {
        profiles = [];
        features = ["language-servers"];
      };
      desktop = {
        profiles = ["fonts"] ++ lib.optional isDarwin "macos";
        features = [];
      };
    };
    features = {
      # Leaf role: no entries today. Feature-to-profile cascades are
      # forbidden; feature-to-feature cascades are possible in
      # principle but none exist today.
    };
  };

  # Project the typed cascade table down to the legacy flat shape
  # "{ <name> = [<name>...]; }", collapsing role labels. Used only to
  # back "expandTagClosure" with the original closure-walk
  # implementation so that pre-typed callers see bit-identical
  # behaviour.
  cascadesForTags = args: let
    typed = cascadesFor args;
  in
    lib.foldlAttrs (
      acc: _role: edges:
        acc // lib.mapAttrs (_name: targets: (targets.profiles or []) ++ (targets.features or [])) edges
    ) {}
    typed;

  # Expand a host's declared tag list into its transitive closure
  # under the given (flat) cascade table. Throws with a readable
  # message if the cascade table is cyclic.
  expandTagClosure = cascades: declared: let
    universe = lib.unique (lib.attrNames cascades ++ lib.concatLists (lib.attrValues cascades));
    sorted = lib.lists.toposort (a: b: builtins.elem b (cascades.${a} or [])) universe;
    closure = builtins.genericClosure {
      startSet = map (tag: {key = tag;}) declared;
      operator = {key, ...}: map (t: {key = t;}) (cascades.${key} or []);
    };
  in
    if sorted ? cycle
    then
      throw ''
        Tag cascade contains a cycle involving: ${lib.concatStringsSep ", " sorted.loops}.
        The cascade table must form a DAG.
      ''
    else map (x: x.key) closure;

  # Role-parametric transitive closure over the typed cascade table.
  # Takes the record produced by "cascadesFor" and a seed
  # "{ profiles = [...]; features = [...]; }" (or any other roles
  # the cascade advertises), and returns a record of the same shape
  # with each list expanded to its transitive closure.
  #
  # The implementation iterates over "builtins.attrNames cascades"
  # rather than hard-coding role names, so adding a new role is a
  # pure data-level change.
  expandClosure = cascades: seed: let
    roles = builtins.attrNames cascades;
    # Map each role to a single-character key prefix used inside
    # "builtins.genericClosure". Two roles must not share a prefix.
    rolePrefix = role: builtins.substring 0 1 role;
    prefixes = map rolePrefix roles;
    _prefixCollision =
      if lib.length (lib.unique prefixes) != lib.length prefixes
      then
        throw ''
          Cascade role names must have distinct first characters;
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
    # Toposort over the combined universe of (role, name) pairs so
    # that cycles anywhere in the graph are rejected.
    universeList =
      lib.concatMap (
        role: let
          edges = cascades.${role};
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
    universe = lib.unique (
      map (
        {
          role,
          name,
        }:
          mkKey role name
      )
      universeList
    );
    edgesFrom = key: let
      p = parseKey key;
      targets = cascades.${p.role}.${p.name} or {};
    in
      lib.concatMap (targetRole: map (n: mkKey targetRole n) (targets.${targetRole} or [])) roles;
    sorted = lib.lists.toposort (a: b: builtins.elem b (edgesFrom a)) universe;
    # Build the seed set by mapping each role's seed list through
    # "mkKey". Missing roles in "seed" default to [].
    startSet = lib.concatMap (role: map (n: {key = mkKey role n;}) (seed.${role} or [])) roles;
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
      lib.seq _prefixCollision (throw ''
        Cascade contains a cycle involving: ${lib.concatStringsSep ", " sorted.loops}.
        The cascade table must form a DAG across all roles.
      '')
    else lib.seq _prefixCollision grouped;
in {
  flake.lib = {
    inherit
      cascadesFor
      cascadesForTags
      collectLegacyPackages
      collectPackages
      expandClosure
      expandTagClosure
      filterNonDrvAttrsRecursive
      flatMapAttrs
      flattenAttrs
      ;
  };
}

{lib}: let
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
  # at runtime by "expandClosure".
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

  # Role-parametric transitive closure over the typed cascade table.
  # Takes the record produced by "cascadesFor", a "knownByRole"
  # attrset naming the identifiers each role advertises (keyed by
  # role name, value is a list of known names), and a seed
  # "{ profiles = [...]; features = [...]; }" (or any other roles
  # the cascade advertises). Returns a record of the same shape
  # with each list expanded to its transitive closure.
  #
  # The implementation iterates over "builtins.attrNames cascades"
  # rather than hard-coding role names, so adding a new role is a
  # pure data-level change.
  #
  # Two static checks run before the closure walk:
  #   1. Every edge target named in "cascades.<role>.<source>"
  #      under a "<targetRole>" key must appear in
  #      "knownByRole.<targetRole>". Dangling edges (typically
  #      typos) are rejected with a message that names the edge.
  #   2. Edges emanating from a feature source may not target
  #      profiles. Profiles are coarser than features; reversing
  #      the hierarchy would render host records misleading. This
  #      is the one role-specific rule inside otherwise
  #      role-parametric machinery.
  expandClosure = cascades: knownByRole: seed: let
    roles = builtins.attrNames cascades;
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
          edgeRecord = cascades.${role};
          sources = builtins.attrNames edgeRecord;
        in
          lib.concatMap (
            source: let
              targetsByRole = edgeRecord.${source};
              targetRoles = builtins.attrNames targetsByRole;
              featureToProfileError = lib.optional (role == "features" && builtins.elem "profiles" targetRoles) ''
                cascadesFor: feature "${source}" has a "profiles" edge field, but features may not cascade to profiles. Remove the field or promote "${source}" to a profile.
              '';
              danglingErrors =
                lib.concatMap (
                  targetRole: let
                    known = knownByRole.${targetRole} or [];
                    targets = targetsByRole.${targetRole};
                    missing = lib.filter (t: !(builtins.elem t known)) targets;
                  in
                    map (t: ''
                      cascadesFor: dangling edge ${role}.${source} -> ${targetRole}.${t} (no such ${targetRole} is known via "flake.known${capitalize targetRole}")
                    '')
                    missing
                )
                targetRoles;
            in
              featureToProfileError ++ danglingErrors
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
      lib.seq _prefixCollision (
        lib.seq _edgeCheck (throw ''
          Cascade contains a cycle involving: ${lib.concatStringsSep ", " sorted.loops}.
          The cascade table must form a DAG across all roles.
        '')
      )
    else lib.seq _prefixCollision (lib.seq _edgeCheck grouped);
in {
  inherit
    cascadesFor
    collectLegacyPackages
    collectPackages
    expandClosure
    filterNonDrvAttrsRecursive
    flatMapAttrs
    flattenAttrs
    ;
}

{lib}: let
  # Cascade table describing, per role, the directed edges from an
  # aggregate name to the names it implies. The table is role-keyed
  # so that a future role (e.g. "bundles") becomes a purely additive
  # change: add a new top-level key alongside "profiles" / "features"
  # and extend "expandClosure" nowhere — the closure walk iterates
  # over whatever roles the table advertises.
  #
  # Evaluated per-host so that the per-profile platform filtering
  # described below can vary with the host's platform.
  #
  # Every entry's "profiles" list is filtered against the profiles'
  # declared "supportedPlatforms" (see the "mkProfile" function): a profile the
  # host's platform does not support never arrives through the
  # cascade, whether via the "all" entry or via a narrower one such
  # as "desktop". The assertion in "modules/_assertions.nix" rejects
  # a host that would activate such a profile anyway (e.g. by
  # declaring it directly).
  #
  # The "all" umbrella's targets are computed from "knownProfiles"
  # rather than enumerated, so that introducing a new profile folds
  # it into "all" automatically. Hosts that want to skip a specific
  # umbrella member list it under "excludeProfiles".
  #
  # The graph across all roles MUST form a DAG; cycles are rejected
  # at runtime by "expandClosure".
  cascadesFor = {
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
    profileEntries = {
      all = {
        profiles = lib.subtractLists ["all"] knownProfiles;
        features = [];
      };
      essential = {
        profiles = ["minimal"];
        features = [
          "bash"
          "dev/difftastic"
          "editor/emacs"
          "gnupg"
          "kitty"
          "lang/markdown"
          "nh"
          "nix"
          "nushell"
          "shell"
          "ssh"
          "vcs/git"
          "vcs/jjui"
          "vcs/jujutsu"
          "zsh"
        ];
      };
      development = {
        profiles = [];
        features = [
          "cloud/aws"
          "cloud/azure"
          "cloud/gcp"
          "coder"
          "dev/editorconfig"
          "dev/language-servers"
          "editor/helix"
          "kubernetes"
          "lang/lua"
          "lang/rust"
          "model-agent/claude"
          "model-agent/copilot"
          "model-agent/opencode"
          "vcs/commit-signing"
        ];
      };
      desktop = {
        # NB: The per-entry platform filtering above keeps "apps"
        # and "macos" from reaching hosts whose platforms they do
        # not support.
        profiles = [
          "apps"
          "fonts"
          "macos"
        ];
      };
      web = {
        profiles = [];
        features = ["web/firefox"];
      };
    };
  in {
    profiles = lib.mapAttrs (_name: filterEntryProfiles) profileEntries;
    features = {
      # NB: Feature-to-profile cascades are forbidden, but
      # feature-to-feature cascades are allowed.
      "vcs/jjui" = {
        features = ["vcs/jujutsu"];
      };
    };
  };

  # Role-parametric transitive closure over the typed cascade table.
  # Takes the record produced by "cascadesFor", a "knownByRole"
  # attrset naming the identifiers each role advertises (keyed by
  # role name, value is a list of known names), and a seed
  # "{ profiles = [...]; features = [...]; }" (or any other roles
  # the cascade advertises). Returns a record of the same form
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
  # Delete a set of vertices from the cascade table. Excluded
  # vertices lose their out-edges (their adjacency entries are
  # removed) and their in-edges (every surviving adjacency list is
  # filtered to drop them as targets). The result is a cascade
  # record of the same form, suitable for passing to
  # "expandClosure".
  #
  # Excluding a profile (or feature) is therefore equivalent to
  # deleting that vertex from the DAG: anything reachable only
  # through the excluded vertex falls out of the closure
  # automatically, without the consumer having to enumerate the
  # downstream vertices in "excludeFeatures".
  pruneCascades = cascades: excluded: let
    isExcluded = role: name: builtins.elem name (excluded.${role} or []);
    withoutExcludedSources =
      lib.mapAttrs (
        role: edges: lib.filterAttrs (name: _: !(isExcluded role name)) edges
      )
      cascades;
    pruneTargets = targetsByRole:
      lib.mapAttrs (
        targetRole: targetNames: lib.filter (n: !(isExcluded targetRole n)) targetNames
      )
      targetsByRole;
  in
    lib.mapAttrs (_role: edges: lib.mapAttrs (_source: pruneTargets) edges) withoutExcludedSources;
in {
  inherit
    cascadesFor
    expandClosure
    pruneCascades
    ;
}

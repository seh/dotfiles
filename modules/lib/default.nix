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

  # Cascade table mapping an "aggregate" tag to the set of tags it
  # implies. Evaluated per-host so that platform-conditional rules
  # (e.g. "desktop" implies "macos" only on Darwin hosts) can vary
  # with the host's framework.
  #
  # The table MUST form a DAG over the tag namespace; cycles are
  # rejected at runtime by "expandTagClosure".
  cascadesFor = {
    framework,
    isDarwin ? framework == "nixDarwin",
    ...
  }: {
    all = [
      "essential"
      "development"
      "desktop"
    ];
    essential = ["minimal"];
    development = ["language-servers"];
    desktop = ["fonts"] ++ lib.optional isDarwin "macos";
  };

  # Expand a host's declared tag list into its transitive closure
  # under the given cascade table. Throws with a readable message if
  # the cascade table is cyclic.
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
in {
  flake.lib = {
    inherit
      cascadesFor
      collectLegacyPackages
      collectPackages
      expandTagClosure
      filterNonDrvAttrsRecursive
      flatMapAttrs
      flattenAttrs
      ;
  };
}

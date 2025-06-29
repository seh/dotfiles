{ lib, ... }:

let
  flatMapAttrs =
    f: attrs:
    lib.foldlAttrs (
      acc: name: value:
      lib.warnIf (lib.hasAttr name acc) "flatMapAttrs: conflicting definitions for attribute ${name}" acc
      // f name value
    ) { } attrs;

  flattenAttrs = flatMapAttrs (
    name: value: if lib.isDerivation value || !lib.isAttrs value then { ${name} = value; } else value
  );

  filterNonDrvAttrsRecursive =
    predicate:
    flatMapAttrs (
      name: value:
      lib.optionalAttrs (predicate name value) {
        "${name}" =
          if (lib.isAttrs value && !lib.isDerivation value) then
            filterNonDrvAttrsRecursive predicate value
          else
            value;
      }
    );

  collectLegacyPackages =
    attrs@{ pkgs, ... }:
    packagesFn:
    let
      autoCalledPkgs =
        self:
        lib.packagesFromDirectoryRecursive (
          { inherit (self) callPackage; } // lib.removeAttrs attrs [ "pkgs" ]
        );

      packagesFn' = self: { callPackages = lib.callPackagesWith (pkgs // self); } // packagesFn self;

      overlay = lib.flip (_: packagesFn');

      isAvailable =
        drv:
        lib.any (pred: pred drv) [
          (lib.meta.availableOn { inherit (pkgs.stdenv.hostPlatform) system; })
          (drv: !lib.isDerivation drv)
        ];

      allPackages = lib.makeScope pkgs.newScope (lib.extends overlay autoCalledPkgs);
    in
    filterNonDrvAttrsRecursive (_: isAvailable) allPackages;

  collectPackages =
    attrs: packagesFn:
    let
      allPackages = collectLegacyPackages attrs packagesFn;
    in
    lib.filterAttrs (_: lib.isDerivation) (flattenAttrs allPackages);

  #importDir = dir: lib.mapAttrsToList (path: _: lib.path.append dir path) (builtins.readDir dir);
  importDir =
    dir:
    let
      nixFileNames = lib.attrsets.attrNames (
        lib.attrsets.filterAttrs (
          name: type: type == "directory" || (type == "regular" && lib.hasSuffix ".nix" name)
        ) (builtins.readDir dir)
      );
    in
    builtins.map (name: lib.path.append dir name) nixFileNames;

  importDirs = lib.concatMap importDir;
in
{
  flake.lib = {
    inherit
      collectLegacyPackages
      collectPackages
      filterNonDrvAttrsRecursive
      flatMapAttrs
      flattenAttrs
      importDir
      importDirs
      ;
  };
}

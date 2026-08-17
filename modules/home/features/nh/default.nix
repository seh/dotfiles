{flakeLib, ...}:
flakeLib.mkFeature "nh" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.nh =
      {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.nh;
        clean = {
          enable = lib.mkDefault true;
          extraArgs = "--optimize";
        };
      }
      // (
        let
          hostName = "local";
        in {
          darwinFlake = lib.mkDefault (".#darwinConfigurations." + hostName);
          osFlake = lib.mkDefault (".#nixosConfigurations." + hostName);
        }
      );
  };
}

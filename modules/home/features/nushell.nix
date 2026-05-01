{flakeLib, ...}:
flakeLib.mkFeature "nushell" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.nushell = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.nushell;
      plugins = with pkgs.nushellPlugins; [
        formats
        gstat
        hcl
        #highlight # TODO(seh): This one is not building correctly for now.
        #net
        polars
        query
        #semver # TODO(seh): This one is not available for Darwin atop x86-64 for now.
        #units
      ];
    };
    home.packages = with pkgs; [
      nufmt
    ];
  };
}

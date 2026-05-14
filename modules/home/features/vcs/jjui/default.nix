{flakeLib, ...}:
flakeLib.mkFeature "vcs/jjui" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.jjui = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.jjui;
      settings = {
        bindings = [
          {
            action = "revisions.inline_describe.accept";
            key = "enter";
            scope = "revisions.inline_describe";
          }
          {
            action = "revisions.inline_describe.new_line";
            key = "shift+enter";
            scope = "revisions.inline_describe";
          }
        ];
        ui = {
          tracer = {
            enabled = true;
          };
        };
      };
      configLua = ./config.lua;
    };
  };
}

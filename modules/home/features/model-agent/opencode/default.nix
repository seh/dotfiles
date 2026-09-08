{flakeLib, ...}:
flakeLib.mkFeature "model-agent/opencode" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.opencode = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.opencode;
      settings = {
        autoupdate = false;
        default_agent = "plan";
        formatter = {
          alejandra = {
            command = [
              (lib.getExe pkgs.alejandra)
              "--quiet"
              "$FILE"
            ];
            extensions = [".nix"];
          };
          gofumpt = {
            command = [
              (lib.getExe pkgs.gofumpt)
              "$FILE"
            ];
            extensions = [".go"];
          };
          zig = {
            command = [
              (lib.getExe pkgs.zig)
              "fmt"
              "$FILE"
            ];
            extensions = [
              ".zig"
              ".zon"
            ];
          };
        };
      };
      skills = lib.genAttrs [
        "catch-up-on-recent-jj-changes"
        "close-bazel-drift"
      ] (name: ./skills/${name});
      tui = {
        scroll_acceleration = {
          enabled = true;
        };
      };
    };
  };
}

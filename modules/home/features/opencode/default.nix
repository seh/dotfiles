{flakeLib, ...}:
flakeLib.mkFeature "opencode" {
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

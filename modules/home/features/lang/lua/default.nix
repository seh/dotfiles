{flakeLib, ...}:
flakeLib.mkFeature "lang/lua" {
  homeManager = {
    options = {lib, ...}: {
      options.dotfiles.lua.styluaConfigFile = lib.mkOption {
        type = lib.types.path;
        default = ./stylua.toml;
        description = ''
          Path to the canonical "stylua.toml" for this user. The
          "lang/lua" feature deploys it as
          "~/.config/stylua/stylua.toml" and the Claude Code hook
          passes it to "stylua" via "--config-path". Hosts may set
          this to a host-local file.
        '';
      };
    };
    config = {
      config,
      pkgs,
      ...
    }: {
      home.packages = with pkgs; [
        emmylua-ls
        stylua
      ];
      xdg.configFile."stylua/stylua.toml".source = config.dotfiles.lua.styluaConfigFile;
    };
  };
}

{flakeLib, ...}:
# The Lua toolchain, active when this configuration expresses the
# "lang/lua" interest: the "stylua" formatter, whose configuration
# file this feature deploys. The "lang/lua/ls" feature installs the
# language server.
flakeLib.mkFeature "lang/lua/tools" {
  preconditions = ["lang/lua"];
  homeManager = {
    options = {lib, ...}: {
      options.dotfiles.lang.lua.tools.styluaConfigFile = lib.mkOption {
        type = lib.types.path;
        default = ./stylua.toml;
        description = ''
          Path to the canonical "stylua.toml" file for this user. The
          "lang/lua/tools" feature deploys it as the
          "~/.config/stylua/stylua.toml" file and the Claude Code hook
          passes it to the "stylua" program via the "--config-path"
          flag. Hosts may set this to a host-local file.
        '';
      };
    };
    config = {
      config,
      pkgs,
      ...
    }: {
      home.packages = with pkgs; [
        stylua
      ];
      xdg.configFile."stylua/stylua.toml".source = config.dotfiles.lang.lua.tools.styluaConfigFile;
    };
  };
}

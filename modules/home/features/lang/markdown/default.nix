{flakeLib, ...}:
flakeLib.mkFeature "lang/markdown" {
  homeManager = {
    options = {lib, ...}: {
      options.dotfiles.markdown.rumdlConfigFile = lib.mkOption {
        type = lib.types.path;
        default = ./rumdl.toml;
        description = ''
          Path to the canonical "rumdl.toml" for this user. The
          "lang/markdown" feature deploys it at
          "~/.config/rumdl.toml". Hosts may set this to a host-local
          file to override the default configuration.
        '';
      };
    };
    config = {
      config,
      pkgs,
      ...
    }: {
      home = {
        packages = [pkgs.rumdl];
        # NB: Use "home.file" rather than "xdg.configFile" because
        # "rumdl" walks parent directories looking literally for
        # ".config/rumdl.toml" and does not consult the
        # "XDG_CONFIG_HOME" environment variable. Were
        # "XDG_CONFIG_HOME" ever set to a non-default value,
        # "xdg.configFile" would place this file where "rumdl" would
        # never look.
        file.".config/rumdl.toml".source = config.dotfiles.markdown.rumdlConfigFile;
      };
    };
  };
}

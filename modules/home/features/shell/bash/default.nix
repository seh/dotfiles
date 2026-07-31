{flakeLib, ...}:
flakeLib.mkFeature "shell/bash" {
  homeManager = _: {
    # TODO(seh): Elaborate this configuration.
    programs.bash = {
      enable = true;
    };
  };
}

{flakeLib, ...}:
flakeLib.mkFeature "bash" {
  homeManager = _: {
    # TODO(seh): Elaborate this configuration.
    programs.bash = {
      enable = true;
    };
  };
}

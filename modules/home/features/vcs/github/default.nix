{flakeLib, ...}:
flakeLib.mkFeature "vcs/github" {
  homeManager = _: {
    programs.gh = {
      enable = true;
    };
  };
}

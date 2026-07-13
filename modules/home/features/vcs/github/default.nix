{flakeLib, ...}:
flakeLib.mkFeature "vcs/github" {
  homeManager = {...}: {
    programs.gh = {
      enable = true;
    };
  };
}

{flakeLib, ...}:
flakeLib.mkFeature "vcs/github" {
  # GitHub's command-line tool works against Git repositories, so
  # it brings along the "vcs/git" feature.
  implies = ["vcs/git"];

  homeManager = _: {
    programs.gh = {
      enable = true;
    };
  };
}

{flakeLib, ...}:
flakeLib.mkFeature "vcs/github" {
  # GitHub's command-line tool works against Git repositories, so it
  # implies the "vcs/git" feature.
  implies = ["vcs/git"];

  homeManager = {...}: {
    programs.gh = {
      enable = true;
    };
  };
}

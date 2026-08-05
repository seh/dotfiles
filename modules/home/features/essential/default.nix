{flakeLib, ...}:
flakeLib.mkFeature "essential" {
  # The essential bundle brings along the minimal bundle and the
  # baseline features every managed home wants, plus the machine-side
  # features a managed machine wants: its login shell's integration
  # and its SSH daemon. The implication graph is class-agnostic, so a
  # machine that selects this bundle activates those machine-side
  # features and applies their system bodies, while a user who selects
  # it receives the home bodies alone.
  implies = [
    "minimal"
    "dev/difftastic"
    "editor/emacs"
    "essential/tools"
    "gnupg"
    "kitty"
    "net/ssh-daemon"
    "nh"
    "nix"
    "shell/bash"
    "shell/nushell"
    "shell/zsh"
    "shell/zsh/integration"
    "ssh"
    "vcs/git"
    "vcs/jjui"
    "vcs/jujutsu"
  ];
}

# The machine's zsh integration: a machine-only feature that asserts
# "programs.zsh.enable" so that NixOS installs zsh, writes
# "/etc/zshrc", lists zsh among "/etc/shells", and a login shell
# loads the system environment.
#
# The machine alone nominates this feature. The "essential" profile
# implies it, and only the machine's own selections decide a nixOS
# body, so a user who selects "essential" does not bring it along.
# The "shell/zsh" feature configures a user's own zsh and stays
# independent of this one—a user's zsh still loads and reads that
# configuration wherever this feature is absent.
#
# This name carries a nix-darwin body as well, in
# "modules/nix-darwin/features/shell/zsh/integration/default.nix". The
# concern is the same on either system class, so one name selected
# once serves whichever class the machine runs, the way the "nix"
# feature spans both.
{flakeLib, ...}:
flakeLib.mkFeature "shell/zsh/integration" {
  nixOS = _: {
    programs.zsh.enable = true;
  };
}

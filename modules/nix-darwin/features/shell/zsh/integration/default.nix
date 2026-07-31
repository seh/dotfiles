# The machine's zsh integration: a machine-only feature that asserts
# the "programs.zsh.enable" option so that nix-darwin writes the
# "/etc/zshrc" file and a login shell loads the nix-darwin
# environment.
#
# The machine alone nominates this feature. The "essential" profile
# implies it, and only the machine's own selections decide a
# nix-darwin body, so a user who selects "essential" does not bring it
# along. The "shell/zsh" feature configures a user's own zsh and stays
# independent of this one—a user's zsh still loads and reads that
# configuration wherever this feature is absent.
#
# nix-darwin defaults the "programs.zsh.enable" option to true today,
# so this assignment changes nothing on its own. It pins behavior this
# configuration depends on: the "/etc/zshrc" file must exist.
# Inheriting that from an upstream default leaves the dependence
# unwritten, and writing it down here is exactly what the split of
# this feature from the "shell/zsh" feature corrects.
#
# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/98f983380770d6f6d33a828f41f3656adeb4e9a7/nix/darwin/modules/shell.nix
{flakeLib, ...}:
flakeLib.mkFeature "shell/zsh/integration" {
  nixDarwin = {pkgs, ...}: {
    # Create /etc/zshrc that loads the nix-darwin environment.
    # See https://github.com/LnL7/nix-darwin/issues/177.
    programs.zsh.enable = true;

    environment.systemPackages = with pkgs; [
      # TODO(seh): Do we need any of these?
    ];

    # TODO(seh): Consider adapting more of the example configuration,
    # such as environment variables and system defaults.
  };
}

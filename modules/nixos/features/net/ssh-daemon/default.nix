# The machine's SSH daemon: a machine-only feature that accepts
# inbound SSH connections with root logins and password authentication
# both turned off, so every session authenticates with a key as an
# ordinary user.
#
# Whether a nixOS body applies depends on the "dotfiles.host.features"
# set alone, never on a user's own "features" set, so this feature
# takes effect when that set contains it or contains a feature that
# implies it, as "remote-terminal-host" does. The "ssh" feature
# configures a user's own SSH client—the outbound half—and stays
# independent of this one.
{flakeLib, ...}:
flakeLib.mkFeature "net/ssh-daemon" {
  nixOS = _: {
    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };
  };
}

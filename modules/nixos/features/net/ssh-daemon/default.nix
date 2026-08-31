# The machine's SSH daemon: a machine-only feature that accepts
# inbound SSH connections with root logins and password authentication
# both turned off, so every session authenticates with a key as an
# ordinary user.
#
# The machine alone nominates this feature. The "essential" feature
# implies it, and only the machine's own selections decide a nixOS
# body, so a user who selects "essential" does not activate it. The
# "ssh" feature configures a user's own SSH client—the outbound
# half—and stays independent of this one.
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

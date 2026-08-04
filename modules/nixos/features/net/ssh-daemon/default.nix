# The machine's SSH daemon: a machine-only feature that accepts
# inbound SSH connections while refusing both root logins and
# password authentication, so every session arrives as an ordinary
# user holding a key.
#
# The machine alone nominates this feature. The "essential" profile
# implies it, and only the machine's own selections decide a nixOS
# body, so a user who selects "essential" does not bring it along.
# The "ssh" feature configures a user's own SSH client—the outbound
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

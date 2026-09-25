# A machine that people use from a terminal on another computer, by
# whatever channel: SSH on a Coder workspace, or OrbStack's own
# channel on a virtual machine.
{flakeLib, ...}:
flakeLib.mkFeature "remote-terminal-host" {
  # Such a machine accepts sessions over SSH. A machine that people
  # use through another channel alone excludes this feature in its
  # host record.
  implies = [
    "net/ssh-daemon"
  ];

  nixOS = {pkgs, ...}: {
    # A session from kitty announces the terminal type "xterm-kitty",
    # which a program looks up in the terminfo database. Only the
    # "kitty" feature installs kitty's own package, so this package
    # supplies the entry alone.
    environment.systemPackages = [
      pkgs.kitty.terminfo
    ];
  };
}

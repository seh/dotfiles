{flakeLib, ...}:
flakeLib.mkFeature "dev/emulation" {
  # OrbStack is installed on Darwin hosts alone below, while this
  # toleration applies to every instantiation.
  unfreePackages = ["orbstack"];

  homeManager = {
    lib,
    pkgs,
    ...
  }: let
    inherit (lib) optionals;
    inherit (pkgs.stdenv.hostPlatform) isDarwin;
  in {
    home.packages = with pkgs;
      [
        # QEMU is a general-purpose machine emulator available on
        # every platform, and it additionally serves as Podman's
        # virtual-machine backend on macOS.
        qemu
      ]
      ++ optionals isDarwin [
        orbstack
      ];

    programs.zsh.profileExtra =
      lib.mkIf isDarwin
      # Preserve the line that OrbStack installs in this file for its
      # own integration when Home Manager writes the rest of the file
      # for us.
      ''
        source ~/.orbstack/shell/init.zsh 2>/dev/null || :
      '';
  };
}

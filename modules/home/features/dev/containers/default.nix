{
  flakeLib,
  lib,
  ...
}:
flakeLib.mkFeature "dev/containers" {
  # Podman uses QEMU as its virtual-machine backend on macOS, and
  # the "dev/emulation" feature provides QEMU; selecting containers
  # on a Darwin host must therefore bring emulation along. On Linux
  # podman runs natively and needs no such edge.
  implies = [
    {
      name = "dev/emulation";
      supportedPlatforms = lib.platforms.darwin;
    }
  ];

  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      podman
    ];
  };
}

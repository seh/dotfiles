# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/main/nix/packages/nix-darwin.nix
{ lib, stdenv, runCommand, nix-darwin, nix }:

let
  # Create a nix-darwin configuration that includes almost nothing
  # other than nix-darwin itself.
  tinyConfig = {
    nix.package = nix;
    documentation.info.enable = false;
    programs.bash.enable = false;
  };

  tinyDarwin = nix-darwin.lib.darwinSystem {
    inherit (stdenv.hostPlatform) system;
    modules = [ tinyConfig ];
  };

  tinySystem = tinyDarwin.system;
in
runCommand "nix-darwin"
{
  meta = {
    description = "Command line tool for nix-darwin";
    longDescription = ''
      Provide command line tools from nix-darwin. This package is mainly useful
      for bootstrapping nix-darwin.
    '';
    homepage = "https://github.com/LnL7/nix-darwin";
    license = with lib.licenses; [ mit ];
    platforms = lib.platforms.darwin;
  };
} ''
  mkdir -p "$out/bin" \
           "$out/share/man/man5"
  for sub in help option rebuild; do
    relpath="bin/darwin-''${sub}"
    ln -s "${tinySystem}/sw/''${relpath}" \
          "$out/''${relpath}"
  done
  relpath='share/man/man5/configuration.nix.5'
  ln -s "${tinySystem}/sw/''${relpath}" \
        "$out/''${relpath}"
''

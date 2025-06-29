{ inputs }@localFlake:
{ lib, config, ... }@flake:

let
  # TODO(seh): Use this if we need to refer to any of the current
  # values for these options.
  #cfg = flake.config.dotfiles;
  #nixosRelease = localFlake.inputs.nixos.lib.trivial.release;
in
{
  options.dotfiles = {
    user = {
      email = lib.mkOption {
        type = lib.types.str;
        description = "The default email address to use throughout this flake.";
        default = "seh@panix.com";
      };

      fullName = lib.mkOption {
        type = lib.types.str;
        description = "The default full name to use throughout this flake.";
        default = "Steven E. Harris";
      };

      gpgKey = lib.mkOption {
        type = lib.types.str;
        description = "The default GPG key to use throughout this flake.";
        default = "";
      };

      name = lib.mkOption {
        type = lib.types.str;
        description = "The default username to use throughout this flake.";
        default = "seh";
      };
    };

    darwin.modules = lib.mkOption {
      type = with lib.types; listOf deferredModule;
      default = [ ];
      description = ''
        Default list of modules to include when generating nix-darwin
        configuration with `lib.mkDarwin` and `lib.importDarwin`.
      '';
    };

    home.modules = lib.mkOption {
      type = with lib.types; listOf deferredModule;
      default = [ ];
      description = ''
        Default list of modules to include when generating Home Manager
        configuration with `lib.mkHome` and `lib.importHome`.
      '';
    };

    nix.package = lib.mkOption {
      type = lib.types.str;
      description = "The Nix package to use, specified by attribute name.";
      default = "latest"; # Example alternative: "nix_2_24"
    };

    nixos.modules = lib.mkOption {
      type = with lib.types; listOf deferredModule;
      default = [ ];
      description = ''
        Default list of modules to include when generating NixOS configuration
        with `lib.mkNixOS` and `lib.importNixOS`.
      '';
    };
  };
}

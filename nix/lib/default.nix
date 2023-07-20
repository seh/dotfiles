# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/lib/default.nix
{ inputs }:

let
  inherit (inputs) self nix-darwin flake-utils home-manager nixpkgs;
  inherit (home-manager.lib) homeManagerConfiguration;
  inherit (nixpkgs.lib) importTOML;
  inherit (nix-darwin.lib) darwinSystem;

  nixpkgsArgs = {
    config = import ../../files/.config/nixpkgs/config.nix;
    overlays = [ self.overlays.default ];
  };
in
rec {
  config = importTOML ../config.toml;

  # Wrapper function for creating a Nixpkgs package set that includes
  # the dotfiles overlays and unfree packages.
  #
  # Example:
  #   mkPkgs nixpkgs { }
  #   mkPkgs { system = "x86_64-linux"; }
  mkPkgs = pkgs:
    { system ? config.os.system
    , config ? { }
    , overlays ? [ ]
    , ...
    } @ args:
    import pkgs (args // {
      inherit system;
      config = nixpkgsArgs.config // config;
      overlays = nixpkgsArgs.overlays ++ overlays;
    });

  eachSystemPkgs = systems: f: flake-utils.lib.eachSystem systems (
    system:
    let
      pkgs = mkPkgs inputs.nixpkgs { inherit system; };
    in
    f { inherit system pkgs; }
  );

  supportedPlatforms = [
    "aarch64-darwin"
    "aarch64-linux"
    "x86_64-darwin"
    "x86_64-linux"
  ];

  eachSupportedSystemPkgs = eachSystemPkgs supportedPlatforms;


  # TODO(seh): Consider defining "mkHome" and "importHome" functions.
  mkHome =
    { system ? config.os.system
    , pkgs ? mkPkgs nixpkgs { inherit system; }
    , modules ? [ ]
    , ...
    } @ args:
    let hmArgs = builtins.removeAttrs args [ "system" ];
    in
    homeManagerConfiguration (hmArgs // {
      inherit pkgs;
      modules = modules ++ [
        self.homeModules.default
        ({ lib, ... }: {
          home = {
            username = lib.mkDefault config.user.name;
            homeDirectory = lib.mkDefault config.user.homeDirectory;
            stateVersion = lib.mkDefault config.user.stateVersion;
          };
        })
      ];
    });

  importHome = configPath: args:
    mkHome (args // { modules = [ (import configPath) ]; });

  mkDarwin = { modules ? [ ], system ? config.os.darwin.system, ... } @ args:
    darwinSystem (args // {
      inherit system;
      modules = modules ++ [
        self.darwinModules.default
        home-manager.darwinModules.default
        ({ lib, ... }:
          {
            system.stateVersion = lib.mkDefault config.os.darwin.stateVersion;
            nixpkgs = nixpkgsArgs;
            home-manager = {
              useGlobalPkgs = true;
              sharedModules = [
                self.homeModules.default
                { home.stateVersion = lib.mkDefault config.user.stateVersion; }
              ];
            };
          })
      ];
    });

  importDarwin = configPath: args:
    mkDarwin (args // { modules = [ (import configPath) ]; });
}

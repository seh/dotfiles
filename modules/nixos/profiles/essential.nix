{
  flake.profileModules.nixOS.essential = {
    config,
    lib,
    ...
  }: let
    inherit (config.dotfiles._resolved) hasTag;
  in {
    config = lib.mkMerge [
      {
        dotfiles._knownTags = ["essential"];
      }
      (lib.mkIf (hasTag "essential") {
        programs.zsh.enable = true;

        services.openssh = {
          enable = true;
          settings = {
            PermitRootLogin = "no";
            PasswordAuthentication = false;
          };
        };
      })
    ];
  };
}

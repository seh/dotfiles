{
  flake.knownProfiles = ["essential"];

  flake.profileModules.nixOS.essential = {
    config,
    lib,
    ...
  }: let
    inherit (config.dotfiles._host) activatesProfile;
  in {
    config = lib.mkIf (activatesProfile "essential") {
      programs.zsh.enable = true;

      services.openssh = {
        enable = true;
        settings = {
          PermitRootLogin = "no";
          PasswordAuthentication = false;
        };
      };
    };
  };
}

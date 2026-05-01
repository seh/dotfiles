{flakeLib, ...}:
flakeLib.mkProfile "essential" {
  nixOS = _: {
    programs.zsh.enable = true;

    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };
  };
}

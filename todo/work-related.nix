{ pkgs, ... }: {
  imports = [
    # TODO(seh): Include more program-specific import entries.
  ];

  home = {
    stateVersion = "21.05";
    packages = with pkgs; [
      aws-vault
      awscli2
      terraform
      # NB: tfenv is not available as a package.
      yubikey-manager
    ];
  };
}

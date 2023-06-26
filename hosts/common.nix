# Basis of inspiration:
#   https://github.com/sebastiant/dotfiles/blob/master/hosts/common.nix
{ pkgs, ... }: {
  imports = [
    ../programs/zsh/zsh.nix
    # TODO(seh): Include more program-specific import entries.
  ];

  home = {
    stateVersion = "21.05";
    packages = with pkgs; [
      # TODO(seh): Consider moving some of these into separate files
      # depending on which kinds of machines should include them
      # (e.g. personal v. work.
      _1password
      age
      bazel # TODO(seh): Sholud we install this directly?
      bazel-buildtools
      bazelisk
      coreutils
      cue
      dig
      elvish
      file
      gnupg
      go
      go-jsonnet
      gopls
      jq
      kubectl
      kustomize
      nixfmt
      openssl
      pinentry_mac
      sbcl
      shellcheck
      sops
      sqlite
      tree
      unzip
      vim
      wget
      whois
      yq-go
    ];
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "Monokai Extended Light";
    };
  };
  programs.direnv = {
    enable = true;
    nix-direnv = { enable = true; };
  };
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };
  programs.fzf = {
    enable = true;
    defaultOptions = [
      "--info=inline"
      "--bind=ctrl-r:toggle-sort"
    ];
  };
  programs.gpg = {
    enable = true;
  };
  programs.k9s = {
    enable = true;
    # TODO(seh): Configure settings.
  };
  programs.go = {
    enable = true;
  };

  services.emacs = {
    defaultEditor = true;
  };
  # TODO(seh): The "gpg-agent" service is only supported on Linux for now.
  # See:
  #   https://github.com/nix-community/home-manager/issues/91
  #   https://github.com/nix-community/home-manager/issues/3864
  # services.gpg-agent = {
  #   enable = true;
  #   enableSshSupport = true;
  #   defaultCacheTtl = 600;
  #   maxCacheTtl = 7200;
  #   pinentryFlavor = "emacs";     # TODO(seh): See https://github.com/NixOS/nixpkgs/issues/240819 for using "pinentry-mac".
  # };
}

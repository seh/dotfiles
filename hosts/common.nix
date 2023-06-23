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
      cue
      dig
      elvish
      file
      gopls
      gnupg
      jq
      # TODO(seh): Figure out why this won't build.
      #jsonnet
      kubectl
      kustomize
      nixfmt
      openssl
      sbcl
      shellcheck
      sops
      sqlite
      tree
      unzip
      vim
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
  programs.fzf.enable = true;
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
}

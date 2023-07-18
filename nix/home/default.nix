{ inputs }: { lib, ... }:

{
  imports = [
    ./profiles
    ./modules/emacs
    ./modules/git.nix
    ./modules/gnupg
    ./modules/firefox.nix
    ./modules/hammerspoon
    ./modules/linkapps.nix
    ./modules/ssh.nix
    ./modules/userinfo.nix
    ./modules/zsh
  ];

  config = {
    # NB: This passes this flake as input to these modules via the
    # "dotfiles" attribute.
    _module.args.dotfiles = inputs.self;

    # TODO(seh): Define "nix.registry"?
    # TODO(seh): Define "dotfiles.nix.channels"?
  };
}

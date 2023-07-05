{ inputs }: { lib, ... }:

{
  imports = [
    # TODO(seh): Include more modules here.
    ./profiles
    ./modules/emacs
    ./modules/git.nix
    ./modules/zsh
    ./modules/userinfo.nix
  ];

  config = {
    # NB: This passes this flake as input to these modules via the
    # "dotfiles" attribute.
    _module.args.dotfiles = inputs.self;

    # TODO(seh): Define "nix.registry"?
    # TODO(seh): Define "dotfiles.nix.channels"?
  };
}

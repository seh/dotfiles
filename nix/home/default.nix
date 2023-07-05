{ inputs }: { lib, ... }:

{
  imports = [
    ./profiles
    ./modules/emacs
    ./modules/zsh
    # TODO(seh): Include more modules here.
  ];

  config = {
    # NB: This passes this flake as input to these modules via the
    # "dotfiles" attribute.
    _module.args.dotfiles = inputs.self;

    # TODO(seh): Define "nix.registry"?
    # TODO(seh): Define "dotfiles.nix.channels"?
  };
}

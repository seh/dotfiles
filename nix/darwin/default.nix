{ inputs }:

{
  imports = [
    ./modules/config.nix
    ./profiles/apps.nix    
  ];

  # NB: This passes this flake as input to these modules via the
  # "dotfiles" attribute.
  config._module.args.dotfiles = inputs.self;
}
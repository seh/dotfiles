# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/home/modules/git.nix
{
  lib,
  pkgs,
  config,
  ...
}:

let
  cfg = config.dotfiles.git;
in
{
  options.dotfiles.git = {
    config = lib.mkOption {
      type = with lib.types; attrsOf (attrsOf anything);
      default = { };
      example = {
        rebase.autosquash = true;
      };
      description = ''
        Configuration for git. This option is meant to be used in
        conjunction with existing non-Home Manager configuration, so
        it's not written out directly to the global
        gitconfig. Instead, it's written to
        <filename>~/.config/git/home-manager-config</filename> meant
        to be included by existing configurations.

        Sample Git configuration section including this generated file:
        <programlisting>[include]
                path = ~/.config/git/home-manager-config</programlisting>

        For specific options, see the CONFIGURATION FILE section of
        <citerefentry>
          <refentrytitle>git-config</refentrytitle>
          <manvolnum>1</manvolnum>
        </citerefentry>.
      '';
    };
  };

  config = lib.mkMerge [
    { xdg.configFile."git/home-manager-config".text = lib.generators.toGitINI cfg.config; }
    # TODO(seh): Consider including additional tools here.
  ];
}

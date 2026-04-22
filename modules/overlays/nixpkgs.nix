# Overlay for the nixpkgs input to this flake
final:
# final
prev: {
  # NB: The channel name "stable" here ideally matches the default
  # value of the "dotfiles.flakeOptions.lix.channel" option defined
  # in "modules/config.nix", but this overlay runs at nixpkgs
  # instantiation time and cannot access flake-level options. A
  # caller that changes the channel option cannot easily rectify the
  # resulting divergence here short of supplying a replacement overlay.
  #
  # NB: We cannot use "final.lixPackageSets.stable.nix-direnv" or
  # "prev.lixPackageSets.stable.nix-direnv" directly here, as both
  # cause infinite recursion: the "nix-direnv" entry within
  # "lixPackageSets" is defined as "nix-direnv.override { ... }",
  # where the bare "nix-direnv" reference resolves back into the
  # fixed point of the package set being extended. Instead we
  # override "prev.nix-direnv" directly, supplying Lix ourselves.
  nix-direnv = prev.nix-direnv.override {
    nix = final.lixPackageSets.stable.lix;
  };

  # NB: Upstream NixOS/nixpkgs#510585 switched "pkgs.formats.toml"'s
  # "generate" from "remarshal" ("json2toml") to "yj -jt". The "yj"
  # tool has a bug (sclevine/yj#52) that silently truncates TOML
  # keys at the first comma, which corrupts generated files such as
  # "~/.config/jj/config.toml" whose "template-aliases" include
  # keys like "boxquoted(body, title)". NixOS/nixpkgs#512319 fixes
  # this upstream by switching to "go-toml", but has not yet
  # reached the "nixos-unstable" channel. Restore the
  # "remarshal"-based generator until the channel catches up.
  formats =
    prev.formats
    // {
      toml = {} @ args:
        (prev.formats.toml args)
        // {
          generate = name: value:
            final.runCommand name {
              nativeBuildInputs = [final.remarshal];
              value = builtins.toJSON value;
              passAsFile = ["value"];
              preferLocalBuild = true;
            } ''json2toml "$valuePath" "$out"'';
        };
    };
}

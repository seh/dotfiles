{
  flake.templates = rec {
    single = {
      path = ./_single;
      description = "Configure one computer with Home Manager or nix-darwin.";
    };

    default = single;
  };
}

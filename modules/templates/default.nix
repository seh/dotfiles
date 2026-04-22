{
  flake.templates = rec {
    single = {
      path = ./single;
      description = "Configure one computer with Home Manager or nix-darwin.";
    };

    default = single;
  };
}

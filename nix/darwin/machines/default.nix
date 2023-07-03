{ inputs }:

let
  inherit (inputs.self.lib) importDarwin;
in
{
  # By default nix-darwin will look for a configuration whose name
  # matches its hostname.
  Spinner = importDarwin ./basic.nix { };
}

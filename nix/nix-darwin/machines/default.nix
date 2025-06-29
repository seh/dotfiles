{ inputs, ... }:

let
  inherit (inputs.self.lib) importDarwin;
in
{
  flake.darwinConfigurations = {
    # By default nix-darwin will look for a configuration whose name
    # matches its hostname, per the value reported by invoking the
    # "scutil --get LocalHostName" command.
    #
    # We can use a general name here to establish the common case.
    local = importDarwin ./basic.nix { };
  };
}

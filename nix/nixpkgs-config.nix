{ inputs }:
{ lib, ... }:

{
  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = lib.mkForce (
        import inputs.nixpkgs {
          inherit system;
          config = {
            allowUnfreePredicate =
              pkg:
              builtins.elem (lib.getName pkg) [
                "1password"
                "1password-cli"
                "claude-code"
                "coder"
                #"discord"
                "dropbox"
                "ngrok"
                "slack"
                "terraform"
                "zoom"
              ];
          };
        }
      );
    };
}

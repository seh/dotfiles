{ lib, pkgs, config, ... }:

let
  cfg = config.dotfiles.ssh;
  panixKnownHostsFile = pkgs.fetchurl {
    url = "https://config.panix.com/vault/sshdata/ssh.ed25519";
    sha256 = "a8a0f8f28ea66d0a3eb73c926b51ede407297fb03143e4ea2ff529b9fe542424";
  };
  sshControlDir = "~/.ssh/sockets";
in
{
  options.dotfiles.ssh = {
    enable = lib.mkEnableOption "SSH";
    enableMultiplexing = lib.mkEnableOption "SSH multiplexing";
  };

  config = lib.mkIf cfg.enable {
    programs.ssh = (lib.mkMerge [
      {
        enable = true;
        forwardAgent = true;
        extraConfig = ''
          CanonicalizeHostname yes
          CanonicalizeFallbackLocal yes
        '';
        matchBlocks = {
          "Panix" = {
            hostname = "shell.panix.com";
            user = "seh";
            extraOptions = {
              UserKnownHostsFile = "${panixKnownHostsFile}";
            };
          };
          "github.com gitlab.com bitbucket.com" = {
            user = "git";
            extraOptions = {
              ControlMaster = "no";
            };
          };
        };
      }
      (lib.mkIf cfg.enableMultiplexing {
        controlMaster = "auto";
        controlPath = "${sshControlDir}/%C.sock";
        controlPersist = "20s"; # Default in Home Manager is ten minutes.
      })
    ]);

    home.activation = lib.mkIf cfg.enableMultiplexing {
      # Alternately, we could use a ".keep" file in this directory and
      # create it via the "homo.file" attribute.
      prepareSSHDirectory = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        mkdir -p ${sshControlDir}
      '';
    };
  };
}

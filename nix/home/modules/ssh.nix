{
  lib,
  pkgs,
  config,
  ...
}:

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
    programs.ssh = lib.mkMerge [
      {
        enable = true;
        enableDefaultConfig = false;
        extraConfig = ''
          CanonicalizeHostname yes
          CanonicalizeFallbackLocal yes
        '';
        matchBlocks = {
          "*" = {
            forwardAgent = true;
          };
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
              AddKeysToAgent = "yes";
              ControlMaster = "no";
              UseKeychain = "yes";
            };
          };
        };
      }
      (lib.mkIf cfg.enableMultiplexing {
        matchBlocks = {
          "*" = {
            controlMaster = "auto";
            controlPath = "${sshControlDir}/%C.sock";
            controlPersist = "30s";
          };
        };
      })
    ];

    home.activation = lib.mkIf cfg.enableMultiplexing {
      # Alternately, we could use a ".keep" file in this directory and
      # create it via the "homo.file" attribute.
      prepareSSHDirectory = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        mkdir -p ${sshControlDir}
      '';
    };
  };
}

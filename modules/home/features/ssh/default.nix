{flakeLib, ...}:
flakeLib.mkFeature "ssh" {
  homeManager = {
    options = {lib, ...}: {
      options.dotfiles.ssh = {
        enableMultiplexing = lib.mkEnableOption "SSH multiplexing";
      };
    };

    config = {
      lib,
      pkgs,
      config,
      ...
    }: let
      cfg = config.dotfiles.ssh;
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
      githubKnownHostsFile = ./ssh-known-hosts-github;
      panixKnownHostsFile = pkgs.fetchurl {
        url = "https://config.panix.com/vault/sshdata/ssh.ed25519";
        sha256 = "a8a0f8f28ea66d0a3eb73c926b51ede407297fb03143e4ea2ff529b9fe542424";
      };
      sshControlDir = "~/.ssh/sockets";
    in {
      programs.ssh = {
        enable = true;
        enableDefaultConfig = false;
        extraConfig = ''
          CanonicalizeFallbackLocal yes
          CanonicalizeHostname yes
        '';
        settings = {
          "*" = lib.mkMerge [
            {
              ForwardAgent = true;
            }
            (lib.mkIf cfg.enableMultiplexing {
              ControlMaster = "auto";
              ControlPath = "${sshControlDir}/%C.sock";
              ControlPersist = "30s";
            })
          ];
          "Panix" = {
            Hostname = "shell.panix.com";
            User = "seh";
            UserKnownHostsFile = "${panixKnownHostsFile}";
          };
          "github.com" =
            {
              AddKeysToAgent = "yes";
              User = "git";
              UserKnownHostsFile = "${githubKnownHostsFile}";
            } # NB: UseKeychain is a macOS-specific option.
            // lib.optionalAttrs isDarwin {
              UseKeychain = "yes";
            };
          "github.com gitlab.com bitbucket.com" = {
            ControlMaster = "no";
          };
        };
      };

      home.activation = lib.mkIf cfg.enableMultiplexing {
        # Alternately, we could use a ".keep" file in this directory and
        # create it via the "homo.file" attribute.
        prepareSSHDirectory = lib.hm.dag.entryAfter ["writeBoundary"] ''
          mkdir -p ${sshControlDir}
        '';
      };
    };
  };
}

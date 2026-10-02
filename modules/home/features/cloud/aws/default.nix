# Tools for working with Amazon Web Services.
{flakeLib, ...}:
flakeLib.mkFeature "cloud/aws" {
  homeManager = {
    options = {lib, ...}: {
      options.dotfiles.cloud.aws.granted = {
        periodicLockSeconds = lib.mkOption {
          type = lib.types.ints.unsigned;
          default = 0;
          example = 60 * 60;
          description = ''
            A launch agent locks the "granted" tool's keychain at this
            interval, in seconds, however recently anything read that
            keychain. Home Manager installs that agent only when the
            interval exceeds zero.
          '';
        };
      };
    };

    config = {
      config,
      lib,
      options,
      pkgs,
      ...
    }: let
      cfg = config.dotfiles.cloud.aws.granted;
      lockOption = options.dotfiles.cloud.aws.granted.periodicLockSeconds;
      inherit (pkgs.stdenv.hostPlatform) isDarwin;

      # The "granted" tool's secrets sit in a keychain of their own
      # rather than in the login keychain, which macOS unlocks at
      # login and then keeps unlocked. macOS discards a keychain's
      # decryption key when it locks that keychain, so the first read
      # afterward asks for that keychain's password, and macOS keeps
      # the key until the next lock.
      keychainName = "granted";
      keychainPath = "${config.home.homeDirectory}/Library/Keychains/${keychainName}.keychain-db";
      idleTimeoutSeconds = 60 * 60;
      intervalStateFile = "${config.xdg.stateHome}/granted-keychain-interval";

      prepareKeychainName = "prepare-granted-keychain";
      prepareKeychain = pkgs.writeShellApplication {
        name = prepareKeychainName;
        runtimeInputs = [pkgs.coreutils];
        text = builtins.readFile ./prepare-granted-keychain.sh;
      };
      prepareKeychainExe = "${prepareKeychain}/bin/${prepareKeychainName}";

      lockKeychain = pkgs.writeShellScript "lock-granted-keychain" ''
        if [ -f '${keychainPath}' ]; then
          /usr/bin/security lock-keychain '${keychainPath}'
        fi
      '';
    in {
      assertions = [
        {
          assertion = (cfg.periodicLockSeconds > 0) -> isDarwin;
          message = ''
            ${lib.options.showOption lockOption.loc} locks a macOS
            keychain, and this host runs
            ${pkgs.stdenv.hostPlatform.system}
          '';
        }
      ];

      home.packages = with pkgs; [
        aws-vault
      ];

      programs.awscli = {
        enable = true;
      };

      programs.granted = {
        enable = true;
        enableFishIntegration = true;
        enableZshIntegration = true;
      };

      home.activation = lib.mkIf isDarwin {
        # The "granted" tool rewrites its whole configuration file
        # whenever one setting changes, so set the keychain's name
        # through the "granted settings set" command rather than
        # rendering the file here.
        nameGrantedKeychain = lib.hm.dag.entryAfter ["writeBoundary"] ''
          ${config.programs.granted.package}/bin/granted settings set \
            --setting Keyring.KeychainName --value '${keychainName}'
        '';

        prepareGrantedKeychain = lib.hm.dag.entryAfter ["writeBoundary"] ''
          ${prepareKeychainExe} \
            -i ${toString idleTimeoutSeconds} \
            -k ${lib.escapeShellArg keychainPath} \
            -s ${lib.escapeShellArg intervalStateFile}
        '';
      };

      # A keychain's idle timeout restarts on every decryption, so
      # this keychain locks only after a gap between reads exceeds
      # that interval.
      launchd.agents = lib.mkIf (isDarwin && cfg.periodicLockSeconds > 0) {
        lock-granted-keychain = {
          enable = true;
          config = {
            ProgramArguments = ["${lockKeychain}"];
            StartInterval = cfg.periodicLockSeconds;
            RunAtLoad = true;
          };
        };
      };
    };
  };
}

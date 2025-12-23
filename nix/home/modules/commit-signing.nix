{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.dotfiles) flakeOptions;
  userConfig = flakeOptions.user;

  hasGPGSigningKey = userConfig.gpgKey != "";
  hasSSHSigningKey = userConfig.sshSigning.key != "";
  hasSigningKey = hasGPGSigningKey || hasSSHSigningKey;

  # Email addresses to include for the user's own key.
  # Falls back to [ userConfig.email ] if emailAddresses is empty.
  ownEmailAddresses =
    if userConfig.sshSigning.emailAddresses != [ ] then
      userConfig.sshSigning.emailAddresses
    else
      [ userConfig.email ];

  # Build the allowed_signers file content.
  ownSignerEntries = lib.optionals hasSSHSigningKey (
    map (email: "${email} ${userConfig.sshSigning.key}") ownEmailAddresses
  );
  additionalSignerEntries = map (s: "${s.email} ${s.key}") (userConfig.sshAllowedSigners or [ ]);
  allowedSignersContent = lib.concatStringsSep "\n" (ownSignerEntries ++ additionalSignerEntries);
  allowedSignersFile = pkgs.writeText "allowed_signers" allowedSignersContent;
in
{
  options.dotfiles.commitSigning = {
    # Read-only computed values for other modules to consume.
    hasGPGKey = lib.mkOption {
      type = lib.types.bool;
      default = hasGPGSigningKey;
      readOnly = true;
      internal = true;
      description = "Whether a GPG signing key is configured.";
    };

    hasSSHKey = lib.mkOption {
      type = lib.types.bool;
      default = hasSSHSigningKey;
      readOnly = true;
      internal = true;
      description = "Whether an SSH signing key is configured.";
    };

    hasKey = lib.mkOption {
      type = lib.types.bool;
      default = hasSigningKey;
      readOnly = true;
      internal = true;
      description = "Whether any signing key is configured.";
    };

    hasAllowedSigners = lib.mkOption {
      type = lib.types.bool;
      default = allowedSignersContent != "";
      readOnly = true;
      internal = true;
      description = "Whether there are any SSH allowed signers configured.";
    };

    backend = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.enum [
          "gpg"
          "ssh"
        ]
      );
      default =
        if userConfig.commitSigningBackend != null then
          userConfig.commitSigningBackend
        else if hasGPGSigningKey then
          if hasSSHSigningKey then null else "gpg"
        else if hasSSHSigningKey then
          "ssh"
        else
          null;
      description = ''
        The signing backend to use for commits: "gpg" or "ssh".

        Defaults to the value of dotfiles.user.commitSigningBackend if set.
        Otherwise, when only one of gpgKey or sshSigning.key is configured,
        this defaults to the corresponding backend. When both are configured
        and dotfiles.user.commitSigningBackend is not set, this option must
        be set explicitly.
      '';
    };

    key = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default =
        let
          selectedBackend = config.dotfiles.commitSigning.backend;
        in
        if selectedBackend == "gpg" then
          userConfig.gpgKey
        else if selectedBackend == "ssh" then
          userConfig.sshSigning.key
        else
          null;
      readOnly = true;
      internal = true;
      description = "The signing key corresponding to the configured backend.";
    };

    sshAllowedSignersFile = lib.mkOption {
      type = lib.types.path;
      default = allowedSignersFile;
      readOnly = true;
      internal = true;
      description = "Path to the allowed_signers file in the Nix store.";
    };
  };

  config = {
    assertions = [
      {
        assertion =
          !(hasGPGSigningKey && hasSSHSigningKey) || config.dotfiles.commitSigning.backend != null;
        message = "dotfiles.commitSigning.backend must be set to \"gpg\" or \"ssh\" when both gpgKey and sshSigning.key are configured";
      }
      {
        assertion = userConfig.commitSigningBackend == "gpg" -> hasGPGSigningKey;
        message = ''dotfiles.user.commitSigningBackend is "gpg" but no gpgKey is configured'';
      }
      {
        assertion = userConfig.commitSigningBackend == "ssh" -> hasSSHSigningKey;
        message = ''dotfiles.user.commitSigningBackend is "ssh" but no sshSigning.key is configured'';
      }
    ];
  };
}

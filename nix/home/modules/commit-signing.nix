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

    backend = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.enum [
          "gpg"
          "ssh"
        ]
      );
      default =
        if hasSSHSigningKey then
          "ssh"
        else if hasGPGSigningKey then
          "gpg"
        else
          null;
      readOnly = true;
      internal = true;
      description = "The signing backend to use, or null if none configured.";
    };

    key = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default =
        if hasSSHSigningKey then
          userConfig.sshSigning.key
        else if hasGPGSigningKey then
          userConfig.gpgKey
        else
          null;
      readOnly = true;
      internal = true;
      description = "The signing key to use, or null if none configured.";
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
        assertion = !(hasGPGSigningKey && hasSSHSigningKey);
        message = "dotfiles.user: gpgKey and sshSigning.key are mutually exclusive; specify only one";
      }
    ];
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.dotfiles) flakeOptions commitSigning;
  userConfig = flakeOptions.user;
  cfg = config.dotfiles.jujutsu;
  tomlFormat = pkgs.formats.toml { };
in
{
  options.dotfiles.jujutsu = {
    enable = lib.mkEnableOption "jujutsu";
    extraSettings = lib.mkOption {
      inherit (tomlFormat) type;
      default = { };
      description = "Additional settings to add to jujutsu's configuration file";
    };
  };

  config = lib.mkIf cfg.enable (
    let
      difftasticMergeToolName = "difftastic-split-view";
    in
    {
      programs = {
        jujutsu = {
          enable = lib.mkDefault true;
          package = lib.mkDefault pkgs.jujutsu;
          ediff = false; # We'll configure our own variant later.
          # See https://github.com/martinvonz/jj/blob/main/docs/config.md#configuration.
          settings = lib.mkMerge [
            {
              # See https://jj-vcs.github.io/jj/latest/config/#json-schema-support.
              "$schema" = "https://jj-vcs.github.io/jj/latest/config-schema.json";
            }
            cfg.extraSettings
            {
              fsmonitor = {
                backend = "watchman";
                watchman = {
                  register-snapshot-trigger = true;
                };
              };
              revset-aliases = {
                "lagging_bookmarks" = ''
                  ::bookmarks()
                  & mutable()
                  & mine()
                  ~ trunk()::
                '';
              };
              template-aliases = {
                # Basis of inspiration:
                #   https://github.com/martinvonz/jj/blob/main/docs/config.md#display-of-commit-and-change-ids
                #   https://v5.chriskrycho.com/essays/jj-init/#revisions-and-revsets.
                "format_short_id(id)" = "id.shortest(12)";
                "format_short_signature(signature)" = "signature";
                "format_timestamp(timestamp)" = "timestamp.ago()";

                "boxquoted(body, title)" = ''
                  surround(
                    "\n,----" ++ if(title, "[ " ++ title ++ " ]\n", "\n"),
                    "`----\n",
                    indent(
                      "| ",
                      body))
                '';
              };
              templates = {
                log = ''
                  builtin_log_compact ++
                  if(!empty &&
                     (!description ||
                      description.starts_with("WIP:") ||
                      current_working_copy),
                    concat(
                      boxquoted(diff.summary(), "summary"),
                      boxquoted(diff.stat(80), "stat")))
                '';
              };
              user = {
                name = userConfig.fullName;
                inherit (userConfig) email;
              };
            }
            (lib.optionalAttrs commitSigning.hasKey {
              git = {
                # NB: Opt for this instead of enabling "signing.sign-all".
                sign-on-push = true;
              };
              signing = {
                inherit (commitSigning) backend key;
                backends = {
                  gpg = {
                    allow-expired-keys = false;
                  };
                }
                // lib.optionalAttrs commitSigning.hasAllowedSigners {
                  ssh = {
                    allowed-signers = "${commitSigning.sshAllowedSignersFile}";
                  };
                };
              };
            })
            (
              let
                emacsMergeToolName = "ediff-alt";
              in
              {
                merge-tools = {
                  ${difftasticMergeToolName} = lib.mkIf config.dotfiles.difftastic.enable {
                    program = lib.getExe config.programs.difftastic.package;
                    diff-args = [
                      "--color=always"
                      "$left"
                      "$right"
                    ];
                  };
                  ${emacsMergeToolName} =
                    let
                      programName = "emacs-ediff-alt";
                      emacsDiffProgram = pkgs.writeShellScriptBin programName (
                        builtins.readFile (./. + "/${programName}")
                      );
                    in
                    {
                      program = lib.getExe emacsDiffProgram;
                      merge-args = [
                        "$left"
                        "$right"
                        "$base"
                        "$output"
                      ];
                      # Attempt to detect when we exit ediff-merge without
                      # resolving all the conflicts, leaving some still
                      # present in the output file.
                      merge-tool-edits-conflict-markers = true;
                    };
                };
                ui = {
                  diff-formatter = difftasticMergeToolName;
                  editor =
                    let
                      programName = "emacsclient-for-jj-describe";
                      emacsclientProgram = pkgs.writeShellScriptBin programName (
                        builtins.readFile (./. + "/${programName}")
                      );
                    in
                    lib.getExe emacsclientProgram;
                  log-word-wrap = true;
                  merge-editor = emacsMergeToolName;
                  show-cryptographic-signatures = true;
                };
              }
            )
          ];
        };
        jjui = {
          enable = lib.mkDefault true;
          package = lib.mkDefault pkgs.jjui;
          settings = {
            preview = {
              file_command = [
                "diff"
                "--color"
                "always"
                "-r"
                "$change_id"
                "--config"
                "ui.diff-formatter=${difftasticMergeToolName}"
                "$file"
              ];
              revision_command = [
                "show"
                "--color"
                "always"
                "-r"
                "$change_id"
                "--config"
                "ui.diff-formatter=${difftasticMergeToolName}"
              ];
            };
            ui = {
              tracer = {
                enabled = true;
              };
            };
          };
        };
      };
      home.packages = with pkgs; [
        jj-fzf
      ];
    }
  );
}

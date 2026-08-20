{flakeLib, ...}:
flakeLib.mkFeature "vcs/jujutsu" {
  homeManager = {
    options = {
      lib,
      pkgs,
      ...
    }: let
      tomlFormat = pkgs.formats.toml {};
    in {
      options.dotfiles.vcs.jujutsu = {
        extraSettings = lib.mkOption {
          inherit (tomlFormat) type;
          default = {};
          description = ''
            Additional settings to add to jujutsu's configuration file
          '';
        };
      };
    };

    config = {
      config,
      lib,
      pkgs,
      ...
    }: let
      commitSigning = config.dotfiles.vcs.commit-signing;
      userConfig = config.dotfiles.identity;
      cfg = config.dotfiles.vcs.jujutsu;
      difftasticMergeToolName = "difftastic-split-view";
    in {
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
              git = {
                # See https://jj-vcs.github.io/jj/latest/FAQ/#how-can-i-avoid-committing-my-local-only-changes-to-tracked-files.
                # NB: Exclude "WIP:"-prefixed commits here via
                # "wip()", as it's often useful to push such changes
                # to topic branches for testing.
                private-commits = "private()";
              };
              revsets = {
                # Basis of inspiration: https://offby1.website/posts/how-i-jujutsu-bookmark-advance.html
                bookmark-advance-to = "closest_pushable(@)";
              };
              revset-aliases = {
                # Basis of inspiration: https://offby1.website/posts/how-i-jujutsu-bookmark-advance.html
                "closest_pushable(to)" = "heads(::to & mutable() & ~denied_for_publishing() & ~description(exact:'') & (~empty() | merges()))";
                "denied_for_publishing()" = "wip() | private()";
                "lagging_bookmarks" = ''
                  ::bookmarks()
                  & mutable()
                  & mine()
                  ~ trunk()::
                '';
                "private()" = "description(glob:'private:*')";
                # stack(x, n) is the set of mutable commits reachable
                # from 'x', with 'n' parents. 'n' is often useful to
                # customize the display and return set for certain
                # operations. 'x' can be used to target the set of
                # 'roots' to traverse, e.g. @ is the current stack.
                #
                # Basis of inspiration: https://github.com/thoughtpolice/a/blob/c88edea13dc86adaa46e33f7fc6c7dd16552755f/tilde/aseipp/dotfiles/jj/config.toml#L109-L115
                "stack(x, n)" = "ancestors(reachable(x, mutable()), n)";
                "stack(x)" = "stack(x, 2)";
                "stack()" = "stack(@)";
                "wip()" = "description(glob:'WIP:*') | description(glob:'wip:*')";
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
              ui = {
                bookmark-list-sort-keys = [
                  "committer-date"
                  "name"
                ];
                diff-formatter = flakeLib.onlyWhen config ["dev/difftastic"] difftasticMergeToolName;
                log-word-wrap = true;
                show-cryptographic-signatures = true;
              };
              user = {
                name = userConfig.fullName;
                inherit (userConfig) email;
              };
            }
            (lib.optionalAttrs commitSigning.hasKey {
              git = {
                # NB: Opt for this instead of enabling
                # "signing.sign-all".
                sign-on-push = true;
              };
              signing = {
                inherit (commitSigning) backend key;
                backends =
                  {
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
              in {
                merge-tools = {
                  ${difftasticMergeToolName} = flakeLib.onlyWhen config ["dev/difftastic"] {
                    program = lib.getExe config.programs.difftastic.package;
                    diff-args = [
                      "--color=always"
                      "$left"
                      "$right"
                    ];
                  };
                  ${emacsMergeToolName} = let
                    programName = "emacs-ediff-alt";
                    emacsDiffProgram = pkgs.writeShellApplication {
                      name = programName;
                      runtimeInputs = with pkgs; [
                        coreutils
                        fswatch
                      ];
                      text = builtins.readFile (./. + "/${programName}");
                    };
                  in {
                    program = lib.getExe emacsDiffProgram;
                    merge-args = [
                      "$left"
                      "$right"
                      "$base"
                      "$output"
                    ];
                    # Attempt to detect when we exit ediff-merge
                    # without resolving all the conflicts, leaving
                    # some still present in the output file.
                    merge-tool-edits-conflict-markers = true;
                  };
                };
                ui = {
                  editor = let
                    programName = "emacsclient-for-jj-describe";
                    emacsclientProgram = pkgs.writeShellApplication {
                      name = programName;
                      runtimeInputs = with pkgs; [
                        jujutsu
                      ];
                      text = builtins.readFile (./. + "/${programName}");
                    };
                  in
                    lib.getExe emacsclientProgram;
                  merge-editor = emacsMergeToolName;
                };
              }
            )
          ];
        };
      };
      home.packages = with pkgs; [
        jj-fzf
      ];
    };
  };
}

{flakeLib, ...}:
flakeLib.mkFeature "model-agent/claude" {
  homeManager = {
    config,
    lib,
    pkgs,
    ...
  }: let
    format-shell-program = pkgs.writeShellApplication {
      name = "format-shell-program";
      runtimeInputs = with pkgs; [
        coreutils
        file
        jq
        shfmt
      ];
      text = builtins.readFile ./format-shell-program;
    };
    mkPerEventHooks = {
      events,
      patterns,
      command,
    }:
      lib.concatMap (
        pattern:
          map (event: {
            type = "command";
            "if" = "${event}(${pattern})";
            inherit command;
          })
          events
      )
      patterns;
  in {
    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.claude-code;
      settings = {
        includeCoAuthoredBy = false;
        model = "claude-opus-4-7";
        hooks = {
          PostToolUse = let
            jq = lib.getExe pkgs.jq;
            editingEvents = [
              "Edit"
              "Write"
            ];
            matcher = lib.concatStringsSep "|" editingEvents;
            receiveInputFilePath = ''"$(${jq} --raw-output '.tool_input.file_path')"'';
          in
            map (entry: entry // {inherit matcher;}) (
              [
                {
                  # Bazel files
                  hooks = mkPerEventHooks {
                    events = editingEvents;
                    patterns = [
                      "*.bazel"
                      "*.bzl"
                    ];
                    command = ''${lib.getExe' pkgs.buildifier "buildifier"} ${receiveInputFilePath}'';
                  };
                }
                {
                  # CUE files
                  hooks = mkPerEventHooks {
                    events = editingEvents;
                    patterns = ["*.cue"];
                    command = ''${lib.getExe pkgs.cue} fmt --files -- ${receiveInputFilePath}'';
                  };
                }
                {
                  # Go files
                  hooks = mkPerEventHooks {
                    events = editingEvents;
                    patterns = ["*.go"];
                    command = ''${lib.getExe pkgs.gofumpt} -w ${receiveInputFilePath}'';
                  };
                }
              ]
              ++ lib.optional (config.dotfiles._host.activatesFeature "lang/lua") {
                # Lua files
                hooks = mkPerEventHooks {
                  events = editingEvents;
                  patterns = ["*.lua"];
                  command = ''${lib.getExe pkgs.stylua} --config-path ${config.dotfiles.lua.styluaConfigFile} -- ${receiveInputFilePath}'';
                };
              }
              ++ [
                {
                  # Markdown files
                  hooks = mkPerEventHooks {
                    events = editingEvents;
                    patterns = ["*.md"];
                    command = ''${lib.getExe pkgs.rumdl} fmt -- ${receiveInputFilePath}'';
                  };
                }
                {
                  # Nix files
                  hooks = mkPerEventHooks {
                    events = editingEvents;
                    patterns = ["*.nix"];
                    command = ''${lib.getExe pkgs.alejandra} --quiet ${receiveInputFilePath}'';
                  };
                }
                {
                  # Shell programs
                  hooks = [
                    {
                      type = "command";
                      command = lib.getExe format-shell-program;
                    }
                  ];
                }
                {
                  # Terraform files
                  hooks = mkPerEventHooks {
                    events = editingEvents;
                    patterns = [
                      "*.tf"
                      "*.tfvars"
                    ];
                    command = ''${lib.getExe' pkgs.tenv "terraform"} fmt ${receiveInputFilePath}'';
                  };
                }
              ]
            );
        };
        permissions = {
          defaultMode = "auto";
        };
      };
    };
  };
}

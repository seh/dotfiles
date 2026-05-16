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
            receiveInputFilePath = ''"$(${jq} --raw-output '.tool_input.file_path')"'';
            languages =
              [
                {
                  # Bazel files
                  patterns = [
                    "*.bazel"
                    "*.bzl"
                  ];
                  command = ''${lib.getExe' pkgs.buildifier "buildifier"} ${receiveInputFilePath}'';
                }
                {
                  # CUE files
                  patterns = ["*.cue"];
                  command = ''${lib.getExe pkgs.cue} fmt --files -- ${receiveInputFilePath}'';
                }
                {
                  # Go files
                  patterns = ["*.go"];
                  command = ''${lib.getExe pkgs.gofumpt} -w ${receiveInputFilePath}'';
                }
              ]
              ++ lib.optional (config.dotfiles._host.activatesFeature "lang/lua") {
                # Lua files
                patterns = ["*.lua"];
                command = ''${lib.getExe pkgs.stylua} --config-path ${config.dotfiles.lua.styluaConfigFile} -- ${receiveInputFilePath}'';
              }
              ++ lib.optional (config.dotfiles._host.activatesFeature "lang/markdown") {
                # Markdown files
                patterns = ["*.md"];
                command = ''${lib.getExe pkgs.rumdl} fmt -- ${receiveInputFilePath}'';
              }
              ++ [
                {
                  # Nix files
                  patterns = ["*.nix"];
                  command = ''${lib.getExe pkgs.alejandra} --quiet ${receiveInputFilePath}'';
                }
                {
                  # Terraform files
                  patterns = [
                    "*.tf"
                    "*.tfvars"
                  ];
                  command = ''${lib.getExe' pkgs.tenv "terraform"} fmt ${receiveInputFilePath}'';
                }
              ];
            mkEventHandlers = event:
              lib.concatMap (
                lang:
                  map (pattern: {
                    type = "command";
                    "if" = "${event}(${pattern})";
                    inherit (lang) command;
                  })
                  lang.patterns
              )
              languages;
          in
            map (event: {
              matcher = event;
              hooks = mkEventHandlers event;
            })
            editingEvents
            ++ [
              {
                # Shell programs (the "format-shell-program" decides
                # internally whether to format).
                matcher = lib.concatStringsSep "|" editingEvents;
                hooks = [
                  {
                    type = "command";
                    command = lib.getExe format-shell-program;
                  }
                ];
              }
            ];
        };
        permissions = {
          defaultMode = "auto";
        };
      };
    };
  };
}

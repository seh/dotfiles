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

    enabledPluginIDs =
      lib.optionals (config.dotfiles._host.activatesFeature "dev/language-servers") [
        "gopls-lsp@claude-plugins-official"
        "typescript-lsp@claude-plugins-official"
      ]
      ++ lib.optional (config.dotfiles._host.activatesFeature "lang/rust") "rust-analyzer-lsp@claude-plugins-official";
  in {
    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.claude-code;
      lspServers = lib.mkMerge [
        # NB: We configure Lua via "lspServers" rather than enabling
        # "lua-lsp@claude-plugins-official" because that marketplace
        # plugin invokes "lua-language-server" (the LuaLS project),
        # while our "lang/lua" feature installs "emmylua-ls" instead.
        (lib.mkIf (config.dotfiles._host.activatesFeature "lang/lua") {
          lua = {
            command = lib.getExe pkgs.emmylua-ls;
            extensionToLanguage = {
              ".lua" = "lua";
            };
          };
        })
        (lib.mkIf (config.dotfiles._host.activatesFeature "dev/language-servers") {
          bash = {
            command = lib.getExe pkgs.bash-language-server;
            args = ["start"];
            extensionToLanguage = {
              ".bash" = "shellscript";
              ".sh" = "shellscript";
            };
          };
          json = {
            command = lib.getExe pkgs.vscode-json-languageserver;
            args = ["--stdio"];
            extensionToLanguage = {
              ".json" = "json";
              ".jsonc" = "jsonc";
            };
          };
          jsonnet = {
            command = lib.getExe pkgs.jsonnet-language-server;
            extensionToLanguage = {
              ".jsonnet" = "jsonnet";
              ".libsonnet" = "jsonnet";
            };
          };
          nix = {
            command = lib.getExe pkgs.nixd;
            extensionToLanguage = {
              ".nix" = "nix";
            };
          };
          starlark = {
            command = lib.getExe pkgs.starpls;
            args = ["server"];
            extensionToLanguage = {
              ".bazel" = "starlark";
              ".bzl" = "starlark";
              ".star" = "starlark";
            };
          };
          terraform = {
            command = lib.getExe pkgs.terraform-ls;
            args = ["serve"];
            extensionToLanguage = {
              ".tf" = "terraform";
              ".tfvars" = "terraform";
            };
          };
          toml = {
            command = lib.getExe pkgs.taplo;
            args = ["lsp" "stdio"];
            extensionToLanguage = {
              ".toml" = "toml";
            };
          };
          yaml = {
            command = lib.getExe pkgs.yaml-language-server;
            args = ["--stdio"];
            extensionToLanguage = {
              ".yaml" = "yaml";
              ".yml" = "yaml";
            };
          };
        })
      ];
      settings =
        {
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
        }
        // lib.optionalAttrs (enabledPluginIDs != []) {
          enabledPlugins = lib.genAttrs enabledPluginIDs (_: true);
        };
    };
  };
}

{flakeLib, ...}:
flakeLib.mkFeature "shell/atuin" {
  homeManager = {
    config,
    lib,
    ...
  }: let
    # Whether "C-r" opens Atuin's search menu instead of fzf's history
    # widget.
    claimKeyBindingControlR = true;
    inherit (config.dotfiles._resolved) inEffect;
  in {
    programs.atuin = {
      daemon.enable = true;
      enable = lib.mkDefault true;
      enableBashIntegration = inEffect "shell/bash";
      enableNushellIntegration = inEffect "shell/nushell";
      enableZshIntegration = inEffect "shell/zsh";
      # The Up arrow keeps stepping back through prior commands one at
      # a time.
      flags =
        [
          "--disable-up-arrow"
        ]
        ++ lib.optional (!claimKeyBindingControlR) "--disable-ctrl-r";
      # When the "config.toml" file is absent, Atuin writes a default
      # one at that path, so Home Manager replaces that file instead
      # of stopping activation when it finds one there.
      forceOverwriteSettings = true;
      settings = {
        ai = {
          # Keep the language model from running commands, reading or
          # changing files, and reading the output of earlier
          # commands.
          capabilities = {
            enable_command_execution = false;
            enable_file_tools = false;
            enable_history_output = false;
          };
          # Pressing "?" at an empty prompt opens Atuin AI, which asks
          # a language model on Atuin's hosted service to write shell
          # commands.
          enabled = true;
        };
        daemon = {
          # Home Manager places the daemon's socket in a directory
          # that Atuin doesn't create automatically, so the daemon
          # can't start there. Place it in Atuin's newer data
          # directory instead, which Atuin creates whenever it loads
          # its settings.
          #
          # See https://github.com/nix-community/home-manager/issues/10165.
          socket_path = "${config.home.homeDirectory}/.atuin/data/daemon.sock";
        };
        # Pressing Enter in the search menu runs the selected command,
        # while pressing Tab places it on the prompt for editing
        # instead.
        enter_accept = true;
        # Rank search matches by frequency and recency, using the
        # daemon's in-memory index. The search menu falls back to the
        # plain "fuzzy" mode when the daemon is unreachable.
        search_mode = "daemon-fuzzy";
      };
    };
    # Keep fzf from binding "C-r" to its history widget when Atuin
    # binds that key.
    programs.fzf.historyWidget.command = lib.mkIf claimKeyBindingControlR "";
  };
}

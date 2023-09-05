{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.kitty;
in
{
  options.dotfiles.kitty.enable = lib.mkEnableOption "kitty";

  config = lib.mkIf cfg.enable {
    # NB: This configuration has to live within a dedicated file with
    # a prescribed name.
    xdg.configFile."kitty/open-actions.conf" = {
      source = ./.kitty/open-actions.conf;
    };

    programs.kitty = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.kitty;
      keybindings = {
        "f7" = "show_last_visited_command_output";
        "f8" = "launch --stdin-source=@last_visited_cmd_output --stdin-add-formatting --type=os-window less -R";
        "shift+f8" = "launch --stdin-source=@last_visited_cmd_output --type=clipboard";
      };
      settings = {
        allow_remote_control = true;
        background_blur = 20;
        background_opacity = "0.96";
        bell_on_tab = true;
        # The default value is -1, which ignores the case of sitting
        # with a shell prompt and no command running.
        #   https://sw.kovidgoyal.net/kitty/conf/#opt-kitty.confirm_os_window_close
        confirm_os_window_close = 1;
        cursor_shape = "beam";
        # The default thickness is 1.5.
        cursor_beam_thickness = "2.5";
        cursor_stop_blinking_after = "2.0";
        #dynamic_background_opacity = true;
        editor = "emacsclient";
        scrollback_lines = 15000;
        scrollback_pager_history_size = 10;
        tab_bar_edge = "top";
        # Basis of inspiration:
        #   https://github.com/kovidgoyal/kitty/discussions/4447#discussioncomment-3912065
        tab_activity_symbol = "◉ ";
        tab_bar_margin_width = 9;
        tab_bar_margin_height = "9 4";
        tab_bar_style = "separator";
        tab_separator = "\"\"";
        # Borrow some colors from Solarized: "Base03" for the
        # background and "Base1" for the foreground.
        tab_title_template = "{fmt.fg._002b36}{fmt.bg.default}{fmt.fg._93a1a1}{fmt.bg._002b36} {fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg._93a1a1}{sup.index}{title} {fmt.fg._002b36}{fmt.bg.default} ";
        active_tab_title_template = "{fmt.fg._e5c07b}{fmt.bg.default}{fmt.fg._282c34}{fmt.bg._e5c07b} {fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg._282c34}{title} {fmt.fg._e5c07b}{fmt.bg.default} ";
        # NB: The cursor color is usually set by themes, so set it
        # here after loading a theme.
        # Emulate "tomato" which is 255 R, 99 G, and 71 B.
        cursor = "#ff6347";
      };
      theme = "Belafonte Day";
    };
  };
}

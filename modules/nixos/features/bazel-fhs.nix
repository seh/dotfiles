# NixOS module providing FHS compatibility for Bazel sandbox actions.
#
# Bazel sanitizes the environment for hermetic builds, setting PATH to
# "/bin:/usr/bin:/usr/local/bin". On NixOS, these paths don't exist by
# default, so genrules and external rulesets fail to find standard
# tools. This module creates the necessary symlinks.
{
  flake.featureModules.nixOS.bazel-fhs = {
    config,
    lib,
    pkgs,
    ...
  }: let
    cfg = config.bazel.fhs;

    # Wrapper script for bash that sets a default "PATH" environment
    # variable when invoked with an empty or dummy environment (e.g.,
    # via "env -"). NixOS's bash has a compiled-in default "PATH" value
    # of "/no-such-path", so scripts that expect standard tools like
    # "mktemp" fail when Bazel runs them with a sanitized environment.
    bashWithDefaultPath = pkgs.writeShellScriptBin "bash" ''
      if [ -z "$PATH" ] || [ "$PATH" = '/no-such-path' ]; then
        export PATH='/usr/bin:/bin'
      fi
      exec ${pkgs.bash}/bin/bash "$@"
    '';

    defaultTools = {
      awk = "${pkgs.gawk}/bin/awk";
      basename = "${pkgs.coreutils}/bin/basename";
      cat = "${pkgs.coreutils}/bin/cat";
      chmod = "${pkgs.coreutils}/bin/chmod";
      cp = "${pkgs.coreutils}/bin/cp";
      cut = "${pkgs.coreutils}/bin/cut";
      date = "${pkgs.coreutils}/bin/date";
      diff = "${pkgs.diffutils}/bin/diff";
      dirname = "${pkgs.coreutils}/bin/dirname";
      expr = "${pkgs.coreutils}/bin/expr";
      find = "${pkgs.findutils}/bin/find";
      git = "${pkgs.git}/bin/git";
      grep = "${pkgs.gnugrep}/bin/grep";
      gzip = "${pkgs.gzip}/bin/gzip";
      head = "${pkgs.coreutils}/bin/head";
      install = "${pkgs.coreutils}/bin/install";
      ln = "${pkgs.coreutils}/bin/ln";
      ls = "${pkgs.coreutils}/bin/ls";
      lscpu = "${pkgs.util-linux}/bin/lscpu";
      mkdir = "${pkgs.coreutils}/bin/mkdir";
      mktemp = "${pkgs.coreutils}/bin/mktemp";
      mv = "${pkgs.coreutils}/bin/mv";
      paste = "${pkgs.coreutils}/bin/paste";
      printf = "${pkgs.coreutils}/bin/printf";
      python3 = "${pkgs.python3}/bin/python3";
      rm = "${pkgs.coreutils}/bin/rm";
      sed = "${pkgs.gnused}/bin/sed";
      sort = "${pkgs.coreutils}/bin/sort";
      tail = "${pkgs.coreutils}/bin/tail";
      tar = "${pkgs.gnutar}/bin/tar";
      touch = "${pkgs.coreutils}/bin/touch";
      tr = "${pkgs.coreutils}/bin/tr";
      uniq = "${pkgs.coreutils}/bin/uniq";
      uname = "${pkgs.coreutils}/bin/uname";
      wc = "${pkgs.coreutils}/bin/wc";
      whoami = "${pkgs.coreutils}/bin/whoami";
    };

    # Merge defaults with user-provided tools, then filter out null
    # entries (which indicate tools to exclude).
    effectiveTools = lib.filterAttrs (_: v: v != null) (defaultTools // cfg.tools);
  in {
    options.bazel.fhs = {
      enable = lib.mkEnableOption "FHS compatibility for Bazel sandbox actions";

      tools = lib.mkOption {
        type = lib.types.attrsOf (lib.types.nullOr lib.types.str);
        default = {};
        example = lib.literalExpression ''
          {
            # Add a tool not in the default set.
            jq = "''${pkgs.jq}/bin/jq";
            # Remove a tool from the default set.
            git = null;
          }
        '';
        description = ''
          Additional tools to symlink into /usr/bin, or null to exclude
          a tool from the default set. Each attribute name is the
          symlink name, and the value is the path to the target binary.
        '';
      };
    };

    config = lib.mkIf cfg.enable {
      # Use our bash wrapper in the system environment so that the
      # "/bin/bash" program has a sensible default PATH.
      environment.systemPackages = [bashWithDefaultPath];

      # Enable nix-ld to run unpatched binaries (e.g., hermetic Python
      # from Bazel's rules_python). This provides the dynamic linker at
      # /lib/ld-linux-aarch64.so.1 (or the equivalent for x86_64).
      programs.nix-ld = {
        enable = true;
        libraries = with pkgs; [
          libGL
          stdenv.cc.cc.lib
          zlib
        ];
      };

      system.activationScripts = {
        # Install the bash wrapper at /bin/bash for scripts with
        # "#!/bin/bash" shebangs.
        binbash = lib.stringAfter ["usrbinenv"] ''
          mkdir -p /bin
          ln -sfn ${bashWithDefaultPath}/bin/bash /bin/bash
        '';

        # Create /usr/bin symlinks for tools that Bazel actions expect
        # at standard FHS paths.
        usrbintools = lib.stringAfter ["usrbinenv"] ''
          mkdir -p /usr/bin
          ${lib.concatStringsSep "\n" (
            lib.mapAttrsToList (name: path: "ln -sfn '${path}' '/usr/bin/${name}'") effectiveTools
          )}
        '';

        # Create /usr/share/terminfo symlink for the ncurses Bazel
        # ruleset.
        usrshareterminfo = lib.stringAfter ["usrbinenv"] ''
          mkdir -p /usr/share
          ln -sfn ${pkgs.ncurses}/share/terminfo /usr/share/terminfo
        '';
      };
    };
  };
}

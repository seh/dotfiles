# NixOS module providing FHS compatibility for Bazel sandbox actions.
#
# Bazel sanitizes the environment for hermetic builds, setting the
# "PATH" variable to "/bin:/usr/bin:/usr/local/bin". On NixOS those
# directories contain only the "sh" and "env" programs, so genrules and
# external rulesets fail to find standard tools. This module serves
# "/bin" and "/usr/bin" through NixOS's envfs file system, which falls
# back to the tools below for a program whose own "PATH" variable lacks
# them.
{flakeLib, ...}:
flakeLib.mkFeature "compat/bazel-fhs" {
  nixOS = {
    options = {lib, ...}: {
      options.dotfiles.compat.bazel-fhs = {
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
            Additional tools for the envfs fallback directory, or null
            to exclude a tool from the default set. Each attribute name
            is the program's name under "/bin" and "/usr/bin", and the
            value is the path to the target binary.
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
      cfg = config.dotfiles.compat.bazel-fhs;

      # Wrapper script for bash that sets a default "PATH" environment
      # variable when invoked with an empty or dummy environment (e.g.,
      # via the "env -" command). NixOS's bash has a compiled-in default
      # "PATH" value of "/no-such-path", so scripts that expect standard
      # tools like the "mktemp" command fail when Bazel runs them with a
      # sanitized environment.
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
        realpath = "${pkgs.coreutils}/bin/realpath";
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

      # The fallback directory already contains these programs.
      reservedNames = [
        "bash"
        "env"
        "sh"
      ];
    in {
      assertions = [
        {
          assertion = lib.all (name: !(effectiveTools ? ${name})) reservedNames;
          message = ''The "dotfiles.compat.bazel-fhs.tools" option must leave out these names, which the envfs fallback directory already contains: ${lib.concatMapStringsSep ", " (name: "\"${name}\"") reservedNames}.'';
        }
      ];

      services.envfs = {
        enable = true;
        # The bash wrapper serves as "/bin/bash" for a program whose own
        # "PATH" variable lacks bash, such as one started with an empty
        # environment.
        extraFallbackPathCommands = ''
          ln -s ${bashWithDefaultPath}/bin/bash $out/bash
          ${lib.concatStringsSep "\n" (
            lib.mapAttrsToList (name: path: "ln -s '${path}' $out/${name}") effectiveTools
          )}
        '';
      };

      # envfs serves only "/bin" and "/usr/bin", so this link provides
      # the "/usr/share/terminfo" directory that the ncurses Bazel
      # ruleset expects.
      system.activationScripts.usrshareterminfo = ''
        mkdir -p /usr/share
        ln -sfn ${pkgs.ncurses}/share/terminfo /usr/share/terminfo
      '';
    };
  };
}

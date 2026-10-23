# NixOS module providing the commands that FHS 3.0 §3.4 requires in
# "/bin" under both "/bin" and "/usr/bin", for programs that look for
# tools at those fixed paths or that start without a "PATH" environment
# variable. On NixOS those two directories contain only the "env" and
# "sh" programs. This module fills both with links to the commands.
# Among those links, "bash", "env", and "sh" are wrapper programs that
# supply a default search path through the same two directories to a
# program started without a "PATH" variable.
{flakeLib, ...}:
flakeLib.mkFeature "compat/fhs" {
  nixOS = {
    options = {lib, ...}: {
      options.dotfiles.compat.fhs = {
        toolPackages = lib.mkOption {
          type = lib.types.attrsOf (lib.types.nullOr lib.types.package);
          default = {};
          example = lib.literalExpression ''
            {
              # Add a program.
              jq = pkgs.jq;
              # Remove a program, whatever supplied it.
              git = null;
            }
          '';
          description = ''
            Additional programs under "/bin" and "/usr/bin", or null to
            remove the program of that name, whatever supplied it. Each
            attribute name is the program's name under those two
            directories, and the value is the package whose "bin"
            directory contains a program of that name.
          '';
        };

        toolPaths = lib.mkOption {
          type = lib.types.attrsOf (lib.types.nullOr lib.types.str);
          default = {};
          example = lib.literalExpression ''
            {
              # Add a program under a name other than its file name.
              uncompress = "''${pkgs.ncompress}/bin/uncompress-ncompress";
            }
          '';
          description = ''
            Additional programs under "/bin" and "/usr/bin" whose file
            names differ from their names there, or null to remove the
            program of that name, whatever supplied it. Each attribute
            name is the program's name under those two directories, and
            the value is the path of the program's file.
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
      cfg = config.dotfiles.compat.fhs;

      # The interpreters that a program's "#!" line can start without a
      # "PATH" variable. Each entry is the nixpkgs package that supplies
      # the interpreter.
      wrapperPackages = {
        bash = pkgs.bash;
        env = pkgs.coreutils;
        # Ubuntu's "sh" is the "dash" program; the module's "sh" is
        # "bash" in POSIX mode. A program that runs under "dash" usually
        # runs under this "sh"; a program that runs under this "sh" may
        # fail under "dash".
        sh = pkgs.bash;
      };

      # The wrapper programs take these names, so the links of other
      # programs must avoid them.
      reservedNames = lib.attrNames wrapperPackages;

      # FHS 3.0 §3.4 requires these commands in "/bin" and lists the
      # archiving ones among its optional commands. Each entry binds the
      # command's name to the nixpkgs package that supplies it. Two
      # required commands are absent, "login" and "su", since a link
      # from the store lacks their privileges. The "sh" command is one
      # of the wrapped interpreters.
      fhsCommands = lib.mapAttrs (name: package: lib.getExe' package name) {
        cat = pkgs.coreutils;
        chgrp = pkgs.coreutils;
        chmod = pkgs.coreutils;
        chown = pkgs.coreutils;
        cp = pkgs.coreutils;
        cpio = pkgs.cpio;
        date = pkgs.coreutils;
        dd = pkgs.coreutils;
        df = pkgs.coreutils;
        dmesg = pkgs.util-linux;
        echo = pkgs.coreutils;
        "false" = pkgs.coreutils;
        gunzip = pkgs.gzip;
        gzip = pkgs.gzip;
        hostname = pkgs.hostname-debian;
        kill = pkgs.procps;
        ln = pkgs.coreutils;
        ls = pkgs.coreutils;
        mkdir = pkgs.coreutils;
        mknod = pkgs.coreutils;
        more = pkgs.util-linux;
        mount = pkgs.util-linux;
        mv = pkgs.coreutils;
        ps = pkgs.procps;
        pwd = pkgs.coreutils;
        rm = pkgs.coreutils;
        rmdir = pkgs.coreutils;
        sed = pkgs.gnused;
        stty = pkgs.coreutils;
        sync = pkgs.coreutils;
        tar = pkgs.gnutar;
        "true" = pkgs.coreutils;
        umount = pkgs.util-linux;
        uname = pkgs.coreutils;
        zcat = pkgs.gzip;
      };

      # A name bound in both options fails evaluation, since otherwise
      # one binding would replace the other silently.
      configuredTools =
        lib.attrsets.unionOfDisjoint
        (lib.mapAttrs (name: value: lib.mapNullable (package: lib.getExe' package name) value) cfg.toolPackages)
        cfg.toolPaths;

      # A null entry in either option removes the program of that name,
      # whatever supplied it.
      removedNames = lib.attrNames (lib.filterAttrs (_: v: v == null) configuredTools);

      effectiveTools = lib.filterAttrs (_: v: v != null) configuredTools;

      # A configured program replaces an FHS command of the same name,
      # and a null entry removes the program of that name, whatever
      # supplied it. The reserved names are absent, so that the wrapper
      # programs are the only "bash", "env", and "sh" among the links.
      links =
        removeAttrs (fhsCommands // effectiveTools) (reservedNames ++ removedNames)
        // lib.mapAttrs (name: wrapper: "${wrapper}/bin/${name}") wrappers;

      # A directory of the links, whose names and targets the activation
      # script copies into "/bin" and "/usr/bin".
      linksDirectory = pkgs.linkFarm "fhs-bin" links;

      populateFHSDirectories = pkgs.writeShellApplication {
        name = "populate-fhs-directories";
        runtimeInputs = [pkgs.coreutils];
        text = builtins.readFile ./populate-fhs-directories.sh;
      };

      # Makes a wrapper program for the program of the given name in the
      # given package. The wrapper supplies a default "PATH" environment
      # variable to a program started without that variable. Without the
      # default, a shell started with an empty environment fails at its
      # first tool. When the "PATH" variable is absent, the bash that
      # runs each wrapper assumes the compiled-in value "/no-such-path",
      # so the wrapper tests for that value. An invoker who sets the
      # variable empty means it, so the wrapper leaves an empty variable
      # alone.
      withDefaultPath = name: package:
        pkgs.writeShellScriptBin name ''
          if [ "$PATH" = '/no-such-path' ]; then
            export PATH='/usr/bin:/bin'
          fi
          # The wrapper's bash would otherwise export "PWD" to the
          # wrapped program. That value is the sandbox's path, which
          # differs per execution and could leak into an output. "SHLVL"
          # passes through, since its value is the same on every
          # execution.
          export -n PWD
          # The wrapped program takes the caller's path for it as its
          # own name, so that its messages show that path rather than
          # this store path.
          exec -a "$0" ${package}/bin/${name} "$@"
        '';

      # The interpreter lines "#!/bin/sh" and "#!/bin/bash" start "sh"
      # and "bash", and the line "#!/usr/bin/env bash" starts "env",
      # which then looks for "bash". Without a "PATH" environment
      # variable, "env" searches the C library's default path, which on
      # NixOS lists the system profile first, so the plain "bash" from
      # the system profile runs. With the default from its own wrapper,
      # "env" finds "/usr/bin/bash", which is the "bash" wrapper.
      wrappers = lib.mapAttrs withDefaultPath wrapperPackages;
    in {
      assertions = [
        {
          assertion = lib.all (name: !(effectiveTools ? ${name})) reservedNames;
          message = ''The "dotfiles.compat.fhs.toolPackages" and "dotfiles.compat.fhs.toolPaths" options must leave out these names, which the module's wrappers take: ${lib.concatMapStringsSep ", " (name: "\"${name}\"") reservedNames}.'';
        }
      ];

      # NixOS's own scripts place "/bin/sh" and "/usr/bin/env". The
      # program that populates the two directories places both, so those
      # two scripts stay blank.
      system.activationScripts.binsh = lib.mkForce "";
      system.activationScripts.usrbinenv = lib.mkForce "";

      system.activationScripts.fhsDirectories = ''
        ${populateFHSDirectories}/bin/populate-fhs-directories ${linksDirectory}
      '';

      # These links provide the "terminfo" and "zoneinfo" directories
      # that FHS 3.0 §4.11 lists under "/usr/share".
      system.activationScripts.usrshare = ''
        mkdir --parents /usr/share
        ln --symbolic --force --no-dereference ${pkgs.ncurses}/share/terminfo /usr/share/terminfo
        ln --symbolic --force --no-dereference ${pkgs.tzdata}/share/zoneinfo /usr/share/zoneinfo
      '';
    };
  };
}

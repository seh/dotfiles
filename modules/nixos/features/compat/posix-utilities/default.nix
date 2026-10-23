# NixOS module providing POSIX.1-2024's utilities (Shell and Utilities
# volume, XCU §3) under "/bin" and "/usr/bin", for programs that look
# for tools at those fixed paths.
{flakeLib, ...}:
flakeLib.mkFeature "compat/posix-utilities" {
  implies = ["compat/fhs"];

  nixOS = {
    options = {lib, ...}: {
      options.dotfiles.compat.posix-utilities = {
        developmentUtilities = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Whether to provide the utilities of POSIX.1-2024's Software
            Development Utilities and C-Language Development Utilities
            options (the SD and CD codes of XBD §1.8.1) too. Those are
            "ar", "c17", "lex", "make", "nm", "strip", "xgettext", and
            "yacc". The "c99" utility, the C compiler under its
            POSIX.1-2017 name, comes with them.
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
      cfg = config.dotfiles.compat.posix-utilities;

      # POSIX.1-2024 specifies these utilities in chapter 3 of its Shell
      # and Utilities volume (XCU §3). Each entry binds the utility's
      # name to the nixpkgs package that supplies it.
      #
      # The commands that FHS 3.0 §3.4 requires in "/bin", with "env"
      # and "zcat", are under "/bin" without this module. The shell
      # itself supplies the special built-in utilities of XCU §2.15 and
      # the intrinsic utilities of XCU §1.7 (XCU §1.6).
      #
      # The module leaves out other utilities on purpose. The utilities
      # for a person at a terminal are absent, since a person's shell
      # finds programs through its search path, which lists the "bin"
      # directories of the system and user Nix profiles rather than
      # "/bin" or "/usr/bin". Four of them—"ex", "man", "talk", and
      # "vi"—belong to the User Portability Utilities option of XBD
      # §2.1.6.2; the other two—"mesg" and "write"—are base utilities of
      # XCU §3. The utilities that hand their work to a daemon are
      # absent, since the machine lacks those daemons; those are "at",
      # "batch", and "crontab" for a scheduler, "lp" for a print
      # spooler, "mailx" for a mail system, and "uucp", "uustat", and
      # "uux" for UUCP. The "newgrp" utility needs privileges that a
      # link from the store lacks. The SCCS utilities are absent, since
      # Git handles version control on these machines.
      #
      # The "asa" and "cxref" utilities are absent because there are no
      # nixpkgs packages to supply them.
      utilityPackages = {
        awk = pkgs.gawk;
        basename = pkgs.coreutils;
        bc = pkgs.bc;
        cal = pkgs.util-linux;
        cflow = pkgs.cflow;
        cksum = pkgs.coreutils;
        cmp = pkgs.diffutils;
        comm = pkgs.coreutils;
        compress = pkgs.ncompress;
        csplit = pkgs.coreutils;
        ctags = pkgs.universal-ctags;
        cut = pkgs.coreutils;
        diff = pkgs.diffutils;
        dirname = pkgs.coreutils;
        du = pkgs.coreutils;
        ed = pkgs.ed;
        expand = pkgs.coreutils;
        expr = pkgs.coreutils;
        file = pkgs.file;
        find = pkgs.findutils;
        fold = pkgs.coreutils;
        fuser = pkgs.psmisc;
        gencat = pkgs.glibc;
        getconf = pkgs.glibc;
        gettext = pkgs.gettext;
        grep = pkgs.gnugrep;
        head = pkgs.coreutils;
        iconv = pkgs.glibc;
        id = pkgs.coreutils;
        ipcrm = pkgs.util-linux;
        ipcs = pkgs.util-linux;
        join = pkgs.coreutils;
        link = pkgs.coreutils;
        locale = pkgs.glibc;
        localedef = pkgs.glibc;
        logger = pkgs.util-linux;
        logname = pkgs.coreutils;
        m4 = pkgs.gnum4;
        mkfifo = pkgs.coreutils;
        msgfmt = pkgs.gettext;
        ngettext = pkgs.gettext;
        nice = pkgs.coreutils;
        nl = pkgs.coreutils;
        nohup = pkgs.coreutils;
        od = pkgs.coreutils;
        paste = pkgs.coreutils;
        patch = pkgs.gnupatch;
        pathchk = pkgs.coreutils;
        pax = pkgs.pax;
        pr = pkgs.coreutils;
        printf = pkgs.coreutils;
        readlink = pkgs.coreutils;
        realpath = pkgs.coreutils;
        renice = pkgs.util-linux;
        sleep = pkgs.coreutils;
        sort = pkgs.coreutils;
        split = pkgs.coreutils;
        strings = pkgs.stdenv.cc;
        tabs = pkgs.ncurses;
        tail = pkgs.coreutils;
        tee = pkgs.coreutils;
        test = pkgs.coreutils;
        time = pkgs.time;
        timeout = pkgs.coreutils;
        touch = pkgs.coreutils;
        tput = pkgs.ncurses;
        tr = pkgs.coreutils;
        tsort = pkgs.coreutils;
        tty = pkgs.coreutils;
        unexpand = pkgs.coreutils;
        uniq = pkgs.coreutils;
        unlink = pkgs.coreutils;
        uudecode = pkgs.sharutils;
        uuencode = pkgs.sharutils;
        wc = pkgs.coreutils;
        who = pkgs.coreutils;
        xargs = pkgs.findutils;
      };

      utilityPaths = {
        # nixpkgs installs this program under the name
        # "uncompress-ncompress".
        uncompress = "${pkgs.ncompress}/bin/uncompress-ncompress";
      };

      # POSIX.1-2024 puts these utilities in its Software Development
      # Utilities and C-Language Development Utilities options (the SD
      # and CD codes of XBD §1.8.1). The "c17" utility runs the C
      # compiler with the matching language standard, since the compiler
      # package in nixpkgs lacks a "c17" program, and "c99" is the name
      # that POSIX.1-2017 used for the same utility.
      developmentUtilityPackages = let
        cCompiler = standard:
          pkgs.writeShellScriptBin standard ''
            exec ${pkgs.stdenv.cc}/bin/cc -std=${standard} "$@"
          '';
      in {
        ar = pkgs.stdenv.cc;
        c17 = cCompiler "c17";
        c99 = cCompiler "c99";
        lex = pkgs.flex;
        make = pkgs.gnumake;
        nm = pkgs.stdenv.cc;
        strip = pkgs.stdenv.cc;
        xgettext = pkgs.gettext;
        yacc = pkgs.bison;
      };
    in {
      # Each entry takes the lowest priority, so that a configuration's
      # own entry of the same name wins, and a null entry there removes
      # the utility.
      dotfiles.compat.fhs = {
        toolPackages = lib.mapAttrs (_: lib.mkDefault) (
          utilityPackages // lib.optionalAttrs cfg.developmentUtilities developmentUtilityPackages
        );
        toolPaths = lib.mapAttrs (_: lib.mkDefault) utilityPaths;
      };
    };
  };
}

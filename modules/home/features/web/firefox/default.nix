# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/152b40c3a412b18ba6057c3ecfb984748962282b/nix/home/modules/firefox.nix
{flakeLib, ...}:
flakeLib.mkFeature "web/firefox" {
  homeManager = {
    options = {
      lib,
      pkgs,
      ...
    }: let
      inherit (pkgs.stdenv) isDarwin;
      defaultPackage =
        if isDarwin
        then null
        else pkgs.firefox-bin;
    in {
      options.dotfiles.web.firefox = {
        package = lib.mkOption {
          type = with lib.types; nullOr package;
          default = defaultPackage;
          description = ''
            The Firefox package to use. If <literal>null</literal>, assume
            that Firefox is installed outside of Nix.
          '';
        };
        preferences = lib.mkOption {
          type = with lib.types;
            attrsOf (oneOf [
              str
              int
              float
              bool
            ]);
          default = {};
          description = ''
            Set default preferences for Firefox.

            The list of available options can be viewed by navigating to
            <literal>about:config</literal> in Firefox.
          '';
        };
        policies = lib.mkOption {
          type = with lib.types; attrsOf anything;
          default = {};
          description = ''
            Configure Firefox enterprise policies. On platforms other than
            macOS, this option requires the <option>package</option>
            option to be a non-<literal>null</literal> value in order for
            it to work.

            See <link xlink:href="https://github.com/mozilla/policy-templates/blob/master/README.md"/>
            for a list of available options.
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
      inherit (pkgs.stdenv) isDarwin;
      cfg = config.dotfiles.web.firefox;
      finalPackage = cfg.package.override {extraPolicies = cfg.policies;};
    in {
      home.packages = lib.optional (cfg.package != null) finalPackage;

      # TODO(seh): Set preferences.

      dotfiles.web.firefox.policies.Preferences = lib.mkMerge [
        (lib.mapAttrs (_: value: {
            Value = value;
            Status = "default";
          })
          cfg.preferences)
        {
          "browser.contentblocking.category" = {
            Value = lib.mkDefault "strict";

            # Firefox forcibly sets this option to "custom" if:
            #   1. The setting doesn't appear to be set by the user
            #   2. Related settings deviate from the expected values
            # https://searchfox.org/mozilla-central/rev/201b2c1/browser/components/BrowserGlue.jsm#5059
            Status = lib.mkDefault "user";
          };
        }
      ];

      targets.darwin.defaults = lib.mkIf (isDarwin && cfg.package == null) {
        "org.mozilla.firefox" =
          cfg.policies
          // {
            EnterprisePoliciesEnabled = true;
          };
      };
    };
  };
}

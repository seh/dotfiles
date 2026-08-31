# The type of the "dotfiles.host" option: the host's name, the
# features and interests selected here, the features and interests its
# author excludes, and the features and interests the machine forbids
# machine-wide. Both declarations of that option share this type—the
# writable one in the "modules/_host-option.nix" file and the
# read-only one in the "modules/_provisioned-host-option.nix" file—so
# neither declaration can differ from the other.
{lib}: let
  inherit (lib) mkOption types;
  inherit (import ./_option-types.nix {inherit lib;}) setOfNames;
in
  types.submodule {
    options = {
      name = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The host's name, or null when the configuration does not set
          one. Purely descriptive: diagnostics cite it, and nothing
          derives configuration from it.
        '';
      };
      features = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          The features this machine or user selects, from the finest
          single concern to a bundle that only implies others.
          Expansion starts from these selected names together with the
          "interests" list.
        '';
      };
      interests = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          Interest names this machine or user selects: the wants that
          guard contingent features, such as a programming language in
          use here. Expansion starts from these selected names
          together with the "features" list. An interest declares no
          configuration of its own, so selecting one activates only
          the contingent features whose preconditions it completes. A
          feature name belongs in the "features" list instead; the two
          kinds are not interchangeable, and an assertion in the
          "modules/_assertions.nix" file rejects a name written under
          the wrong one.
        '';
      };
      excludeFeatures = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          The features its author deletes from its own implication
          graph before the activation walk. The walk drops each
          excluded vertex with its out-edges (the features it implies)
          and its in-edges (the edges that target it). A feature still
          reachable through a non-excluded path stays active; one
          reachable only through excluded vertices drops out. The
          machine's own entries withhold a feature from what the
          machine provisions its users, yet yield to a user who
          selects that same name; a user's own entries apply to that
          user alone. A user's selection does not undo the
          "forbidFeatures" list. A user may exclude a contingent
          feature, and that exclusion applies like any other. A
          machine that provisions users may not: no user may select a
          contingent feature, so no user could opt back in. The
          "contingentExclusionAssertion" assertion in the
          "modules/_assertions.nix" file rejects such an entry and
          points instead to "forbidFeatures", the list that withholds
          a name from every selector outright.
        '';
      };
      excludeInterests = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          Interest names its author deletes from its own implication
          graph before the activation walk. An interest still
          reachable through a non-excluded path—another interest that
          implies it, say—remains active; one reachable only through
          excluded vertices drops out. Withholding an interest
          withholds every contingent feature that has no other way to
          satisfy its preconditions. The machine's own entries
          withhold an interest from what the machine provisions its
          users, yet yield to a user who selects that same name; a
          user's own entries apply to that user alone. A user's
          selection does not undo the "forbidInterests" list.
        '';
      };
      forbidFeatures = mkOption {
        type = setOfNames {merge = "union";};
        default = [];
        description = ''
          Machine-wide forbidding: the walk prunes each feature listed
          here from every activation, the machine's own and every
          user's, so it activates nowhere and no user receives it—not
          even a user who selects that same name. Forbidding is the
          absolute counterpart to the "excludeFeatures" list, which
          prunes only its author's own walk and, at the machine level,
          yields to a user's explicit selection.
        '';
      };
      forbidInterests = mkOption {
        type = setOfNames {merge = "union";};
        default = [];
        description = ''
          Machine-wide forbidding: the walk prunes each interest
          listed here from every activation, the machine's own and
          every user's, so it activates nowhere and no user receives
          it—not even a user who selects that same name. Forbidding is
          the absolute counterpart to the "excludeInterests" list,
          which prunes only its author's own walk and, at the machine
          level, yields to a user's explicit selection.
        '';
      };
    };
  }

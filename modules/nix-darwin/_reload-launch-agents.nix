# TODO(seh): Remove this module once nix-darwin reloads LaunchAgents
# with "launchctl bootout"/"bootstrap". Its activation still uses the
# deprecated "unload"/"load -w", which no-op on macOS 26, so a switch
# rewrites each LaunchAgent's plist but leaves the running job on its
# old definition. Tracking upstream:
#   https://github.com/nix-darwin/nix-darwin/issues/1219
#   https://github.com/nix-darwin/nix-darwin/issues/1255
#   https://github.com/nix-darwin/nix-darwin/issues/1758
{
  config,
  lib,
  pkgs,
  ...
}: let
  reloadName = "reload-launch-agents";
  reloadLaunchAgents = pkgs.writeShellApplication {
    name = reloadName;
    text = builtins.readFile ./reload-launch-agents.sh;
  };
  reloadExe = "${reloadLaunchAgents}/bin/${reloadName}";
  labels = config.dotfiles.darwin.launchAgentLabelsToReload;
in {
  options.dotfiles.darwin.launchAgentLabelsToReload = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
    example = ["org.nixos.timemachine-nas-backup"];
    description = "The launchd \"Label\"s of the per-user LaunchAgents to reload, with \"launchctl bootout\"/\"bootstrap\", after each activation. Empty (the default) runs no reload. See the \"TODO\" at the top of this module for why this workaround exists.";
  };

  config = lib.mkIf (labels != []) {
    assertions = [
      {
        assertion = config.system.primaryUser != null;
        message = "dotfiles.darwin.launchAgentLabelsToReload is set, so \"system.primaryUser\" must be set: the reload runs in that user's GUI domain.";
      }
    ];

    # Append to "postActivation" so this runs late in activation,
    # after the agents' plists are written. "mkAfter" only orders this
    # within "postActivation", not against nix-darwin's own
    # ineffective reload; that is fine, since this reload works from
    # the on-disk plist regardless.
    system.activationScripts.postActivation.text =
      lib.mkAfter "${reloadExe} --user ${lib.escapeShellArg config.system.primaryUser} ${lib.escapeShellArgs labels}";
  };
}

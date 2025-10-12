# Registry of known home-manager modules.
#
# Each string corresponds to a module that defines options under
# dotfiles.<name>, typically with at least an "enable" option.
#
# When adding a new module, add its name here to make it available
# for use in installer.modules.enable and installer.modules.disable.
[
  "bash" # bash/default.nix
  "claude" # claude.nix
  "emacs" # emacs/default.nix
  "firefox" # firefox.nix
  "gemini" # gemini.nix
  "git" # git.nix
  "gnupg" # gnupg/default.nix
  "helix" # helix.nix
  "jujutsu" # jujutsu.nix
  "kitty" # kitty/default.nix
  "nushell" # nushell.nix
  "ssh" # ssh.nix
  "zsh" # zsh/default.nix
]

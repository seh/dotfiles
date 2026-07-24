{flakeLib, ...}:
# The Rust toolchain, active when this configuration expresses the
# "lang/rust" interest. The "rustup" program installs the compiler and
# the tools the note below lists, rather than nixpkgs supplying each.
flakeLib.mkFeature "lang/rust/tools" {
  preconditions = ["lang/rust"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      # NB: rustup includes the following:
      # - cargo
      # - rust-analyzer
      # - rustfmt
      rustup
    ];
  };
}

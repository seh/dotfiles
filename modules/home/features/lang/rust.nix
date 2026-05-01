{flakeLib, ...}:
flakeLib.mkFeature "lang/rust" {
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

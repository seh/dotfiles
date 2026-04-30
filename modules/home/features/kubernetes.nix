{flakeLib, ...}:
flakeLib.mkFeature "kubernetes" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    home.packages = with pkgs;
      [
        fluxcd
        k3d
        kind
        kpt
        kubernetes-helm
        kustomize
      ]
      # NB: On Darwin, "kubectl" is provided by OrbStack.
      ++ lib.optionals (!pkgs.stdenv.hostPlatform.isDarwin) [
        kubectl
      ];

    programs.k9s = {
      enable = true;
      # TODO(seh): Configure settings.
    };
  };
}

{flakeLib, ...}:
flakeLib.mkFeature "cloud/gcp" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      (google-cloud-sdk.withExtraComponents (
        with google-cloud-sdk.components; [
          gke-gcloud-auth-plugin
        ]
      ))
    ];
  };
}

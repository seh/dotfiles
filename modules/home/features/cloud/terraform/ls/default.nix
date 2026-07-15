{flakeLib, ...}:
# The Terraform language server, active only when the
# "cloud/terraform" feature and the "dev/language-servers" interest
# are both active.
flakeLib.mkFeature "cloud/terraform/ls" {
  preconditions = ["cloud/terraform" "dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.terraform-ls];
  };
}

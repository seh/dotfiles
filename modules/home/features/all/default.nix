# Register the "all" feature by name alone. Its targets are computed
# by the "implicationsFor" function in
# "modules/lib/_implications.nix", which gives "all" every feature
# that carries no class body, is not contingent, and no other feature
# implies, so registering a new body-less bundle folds it into "all"
# without an edit here while a feature that configures something of
# its own is one a host asks for deliberately. This registration
# exists so that "all" is a name a host may select and the assertions
# in "modules/_assertions.nix" accept. Hosts decline individual
# members via "excludeFeatures".
{flakeLib, ...}:
flakeLib.mkFeature "all" {}

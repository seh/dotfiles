# Register the "all" feature by name alone. Its targets are computed
# by the "implicationsFor" function in
# "modules/lib/_implications.nix", which gives "all" every
# non-contingent feature that no other feature implies, so registering
# a new bundle folds it into "all" without an edit here. This
# registration exists so that "all" is a name a host may select and
# the assertions in "modules/_assertions.nix" accept. Hosts decline
# individual members via "excludeFeatures".
{flakeLib, ...}:
flakeLib.mkFeature "all" {}

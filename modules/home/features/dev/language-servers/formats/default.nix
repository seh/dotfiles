{flakeLib, ...}:
# The language servers with no subject feature of their own; wanted
# whenever the "dev/language-servers" interest is active. Each moves
# to a subject of its own if one ever exists.
flakeLib.mkFeature "dev/language-servers/formats" {
  preconditions = ["dev/language-servers"];
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      graphql-language-service-cli
      jq-lsp
      nixd # Compare with the "nil" package
      postgres-language-server # Compare with the "sqls" package
      taplo # For TOML files
      tinymist # For typst files
      vscode-json-languageserver
      yaml-language-server
    ];
  };
}

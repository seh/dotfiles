{flakeLib, ...}:
flakeLib.mkFeature "dev/language-servers" {
  homeManager = {pkgs, ...}: {
    home.packages = with pkgs; [
      bash-language-server
      emmylua-ls
      gopls
      graphql-language-service-cli
      jq-lsp
      jsonnet-language-server
      nixd # Compare with "nil"
      postgres-language-server # Compare with "sqls"
      starpls
      taplo # For TOML files
      terraform-ls
      tinymist # For typst files
      typescript-language-server
      vscode-json-languageserver
      yaml-language-server
    ];
  };
}

# haskell-flake setting module for sql-schema.
#
# Adds a per-package `sqlSchemaPostInstall` option whose value is a shell
# snippet appended to the package's postInstall phase. Also automatically
# puts `sql-schema-merge` on PATH during the build so the snippet can
# invoke it without a full nix-store path.
#
# Usage (in a consuming package's settings):
#
#   app-db = { self, ... }: {
#     imports = [ (inputs.sql-schema-src + "/sqlSchema/nix/postInstall.nix") ];
#     sqlSchemaPostInstall = ''
#       mkdir -p $out/share/sql-schema
#       sql-schema-merge \
#         --fragments .juspay/tmp/sql-schema \
#         --source-dirs src \
#         --out $out/share/sql-schema/sql-schema.yaml
#     '';
#   };
{ pkgs, lib, self, mkCabalSettingOptions, ... }:
{
  options = mkCabalSettingOptions {
    name = "sqlSchemaPostInstall";
    type = lib.types.lines;
    description = ''
      Shell snippet appended to the package's postInstall phase.

      The snippet runs in the standard nix-flake haskell build sandbox,
      with the package's source tree as CWD and `sql-schema-merge` on
      PATH. Typical use: invoke `sql-schema-merge` to combine
      per-module fragments (written by SqlSchema.Plugin) plus any
      `--include-merged` dep contracts into
      `$out/share/sql-schema/sql-schema.yaml`.

      Default is the empty string, in which case this setting is a
      no-op and the package's postInstall is unchanged.
    '';
    impl = snippet: drv:
      if snippet == ""
      then drv
      else
        let
          inherit (pkgs.haskell.lib.compose) addBuildTool;
        in
          lib.pipe drv [
            (addBuildTool self.sql-schema)
            (d: d.overrideAttrs (old: {
              postInstall = (old.postInstall or "") + "\n" + snippet;
            }))
          ];
  };
}

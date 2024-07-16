{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/43e3b6af08f29c4447a6073e3d5b86a4f45dd420";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    classyplate.flake = false;
    classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    references.flake = true;
    references.url = "github:eswar2001/references/35912f3cc72b67fa63a8d59d634401b79796469e";
    ghc-hasfield-plugin.flake = false;
    large-records.flake = false;
    ghc-hasfield-plugin.url = "github:juspay/ghc-hasfield-plugin/d82ac5a6c0ad643eebe2b9b32c91f6523d3f30dc";
    large-records.url = "github:eswar2001/large-records/e393f4501d76a98b4482b0a5b35d120ae70e5dd3";
    record-dot-preprocessor.url = "github:ndmitchell/record-dot-preprocessor/99452d27f35ea1ff677be9af570d834e8fab4caf";
    record-dot-preprocessor.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          projectFlakeName = "spider";
          basePackages = pkgs.haskell.packages.ghc8107;
          imports = [
            inputs.references.haskellFlakeProjectModules.output
          ];
          packages = {
            classyplate.source = inputs.classyplate;
            ghc-hasfield-plugin.source = inputs.ghc-hasfield-plugin;
            large-records.source = inputs.large-records + /large-records;
            large-generics.source = inputs.large-records + /large-generics;
            large-anon.source = inputs.large-records + /large-anon;
            ghc-tcplugin-api.source = "0.7.1.0";
            typelet.source = inputs.large-records + /typelet;
            record-dot-preprocessor.source = inputs.record-dot-preprocessor;
          };
          settings = {
            #  aeson = {
            #    check = false;
            #  };
            #  relude = {
            #    haddock = false;
            #    broken = false;
            #  };
              sheriff.check = false;
          };

          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

            hlsCheck.enable = pkgs.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.fdep;
      };
    };
}

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/75a52265bda7fd25e06e3a67dee3f0354e73243c";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    classyplate.flake = false;
    references.flake = true;
    ghc-hasfield-plugin.flake = false;
    classyplate.url = "github:iamanandsingh/classyplate/2c869097df8fdbeb1da23842bf5f52366daf3893";
    references.url = "github:iamanandsingh/references/8366257fbeb4f63e1ec0347c9fe5f203ba6fdebc";
    large-records.url = "github:well-typed/large-records";
    # references.url = "github:eswar2001/references/35912f3cc72b67fa63a8d59d634401b79796469e";
    # classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    ghc-hasfield-plugin.url = "github:eswar2001/ghc-hasfield-plugin/c932ebc0d7e824129bb70c8a078f3c68feed85c9";
    # large-records.url = "github:eswar2001/large-records/e393f4501d76a98b4482b0a5b35d120ae70e5dd3";
    # record-dot-preprocessor.url = "github:ndmitchell/record-dot-preprocessor/99452d27f35ea1ff677be9af570d834e8fab4caf";
    # record-dot-preprocessor.flake = false;
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
          # basePackages = pkgs.haskell.packages.ghc8107;
          basePackages = pkgs.haskell.packages.ghc92;
          imports = [
            inputs.references.haskellFlakeProjectModules.output
          ];
          packages = {
            classyplate.source = inputs.classyplate;
            ghc-hasfield-plugin.source = inputs.ghc-hasfield-plugin;
            large-records.source = inputs.large-records + /large-records;
            large-generics.source = inputs.large-records + /large-generics;
            large-anon.source = inputs.large-records + /large-anon;
            typelet.source = inputs.large-records + /typelet;
            # text.source = "2.0";
            # Cabal-syntax.source = "3.8.1.0";
            # fourmolu.source = "3.8.1.0";
            # record-dot-preprocessor.source = inputs.record-dot-preprocessor;
            # record-dot-preprocessor.source = "0.2.14";
          };
          settings = {
            #  aeson = {
            #    check = false;
            #  };
            #  relude = {
            #    haddock = false;
            #    broken = false;
            #  };
            # primitive-checked = {
            #     broken = false;
            #     jailbreak = true;
            # };
            ghc-hasfield-plugin.check = false;
            ghc-hasfield-plugin.jailbreak = true;
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

{
  inputs = {
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    streamly.url = "github:composewell/streamly/12d85026291d9305f93f573d284d0d35abf40968";
    streamly.flake = false;
    
    # common-ghc8.url = "git+ssh://git@ssh.bitbucket.juspay.net/nix/euler-nix-common.git";
    # sequelize-ghc8 = {
    #   type = "git";
    #   url = "git+ssh://git@ssh.bitbucket.juspay.net/exc/haskell-sequelize";
    #   inputs.common.follows = "common-ghc8";
    #   ref = "master";
    # };

    # common.url = "git+ssh://git@ssh.bitbucket.juspay.net/nix/euler-nix-common.git?ref=ghc928-additions-3";
    # sequelize = {
    #   url = "git+ssh://git@ssh.bitbucket.juspay.net/exc/haskell-sequelize?ref=ghc928";
    #   inputs.common.follows = "common";
    # };

    # ghc 9.2.8 packages
    nixpkgs.url = "github:nixos/nixpkgs/75a52265bda7fd25e06e3a67dee3f0354e73243c";
    classyplate.url = "github:eswar2001/classyplate/a360f56820df6ca5284091f318bcddcd3e065243";
    references.url = "github:eswar2001/references/120ae7826a7af01a527817952ad0c3f5ef08efd0";
    beam.url = "github:juspay/beam/c4f86057db76640245c3d1fde040176c53e9b9a3";
    beam.flake = false;
    large-records.url = "github:eswar2001/large-records/b60bcb312c7d55f1d638aa1a5143696e6586e76d";
    large-records.inputs.beam.follows = "beam";

    # ghc 8.10.7 packages
    ghc8-nixpkgs.url = "github:nixos/nixpkgs/43e3b6af08f29c4447a6073e3d5b86a4f45dd420";
    ghc8-beam.url = "github:juspay/beam/e50e6dc6a5a83c4c0c50183416fad33084c81d9e";
    ghc8-beam.flake = false;
    ghc8-classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    ghc8-classyplate.flake = false;
    ghc8-references.url = "github:eswar2001/references/35912f3cc72b67fa63a8d59d634401b79796469e";
    ghc8-references.flake = true;
    ghc8-ghc-hasfield-plugin.url = "github:juspay/ghc-hasfield-plugin/d82ac5a6c0ad643eebe2b9b32c91f6523d3f30dc";
    ghc8-ghc-hasfield-plugin.flake = false;
    ghc8-large-records.url = "github:eswar2001/large-records/e393f4501d76a98b4482b0a5b35d120ae70e5dd3";
    ghc8-large-records.flake = false;
    ghc8-record-dot-preprocessor.url = "github:ndmitchell/record-dot-preprocessor/99452d27f35ea1ff677be9af570d834e8fab4caf";
    ghc8-record-dot-preprocessor.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ...}: {
      systems = import inputs.systems;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, system, ... }: {
        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.

        # GHC 8 support
        haskellProjects.ghc8 = {
          projectFlakeName = "spider";
          basePackages = inputs.ghc8-nixpkgs.legacyPackages.${system}.haskell.packages.ghc8107;
          imports = [
            inputs.ghc8-references.haskellFlakeProjectModules.output
            # inputs.sequelize-ghc8.haskellFlakeProjectModules.output
          ];
          packages = {
            classyplate.source = inputs.ghc8-classyplate;
            ghc-hasfield-plugin.source = inputs.ghc8-ghc-hasfield-plugin;
            large-records.source = inputs.ghc8-large-records + /large-records;
            large-generics.source = inputs.ghc8-large-records + /large-generics;
            large-anon.source = inputs.ghc8-large-records + /large-anon;
            ghc-tcplugin-api.source = "0.7.1.0";
            typelet.source = inputs.ghc8-large-records + /typelet;
            record-dot-preprocessor.source = inputs.ghc8-record-dot-preprocessor;
            streamly-core.source = inputs.streamly + /core;
          };
          settings = {
            beam-core.jailbreak = true;
            sheriff.check = false;
          };
          devShell = {
            mkShellArgs = {
              name = "ghc8-spider";
            };
            hlsCheck.enable = inputs.ghc8-nixpkgs.legacyPackages.${system}.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

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
          # defaults.enable = false;
          # devShell.tools = hp: with hp; {
          #   inherit cabal-install;
          #   inherit hp;
          # };
          projectFlakeName = "spider";
          # basePackages = pkgs.haskell.packages.ghc8107;
          basePackages = pkgs.haskell.packages.ghc92;
          imports = [
            inputs.references.haskellFlakeProjectModules.output
            inputs.classyplate.haskellFlakeProjectModules.output
            inputs.large-records.haskellFlakeProjectModules.output
            # inputs.sequelize.haskellFlakeProjectModules.output
          ];
          packages = {
            streamly-core.source = inputs.streamly + /core;
          };
          settings = {
            sheriff.check = false;
          };

          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            # tools = hp: { fourmolu = null; ghcid = null; };
            mkShellArgs = {
              name = "spider";
            };
            hlsCheck.enable = pkgs.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.fdep;

      };

      flake.haskellFlakeProjectModules = {
        # To use ghc 9 version, use
        # inputs.spider.haskellFlakeProjectModules.output

        # To use ghc 8 version, use
        # inputs.spider.haskellFlakeProjectModules.output-ghc8

        output-ghc9 = { pkgs, lib, ... }: withSystem pkgs.system ({ config, ... }:
            config.haskellProjects."default".defaults.projectModules.output
        );

        output-ghc8 = { pkgs, lib, ... }: withSystem pkgs.system ({ config, ... }:
            config.haskellProjects."ghc8".defaults.projectModules.output
        );
      };
    });
}

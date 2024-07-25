{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/75a52265bda7fd25e06e3a67dee3f0354e73243c";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    classyplate.url = "github:eswar2001/classyplate/a360f56820df6ca5284091f318bcddcd3e065243";
    beam.url = "github:well-typed/beam/57a12e68727c027f0f1c25752f8c5704ddbe1516";
    beam.flake = false;
    references.url = "github:eswar2001/references/120ae7826a7af01a527817952ad0c3f5ef08efd0";
    large-records = { 
      url = "github:eswar2001/large-records/b60bcb312c7d55f1d638aa1a5143696e6586e76d";
      inputs.beam.follows = "beam";
    };
    streamly = {
      url = "github:composewell/streamly/12d85026291d9305f93f573d284d0d35abf40968";
      flake = false;
    };
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
          ];
          packages = {
            streamly-core.source = inputs.streamly + /core;
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
            sheriff.check = false;
          };

          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            # tools = hp: { fourmolu = null; ghcid = null; };

            hlsCheck.enable = pkgs.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.fdep;
      };
    };
}

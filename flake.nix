{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/75a52265bda7fd25e06e3a67dee3f0354e73243c";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    classyplate.flake = false;
    classyplate.url = "github:iamanandsingh/classyplate/2c869097df8fdbeb1da23842bf5f52366daf3893";
    references.flake = true;
    references.url = "github:iamanandsingh/references/8366257fbeb4f63e1ec0347c9fe5f203ba6fdebc";
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
          basePackages = pkgs.haskell.packages.ghc92;
          packages = {
            references.source = inputs.references;
            classyplate.source = inputs.classyplate;
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

{
  inputs = {
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    # Same nixpkgs pin as euler-nix-common's ghc984 set, so that `ghc98` == GHC
    # 9.8.4 and the patched GHC derivation is byte-identical to euler's
    # `ghc98-perf-events` — hence substitutable from cache.nixos.asia/juspay
    # rather than requiring a from-source GHC build.
    nixpkgs.url = "github:nixos/nixpkgs/89c2b2330e733d6cdb5eae7b899326930c2c0648";

    # Only the `core` sub-package is consumed.
    streamly.url = "github:composewell/streamly/12d85026291d9305f93f573d284d0d35abf40968";
    streamly.flake = false;

    # GHC 9.8.4-ported dependency forks. Revs match the ones locked by
    # euler-nix-common's ghc984 set (and the other euler repos), so these resolve
    # from the shared juspay cache.
    classyplate = {
      url = "github:infinitumkiran/classyplate/71022deb4163c39ef278e30c2c1d3e56a3137812";
      flake = false;
    };
    references = {
      url = "github:infinitumkiran/references/663c62cddf86d84f5f91568f5504e65e92cf7461";
      flake = false;
    };
    # Dependency of `references`; the nixpkgs Hackage version is marked broken.
    instance-control = {
      url = "github:infinitumkiran/instance-control/7a0ab66ffa44f8634857440701b8451a20436756";
      flake = false;
    };
    ghc-hasfield-plugin = {
      url = "github:eswar2001/ghc-hasfield-plugin/13887ab3f0d26bc724300521c012bf335e1945c6";
      flake = false;
    };
    record-dot-preprocessor = {
      url = "github:AyushChaturvedi-7/record-dot-preprocessor/de31a3a89b1d89a94fb4b5f8c0506d2f2cde89bf";
      flake = false;
    };
    # NOTE: the large-anon family (large-anon / large-records / large-generics /
    # typelet) is intentionally NOT pinned to a fork here. It resolves from
    # Hackage via the shared all-cabal-hashes pin below (large-anon 0.3.3,
    # large-records 0.4.4), exactly as euler-nix-common's ghc984 set does — that
    # is the combination proven to build under GHC 9.8.4. The old
    # infinitumkiran/large-records fork resolved to large-anon 0.2, which does
    # not build under 9.8.4.
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = import inputs.systems;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, system, ... }:
        let
          # Patched GHC 9.8.4 adding the `desugarResultAction` plugin hook (used by
          # the `warner` plugin) and the thread-statistics primop. Applies the exact
          # same patch set, in the same order, as euler-nix-common's
          # `ghc98-perf-events`, so the resulting compiler derivation is identical
          # and substitutable from the juspay cache. The patch is purely additive,
          # so every other package builds against it unchanged.
          ghc-desugar-plugin-overlay = final: prev: {
            haskell = prev.haskell // {
              compiler = prev.haskell.compiler // {
                ghc98-desugar-plugin = prev.haskell.compiler.ghc98.overrideAttrs (drv: {
                  patches = (drv.patches or [ ]) ++ [
                    ./ghc-patches/0001-Add-a-primop-to-get-the-thread-statistics.patch
                    ./ghc-patches/added-support-for-desugar-plugin.patch
                  ];
                });
              };
              packages = prev.haskell.packages // {
                ghc98-desugar-plugin = prev.haskell.packages.ghc98.override {
                  buildHaskellPackages = final.buildPackages.haskell.packages.ghc98-desugar-plugin;
                  ghc = final.buildPackages.haskell.compiler.ghc98-desugar-plugin;
                };
              };
            };
          };
        in
        {
          _module.args.pkgs = import inputs.nixpkgs {
            overlays = [ ghc-desugar-plugin-overlay ];
            inherit system;
          };

          haskellProjects.default = {
            projectFlakeName = "spider";
            # `warner` needs the patched GHC (desugarResultAction hook), so it is
            # the project-wide base. Same all-cabal-hashes pin as euler-nix-common
            # so Hackage version resolution (ghc-tcplugin-api etc.) lines up.
            basePackages = pkgs.haskell.packages.ghc98-desugar-plugin.override {
              all-cabal-hashes = builtins.fetchurl {
                url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/0c3c1e49cb6c1ba8419d11e259eb72f2e89e76ca.tar.gz";
                sha256 = "1qs0cxvzjpsysnp5fm5i6b8p9vb2rsdw9pcyqaf8gi8nv6ppv40k";
              };
            };

            packages = {
              streamly-core.source = inputs.streamly + /core;
              classyplate.source = inputs.classyplate;
              references.source = inputs.references;
              instance-control.source = inputs.instance-control;
              ghc-hasfield-plugin.source = inputs.ghc-hasfield-plugin;
              record-dot-preprocessor.source = inputs.record-dot-preprocessor;
              # large-anon / large-records / large-generics / typelet come from
              # Hackage (all-cabal-hashes pin above), matching euler-nix-common.
              ghc-tcplugin-api.source = "0.16.1.0";
            };

            settings = {
              classyplate = {
                jailbreak = true;
                broken = false;
              };
              record-dot-preprocessor.jailbreak = true;
              # The de31a3a record-dot-preprocessor rev no longer auto-injects
              # GHC.Records(.Extra) imports (downstream modules import them
              # themselves now). ghc-hasfield-plugin's bundled test suite relied
              # on that injection and no longer compiles; its library is fine and
              # spider never runs the test, so skip the check.
              ghc-hasfield-plugin.check = false;
              # Hackage large-anon 0.3.3 / typelet — same overrides euler-nix-common
              # uses to build the family under GHC 9.8.4. large-records and
              # large-generics build with nixpkgs defaults (no override needed).
              large-anon = {
                broken = false;
                check = false;
              };
              typelet = {
                broken = false;
                jailbreak = true;
              };
              servant.jailbreak = true;
              servant-server.jailbreak = true;

              # Local plugin packages self-apply their plugin in the test-suite
              # (needs a running collector / extra setup); skip checks.
              sheriff.check = false;
              fdep.check = false;
              api-contract.check = false;
              fieldInspector.check = false;
              warner.check = false;
              paymentFlow.check = false;
              endpoints.check = false;
              dc.check = false;
              keyLookupTracker.check = false;
              coresyn2chart.check = false;

              # api-contract disables its own profiling library; fieldInspector
              # imports ApiContract.Plugin at compile time, so match it — otherwise
              # fieldInspector's profiling pass can't find api-contract's
              # (never-built) profiling library.
              api-contract.libraryProfiling = false;
              fieldInspector.libraryProfiling = false;
            };

            devShell = {
              mkShellArgs = {
                name = "spider-ghc98";
              };
              # HLS 2.x fails to configure against this patched GHC and isn't
              # needed for the build loop.
              tools = hp: {
                haskell-language-server = null;
              };
              hlsCheck.enable = false;
            };
          };

          packages.default = self'.packages.sheriff;
        };
    });
}

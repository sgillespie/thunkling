{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    treefmt.url = "github:numtide/treefmt-nix";

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
    };

    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;

          overlays = [ inputs.haskellNix.overlay ];
        };

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc96";
          shell = {
            withHoogle = true;

            tools = {
              cabal = "latest";
              haskell-language-server = "latest";
            };

            buildInputs = [ ];
            inputsFrom = [ formatters.config.build.devShell ];
          };
        };

        formatters = inputs.treefmt.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs = {
            fourmolu.enable = true;
            hlint.enable = true;
            mdformat.enable = true;
            nixfmt.enable = true;
          };

          settings = {
            global.excludes = [ "LICENSE" ];

            formatter.cabal-gild = {
              command = "${pkgs.bash}/bin/bash";

              options =
                let
                  cabal-gild = project.tool "cabal-gild" "latest";
                in
                [
                  "-euc"
                  ''
                    for file in "$@"; do
                      ${cabal-gild}/bin/cabal-gild --io="$file"
                    done
                  ''
                  "--"
                ];

              includes = [
                "*.cabal"
                "cabal.project"
              ];
            };
          };
        };

        flake = project.flake { };
      in
      pkgs.lib.recursiveUpdate flake {
        inherit project;

        packages.default = flake.packages."thunkling:exe:thlc";
        legacyPackages = pkgs;

        checks.formatting = formatters.config.build.check inputs.self;

        formatter = formatters.config.build.wrapper;
      }
    );

  nixConfig = {
    allow-import-from-derivation = true;

    extra-substituters = [
      "https://sgillespie.cachix.org"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];

    extra-trusted-public-keys = [
      "sgillespie.cachix.org-1:Zgif/WHW2IzHqbMb1z56cMmV5tLAA+zW9d5iB5w/VU4="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}

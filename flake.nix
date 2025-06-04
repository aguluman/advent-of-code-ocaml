# flake.nix
{
  description = "Advent of Code solutions in OCaml - Multi-year competitive programming project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # OCaml development dependencies
        ocamlPackages = with pkgs.ocamlPackages; [
          dune_3
          base
          stdio
          ounit2
          str
          ocamlformat
          odoc
          merlin
          ocaml-lsp
          utop
        ];

        # Build tools and utilities
        buildTools = with pkgs; [
          ocaml
          watchexec
          hyperfine
          gnumake
          direnv
          git
        ];

        # Function to build a single day's solution
        buildDay = year: day: 
          let
            dayPath = "${year}/${day}";
            dayExists = builtins.pathExists ./${dayPath};
          in
          if dayExists then
            pkgs.stdenv.mkDerivation {
              pname = "aoc-${year}-${day}";
              version = "0.1.0";
              src = ./${dayPath};
              
              nativeBuildInputs = [ pkgs.ocaml pkgs.dune_3 ] ++ ocamlPackages;
              
              buildPhase = ''
                runHook preBuild
                dune build --profile release
                runHook postBuild
              '';
              
              installPhase = ''
                runHook preInstall
                mkdir -p $out/bin
                dune install --prefix $out --profile release
                runHook postInstall
              '';
              
              meta = with pkgs.lib; {
                description = "Advent of Code ${year} Day ${day} solution";
                license = licenses.mit;
                platforms = platforms.unix;
              };
            }
          else null;

        # Function to build all days for a year
        buildYear = year:
          let
            yearPath = ./${year};
            yearExists = builtins.pathExists yearPath;
            days = if yearExists then 
              builtins.filter (name: builtins.match "day[0-9][0-9]" name != null)
                (builtins.attrNames (builtins.readDir yearPath))
            else [];
            
            dayPackages = builtins.listToAttrs (
              map (day: {
                name = "${year}-${day}";
                value = buildDay year day;
              }) days
            );
          in
          if yearExists && days != [] then
            pkgs.symlinkJoin {
              name = "aoc-${year}-all";
              paths = builtins.filter (pkg: pkg != null) (builtins.attrValues dayPackages);
              meta.description = "All Advent of Code ${year} solutions";
            }
          else null;

        # Available years (you can extend this list)
        availableYears = [ "2024" ];
        
        # Generate packages for all years and days
        yearPackages = builtins.listToAttrs (
          map (year: {
            name = "all-${year}";
            value = buildYear year;
          }) availableYears
        );

        # Individual day packages
        dayPackages = builtins.listToAttrs (
          pkgs.lib.flatten (
            map (year:
              let
                yearPath = ./${year};
                days = if builtins.pathExists yearPath then 
                  builtins.filter (name: builtins.match "day[0-9][0-9]" name != null)
                    (builtins.attrNames (builtins.readDir yearPath))
                else [];
              in
              map (day: {
                name = "${day}-${year}";
                value = buildDay year day;
              }) days
            ) availableYears
          )
        );

      in {
        # Development shell (replaces your shell.nix)
        devShells.default = pkgs.mkShell {
          buildInputs = buildTools ++ ocamlPackages;
          
          shellHook = ''
            echo "🐪 Advent of Code - OCaml Development Environment 🐪"
            echo "NixOS: $(nixos-version 2>/dev/null || echo 'Not on NixOS')"
            echo "OCaml: $(ocaml -version)"
            echo "Dune: $(dune --version)"
            echo ""
            echo "Available commands:"
            echo "  make run DAY=XX           - Run specific day"
            echo "  make run-release DAY=XX   - Run in release mode"
            echo "  make test DAY=XX          - Run tests"
            echo "  nix build .#day01-2024    - Build specific day"
            echo "  nix run .#day01-2024      - Run specific day"
            echo ""
            echo "Flake commands:"
            echo "  nix flake update          - Update dependencies"
            echo "  nix flake show            - Show available packages"
            echo ""
          '';

          # Set up OCaml environment
          OCAML_TOPLEVEL_PATH = "${pkgs.ocamlPackages.findlib}/lib/ocaml/*/site-lib";
        };

        # All packages
        packages = yearPackages // dayPackages // {
          # Default package - all available solutions
          default = if yearPackages != {} then
            pkgs.symlinkJoin {
              name = "aoc-all-years";
              paths = builtins.filter (pkg: pkg != null) (builtins.attrValues yearPackages);
              meta.description = "All Advent of Code solutions across all years";
            }
          else pkgs.writeText "aoc-placeholder" "No solutions built yet";
        };

        # Apps for easy running with `nix run`
        apps = 
          let
            dayApps = builtins.listToAttrs (
              pkgs.lib.flatten (
                map (year:
                  let
                    yearPath = ./${year};
                    days = if builtins.pathExists yearPath then 
                      builtins.filter (name: builtins.match "day[0-9][0-9]" name != null)
                        (builtins.attrNames (builtins.readDir yearPath))
                    else [];
                  in
                  map (day: 
                    let dayPkg = buildDay year day; in
                    if dayPkg != null then {
                      name = "${day}-${year}";
                      value = flake-utils.lib.mkApp {
                        drv = dayPkg;
                        name = "${day}";
                      };
                    } else null
                  ) days
                ) availableYears
              )
            );
          in
          dayApps // {
            default = if dayApps != {} then
              (builtins.head (builtins.attrValues dayApps))
            else flake-utils.lib.mkApp {
              drv = pkgs.writeShellScriptBin "aoc-help" ''
                echo "No Advent of Code solutions available yet!"
                echo "Build some days first, then run with: nix run .#day01-2024"
              '';
            };
          };

        # Formatter for `nix fmt`
        formatter = pkgs.nixpkgs-fmt;

        # Checks for `nix flake check`
        checks = {
          # Check that the flake builds
          build-test = self.packages.${system}.default;
          
          # Check formatting
          format-check = pkgs.runCommand "format-check" {
            nativeBuildInputs = [ pkgs.nixpkgs-fmt ];
          } ''
            nixpkgs-fmt --check ${./flake.nix}
            touch $out
          '';
        };
      });
}
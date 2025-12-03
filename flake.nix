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
          ocamlformat
          ocaml-lsp
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

        # Auto-detect years like your Makefile does
        availableYears = 
          let
            allEntries = builtins.readDir ./.;
            yearDirs = builtins.filter 
              (name: builtins.match "[0-9][0-9][0-9][0-9]" name != null)
              (builtins.attrNames allEntries);
          in
          builtins.sort (a: b: a > b) yearDirs;  # Sort newest first like Makefile

        # Get the current/latest year (like YEAR variable in Makefile)
        currentYear = if builtins.length availableYears > 0 
                     then builtins.head availableYears 
                     else "2024";  # Fallback

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

        # Function to get all days for a given year
        getDaysForYear = year:
          let
            yearPath = ./${year};
            yearExists = builtins.pathExists yearPath;
          in
          if yearExists then 
            builtins.filter (name: builtins.match "day[0-9][0-9]" name != null)
              (builtins.attrNames (builtins.readDir yearPath))
          else [];

        # Function to build all days for a year
        buildYear = year:
          let
            days = getDaysForYear year;
            dayPackages = builtins.listToAttrs (
              map (day: {
                name = "${year}-${day}";
                value = buildDay year day;
              }) days
            );
          in
          if days != [] then
            pkgs.symlinkJoin {
              name = "aoc-${year}-all";
              paths = builtins.filter (pkg: pkg != null) (builtins.attrValues dayPackages);
              meta.description = "All Advent of Code ${year} solutions";
            }
          else null;

        # Generate packages for all years and days
        yearPackages = builtins.listToAttrs (
          map (year: {
            name = "all-${year}";
            value = buildYear year;
          }) availableYears
        );

        # Individual day packages for all years
        dayPackages = builtins.listToAttrs (
          pkgs.lib.flatten (
            map (year:
              let days = getDaysForYear year; in
              map (day: {
                name = "${day}-${year}";
                value = buildDay year day;
              }) days
            ) availableYears
          )
        );

        # Create apps for all year/day combinations
        dayApps = builtins.listToAttrs (
          pkgs.lib.flatten (
            map (year:
              let days = getDaysForYear year; in
              map (day: 
                let dayPkg = buildDay year day; in
                if dayPkg != null then {
                  name = "${day}-${year}";
                  value = flake-utils.lib.mkApp {
                    drv = dayPkg;
                    name = day;
                  };
                } else null
              ) days
            ) availableYears
          )
        );

      in {
        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = buildTools ++ ocamlPackages;
          
          shellHook = ''
            echo "🐪 Advent of Code - OCaml Development Environment 🐪"
            echo "NixOS: $(nixos-version 2>/dev/null || echo 'Not on NixOS')"
            echo "OCaml: $(ocaml -version)"
            echo "Dune: $(dune --version)"
            echo ""
            echo "Auto-detected years: ${toString availableYears}"
            echo "Current year: ${currentYear}"
            echo "Total days available: ${toString (builtins.length (builtins.attrNames dayPackages))}"
            echo ""
            echo "Available commands:"
            echo "  make run DAY=XX           - Run specific day"
            echo "  make run-release DAY=XX   - Run in release mode"
            echo "  make test DAY=XX          - Run tests"
            echo "  nix build .#day01-${currentYear}    - Build specific day"
            echo "  nix run .#day01-${currentYear}      - Run specific day"
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
          # Default package - current year or all years
          default = if yearPackages ? "all-${currentYear}" then
            yearPackages."all-${currentYear}"
          else if yearPackages != {} then
            pkgs.symlinkJoin {
              name = "aoc-all-years";
              paths = builtins.filter (pkg: pkg != null) (builtins.attrValues yearPackages);
              meta.description = "All Advent of Code solutions across all years";
            }
          else 
            pkgs.writeText "aoc-placeholder" "No solutions built yet";
        };

        # Apps for easy running with `nix run`
        apps = dayApps // {
          default = if dayApps != {} then
            # Default to the most recent day of the current year
            let
              currentYearDays = builtins.filter 
                (name: pkgs.lib.hasSuffix currentYear name) 
                (builtins.attrNames dayApps);
              sortedDays = builtins.sort (a: b: a > b) currentYearDays;
            in
            if builtins.length sortedDays > 0 then
              dayApps.${builtins.head sortedDays}
            else
              (builtins.head (builtins.attrValues dayApps))
          else 
            flake-utils.lib.mkApp {
              drv = pkgs.writeShellScriptBin "aoc-help" ''
                echo "No Advent of Code solutions available yet!"
                echo "Available years: ${toString availableYears}"
                echo "Create some days first, then run with: nix run .#day01-${currentYear}"
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

          # Verify year detection works
          year-detection-test = pkgs.runCommand "year-detection-test" {} ''
            echo "Detected years: ${toString availableYears}"
            echo "Current year: ${currentYear}"
            ${if builtins.length availableYears > 0 
              then "echo 'Year detection: PASS'"
              else "echo 'Year detection: FAIL' && exit 1"}
            touch $out
          '';
        };
      });
}
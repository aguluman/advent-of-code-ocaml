{ pkgs ? import <nixpkgs> { } }:

let
  # Import the default.nix to reuse the build configuration
  aoc-package = import ./default.nix { inherit pkgs; };
  
  # Define explicit days to build - this section can be updated by scripts
  explicitDays = [
    "2024/day01"
    "2024/day02"
    "2024/day03"
    "2024/day04"
    "2024/day05"
    "2024/day06"
    "2024/day07"
    "2024/day08"
    "2024/day09"
    "2024/day10"
    "2024/day11"
    "2024/day12"
    "2024/day13"
    "2024/day14"
    "2024/day15"
    "2024/day16"
    "2024/day17"
    "2024/day18"
    "2024/day19"
    "2024/day20"
    "2024/day21"
    "2024/day22"
    "2024/day23"
    "2024/day24"
    "2024/day25"
    # Add new days as they are created
  ];
  
  # Also auto-detect directories as a fallback
  days = let
    # Find the most recent year directory (assumes year directories like 2024, 2023, etc.)
    years = builtins.filter (x: builtins.match "[0-9]{4}" x != null) 
                        (builtins.attrNames (builtins.readDir ./.));
    currentYear = builtins.head (builtins.sort (a: b: a > b) years);
    
    # Read all entries in the year directory
    yearDir = if builtins.length years > 0 then ./${currentYear} else ./.;
    dirEntries = builtins.readDir yearDir;
    
    # Filter to only get day directories (matching "day##" pattern)
    isDayDir = name: type: 
      type == "directory" && builtins.match "day[0-9]{2}" name != null;
    
    # Extract just the directory names that match day pattern
    dayDirs = builtins.attrNames 
      (builtins.filterAttrs isDayDir dirEntries);
    
    # If we have a year structure, prefix with year, otherwise use as-is
    prefixYear = day: 
      if builtins.length years > 0 
      then "${currentYear}/${day}" 
      else day;
      
    # Get the list of day directories, either from explicit list or auto-detection
    finalDayDirs = 
      if builtins.length explicitDays > 0 
      then explicitDays 
      else builtins.sort (a: b: a < b) dayDirs;
  in
    if builtins.length explicitDays > 0
    then explicitDays
    else builtins.map prefixYear (builtins.sort (a: b: a < b) dayDirs);
  
  # Function to build a single day
  buildDay = day: 
    let
      dayPackages = with pkgs.ocamlPackages; [
        dune_3
        base
        stdio
        ounit2
        str
        ocamlformat
      ];
    in
    pkgs.stdenv.mkDerivation {
      pname = "aoc-${builtins.replaceStrings ["/"] ["-"] day}";
      version = "0.1.0";
      src = ./${day};
      
      nativeBuildInputs = [ pkgs.ocaml pkgs.dune_3 ] ++ dayPackages;
      
      buildPhase = ''
        runHook preBuild
        dune build
        runHook postBuild
      '';
      
      installPhase = ''
        runHook preInstall
        mkdir -p $out/bin
        find _build -name "*.exe" -type f -executable -exec cp {} $out/bin/ \;
        runHook postInstall
      '';
      
      meta = with pkgs.lib; {
        description = "Advent of Code 2024 - ${day}";
        platforms = platforms.unix;
      };
    };
  
  # Map the buildDay function over all days
  dayPackages = builtins.listToAttrs (
    builtins.map (day: { 
      name = builtins.replaceStrings ["/"] ["-"] day; 
      value = buildDay day; 
    }) days
  );
in
{
  # Export the entire package
  inherit aoc-package;
  
  # Export each day individually
  days = dayPackages;
  
  # Create a combined package with all binaries
  all = pkgs.symlinkJoin {
    name = "advent-of-code-ocaml-all";
    paths = builtins.attrValues dayPackages;
  };
}

{ pkgs ? import <nixpkgs> { } }:

let
  # OCaml packages needed for the project
  ocamlPackages = with pkgs.ocamlPackages; [
    dune_3
    base
    stdio
    ounit2
    str
    ocamlformat
    odoc
  ];
in
pkgs.stdenv.mkDerivation {
  pname = "advent-of-code-ocaml";
  version = "0.1.0";
  
  src = ./.;
  
  nativeBuildInputs = [ pkgs.ocaml pkgs.dune_3 ] ++ ocamlPackages;
  
  buildPhase = ''
    runHook preBuild
    
    # Build all days using the makefile
    make build
    
    runHook postBuild
  '';
  
  installPhase = ''
    runHook preInstall
    
    mkdir -p $out/bin
    
    # Find and copy all built executables
    find . -name "*.exe" -type f -executable -exec cp {} $out/bin/ \;
    
    # Also copy the source for reference
    mkdir -p $out/src
    cp -r 20*/ $out/src/ 2>/dev/null || true
    
    runHook postInstall
  '';
  
  meta = with pkgs.lib; {
    description = "Solutions for Advent of Code 2024 in OCaml";
    homepage = "https://github.com/aguluman/advent-of-code-ocaml";
    license = licenses.mit;  
    maintainers = [ "Chukwuma Akunyili" ];
    platforms = platforms.unix;
  };
}

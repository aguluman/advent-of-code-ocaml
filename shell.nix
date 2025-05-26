{ pkgs ? import <nixpkgs> { } }:

let
  # OCaml version and opam packages
  ocaml-version = pkgs.ocaml;
  opam-packages = with pkgs.ocamlPackages; [
    dune_3
    base
    stdio
    ounit2
    str
    ocamlformat
    odoc
    merlin
    ocaml-lsp
  ];
in
pkgs.mkShell {
  name = "advent-of-code-ocaml";
  
  buildInputs = with pkgs; [
    # OCaml toolchain
    ocaml-version
    opam
    ] ++ opam-packages ++ [
    
    # Development tools
    watchexec        # For file watching
    hyperfine        # For benchmarking
    
    # Additional dependencies
    gnumake          # For the Makefile
    direnv           # For .envrc support
    
    # Optional but useful tools
    rlwrap           # Better REPL experience
    git              # Version control
  ];

  # Shell hook to setup environment
  shellHook = ''
    echo "🐪 Advent of Code - OCaml Development Environment 🐪"
    echo "Run 'make help' to see available commands."
    echo "OCaml version: $(ocaml -version)"
    echo "Dune version: $(dune --version)"
  '';
}

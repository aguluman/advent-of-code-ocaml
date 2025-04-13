[Advent of Code 2024](https://adventofcode.com/2024) using OCaml

To get started with OCaml development.

- Install Opam using this link, [Opam Installation Guide per OS](https://opam.ocaml.org/doc/Install.html)

- Follow the guidelines on this page to setup OCaml, [OCaml By Example](https://o1-labs.github.io/ocamlbyexample/basics-opam.html)


The command used on Windows OS to read the input file and run the code is:
```
  type "C:\path\to\your\input.txt" | dune exec ./test.exe
```

The command used on Linux OS to read the input file and run the code is:
```
  cat "C:\path\to\your\input.txt" | dune exec ./test.exe
```

### Note
- The parse function for each challenge question’s input file was designed to handle the ```CRLF``` end of line sequence.

- This solution to day 24 part two challenge question is wrong. ` I have not been able to figure out a solution to achieve the    correct. 
- ```I need help```. If you have the knowledge or information please create a [Pull Request](https://github.com/aguluman/advent-of-code-2024-ocaml/pulls).
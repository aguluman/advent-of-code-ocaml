# Advent of Code - OCaml Solutions

[Advent of Code 2024](https://adventofcode.com/2024) using OCaml

## Getting Started

To get started with OCaml development:

- Install Opam using this link: [Opam Installation Guide per OS](https://opam.ocaml.org/doc/Install.html)
- Follow the guidelines on this page to setup OCaml: [OCaml By Example](https://o1-labs.github.io/ocamlbyexample/basics-opam.html)

## Quick Commands

This project includes a comprehensive Makefile with various commands to help you build, test, and run solutions efficiently.

### Building

```bash
  # Build all days in debug mode
  make build

  # Build a specific day (e.g., day 01)
  make build-01

  # Build all days in release mode (optimized)
  make release
```

### Testing

```bash
  # Run tests for all days
  make test

  # Run tests for a specific day (e.g., day 01)
  make test-01
```

### Running Solutions

```bash
  # Run a specific day with downloaded input
  make run-day DAY=01 INPUT=download

  # Run a specific day with custom input file
  make run-day DAY=01 INPUT=path/to/input.txt

  # Run the most recent day with downloaded input
  make run-current INPUT=download

  # Run in release mode (faster execution)
  make run-release DAY=01 INPUT=download
```

### Project Management

```bash
  # Create a new day from template
  make new-day

  # Download puzzle input (requires AUTH_TOKEN in .env file)
  make download DAY=01

  # Clean build artifacts
  make clean

  # Format code
  make fmt

  # Check code formatting
  make fmt-check
```

### Benchmarking

```bash
  # Run benchmarks for all days
  make benchmark

  # Run benchmark for a specific day
  make benchmark-01
```

### Submission

```bash
  # Check submission status for a day
  make check-status DAY=01

  # Submit an answer (requires AUTH_TOKEN in .env file)
  make submit DAY=01 PART=1

  # Run solution and optionally submit answers
  make run-submit DAY=01 INPUT=download
```

### Help

```bash
  # Show all available commands
  make help
```

## Manual Execution

The command used on Windows OS to read the input file and run the code is:
```bash
type "C:\path\to\your\input.txt" | dune exec ./test.exe
```

The command used on Linux OS to read the input file and run the code is:
```bash
cat "/path/to/your/input.txt" | dune exec ./test.exe
```

## Configuration

### Environment Variables

For automatic input downloading and submission features, create a `.env` file in the project root:

```bash
  AUTH_TOKEN=your_advent_of_code_session_token
```

You can find your session token by:
1. Going to [Advent of Code](https://adventofcode.com/)
2. Logging in
3. Opening browser developer tools
4. Finding the `session` cookie value

## Project Structure

- `2024/dayXX/` - Individual day solutions
- `day_template/` - Template for creating new days
- `inputs/2024/` - Downloaded puzzle inputs
- `answers/2024/` - Generated answers for submission
- `benchmark/` - Benchmark results


## Development Setup

### With Nix Flakes (Recommended)
```bash
# Enter development environment
nix develop

# Or with direnv (auto-loads when entering directory)
echo "use flake" > .envrc
direnv allow
```

## Notes

- The parse function for each challenge question's input file was designed to handle the `CRLF` end of line sequence.

- My solution to [day 24 part two challenge question is wrong](https://github.com/aguluman/advent-of-code-2024-ocaml/issues/1). I have not been able to figure out a solution to achieve the correct answer.

- **I need help**. If you have the knowledge or information please create a [Pull Request](https://github.com/aguluman/advent-of-code-2024-ocaml/pulls).

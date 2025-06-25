# Advent of Code - OCaml Solutions

[Advent of Code 2024](https://adventofcode.com/2024) using OCaml

## Getting Started

To get started with OCaml development:

- Install Opam using this link: [Opam Installation Guide per OS](https://opam.ocaml.org/doc/Install.html)
- Follow the guidelines on this page to setup OCaml: [OCaml By Example](https://o1-labs.github.io/ocamlbyexample/basics-opam.html)

## Quick Commands

This project includes a comprehensive Makefile with various commands to help you build, test, and run solutions efficiently.

### Testing

```bash
  # Run tests for all days
  make test

  # Run tests for a specific day (e.g., day 01)
  make test-01
```

### Code Formatting

```bash
  # Format all code using dune fmt
  make fmt

  # Format code for a specific day (e.g., day 01)
  make fmt-01
```

### Running Solutions

```bash
  # Run a specific day with downloaded input (auto-saves answers)
  make run-day DAY=01 INPUT=download

  # Run a specific day with custom input file
  make run-day DAY=01 INPUT=path/to/input.txt

  # Run a specific day using repository inputs or fallback locations
  make run-day DAY=01 INPUT=puzzle_input

  # Run in release mode (faster execution, auto-saves answers)
  make run-release DAY=01 INPUT=download
  make run-release DAY=01 INPUT=puzzle_input

  # Run the most recently modified day with input (workflow-aware, no answer saving)"
  make run-current INPUT=download
  make run-current INPUT=puzzle_input
```

### Project Management

```bash
  # Create a new day from template (interactive, auto-fetches problem title)
  make new-day

  # Download puzzle input (requires AUTH_TOKEN in .env file)
  make download DAY=01

  # Force download input (overwrite existing)
  make download DAY=01 FORCE=1

  # Clean all build artifacts
  make clean
```

### Benchmarking

```bash
  # Run benchmark for a specific day
  make benchmark-01
```

### Submission

```bash
  # Check submission status for a day
  make check-status DAY=01

  # Submit an answer (requires AUTH_TOKEN in .env file)
  make submit DAY=01 PART=1
  make submit DAY=01 PART=2

  # Run solution with interactive submission prompts
  make run-submit DAY=01 INPUT=download
```

### Help

```bash
  # Show all available commands
  make help
```

## 🎥 Complete Automation Demo

[![Advent of Code OCaml: Complete Automation Pipeline](https://img.youtube.com/vi/oYCMW7mid_8/0.jpg)](https://www.youtube.com/watch?v=oYCMW7mid_8)

**🎄 Watch the Complete Automation Pipeline in Action!**

See how this project eliminates all manual steps between solving and submitting Advent of Code solutions:

🚀 **What the video demonstrates:**
- Automatic puzzle input downloading with authentication
- Lightning-fast OCaml solution execution (0.3s runtime!)
- Beautiful Christmas tree visualization for Day 14 Part 2
- Smart submission system that prevents duplicate submissions
- Individual part submission (Part 1 & Part 2 separately)
- Bulk submission with interactive prompts
- Complete terminal-based workflow - no browser switching!

⚡ **Key features shown:**
- `make new-day` - Creates day template with problem title fetching
- `make download DAY=14` - Auto-downloads puzzle input
- `make run-current INPUT=download` - Runs latest solution with downloaded input
- `make submit DAY=14 PART=1` - Smart individual part submission
- `make run-submit DAY=14 INPUT=download` - Complete pipeline: download → run → submit

## Manual Execution

For manual execution without using the Makefile commands, navigate to the specific day directory first:

```bash
cd 2024/day01
```

Then use the appropriate command for your operating system:

**Windows:**
```bash
  type "../../inputs/2024/day01.txt" | dune exec ./test.exe
```

**Linux:**
```bash
  cat "../../inputs/2024/day01.txt" | dune exec ./test.exe
```

**Note:** The Makefile commands (like `make run-day DAY=01 INPUT=download`) are recommended as they handle paths automatically and provide additional features like answer saving.


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

- The parse function for each challenge question's input file was designed to handle the `LF` end of line sequence.

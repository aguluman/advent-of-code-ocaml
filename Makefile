.PHONY: all help test fmt fmt-check clean new-day run-day run-release run-current \
	download check-status submit run-submit flake flake-build flake-run flake-update

# Recipes use bash features.
SHELL := bash
MAKEFLAGS += --no-print-directory

# Default target
all: help

# Variables
# Latest year directory (e.g. 2025). Override with: make ... YEAR=2024
YEAR ?= $(lastword $(sort $(wildcard [0-9][0-9][0-9][0-9])))

ifeq ($(YEAR),)
$(error No year directories found. Please create a directory like 2024 first.)
endif
export YEAR

# ---------------------------------------------------------------------------
# Shell helpers shared by run-day, run-release, run-current, download, submit
# and run-submit. This is plain bash (no $$ doubling): $(value ...) hands it to
# the environment unexpanded, and recipes load it with: eval "$$AOC_LIB".
# Recipes run it inside ( ... ) so `exit` in die() only ends that subshell.
# Every step checks its own errors: bash ignores `set -e` in code called from
# `||` or `if`, which is exactly how the recipes call these functions.
# ---------------------------------------------------------------------------
define AOC_LIB_SRC
set -o pipefail
AOC_USER_AGENT="github.com/aguluman/advent-of-code-ocaml"

die() {
  printf '%b\n' "$*" >&2
  exit 1
}

require_day() {
  [ -n "${1:-}" ] || die "Please specify a day number using DAY=XX"
}

# "05" -> "5": the AoC website uses unpadded day numbers.
day_number() {
  local n
  n=$(printf '%s' "$1" | sed 's/^0*//')
  printf '%s' "${n:-0}"
}

# The session cookie from .env, with CR (Windows line endings) and whitespace
# stripped, since either one silently breaks the cookie.
session_token() {
  local token
  token=$(grep -E '^AUTH_TOKEN=' .env 2>/dev/null | head -n 1 | cut -d= -f2- | tr -d '\r[:space:]')
  [ -n "$token" ] || die "No session token found in .env. Add a line: AUTH_TOKEN=your_token"
  printf '%s' "$token"
}

# True when FILE exists, is non-empty, and is not one of AoC's error pages.
input_is_valid() {
  [ -s "$1" ] || return 1
  ! head -c 300 "$1" | grep -qiE 'puzzle inputs differ|please log in|<!doctype|<html|not found|before it unlocks'
}

# Download inputs/YEAR/dayDD.txt. It goes to a temporary file first and is
# only moved into place once checked, so a failure never leaves a bad input.
aoc_download() {
  local day=$1 out tmp token url preview
  require_day "$day"
  out="inputs/$YEAR/day$day.txt"
  if [ "${FORCE:-}" != 1 ] && input_is_valid "$out"; then
    echo "Input file already exists: $out (use FORCE=1 to overwrite)"
    return 0
  fi
  if [ -e "$out" ] && ! input_is_valid "$out"; then
    echo "Existing $out is empty or not a puzzle input; downloading it again."
  fi
  token=$(session_token) || exit 1
  url="https://adventofcode.com/$YEAR/day/$(day_number "$day")/input"
  mkdir -p "inputs/$YEAR" || die "Could not create inputs/$YEAR"
  tmp=$(mktemp "inputs/$YEAR/.day$day.XXXXXX") || die "Could not create a temporary file"
  # Remove the temporary file even if the download is interrupted (Ctrl-C).
  trap 'rm -f "$tmp"' EXIT INT TERM
  echo "Downloading $url"
  # -f: treat HTTP errors (400 bad cookie, 404 not unlocked yet) as failures.
  if ! curl -fsS --cookie "session=$token" -A "$AOC_USER_AGENT" -o "$tmp" "$url"; then
    rm -f "$tmp"
    die "Download failed. Is AUTH_TOKEN in .env still valid, and is day $day unlocked?"
  fi
  if ! input_is_valid "$tmp"; then
    preview=$(head -c 200 "$tmp")
    rm -f "$tmp"
    die "The downloaded data is not a puzzle input:\n$preview"
  fi
  mv "$tmp" "$out" || { rm -f "$tmp"; die "Could not save $out"; }
  echo "Saved $out ($(wc -l <"$out" | tr -d ' ') lines)"
}

# Turn INPUT (download | puzzle_input | a path) into an absolute path to a
# valid input file. Messages go to stderr; the path is the only stdout.
resolve_input() {
  local day=$1 input=$2 path="" downloads candidate
  case "$input" in
    download)
      aoc_download "$day" >&2 || exit 1
      path="inputs/$YEAR/day$day.txt"
      ;;
    puzzle_input)
      mkdir -p "inputs/$YEAR"
      if [ -n "${AOC_DOWNLOADS:-}" ]; then
        downloads=$AOC_DOWNLOADS
      elif [ -d /mnt/c/Users/chukw/Downloads ]; then
        downloads=/mnt/c/Users/chukw/Downloads
      else
        downloads=$HOME/Downloads
      fi
      for candidate in "inputs/$YEAR/day$day.txt" "inputs/$YEAR/input.txt" "$downloads/input.txt"; do
        if [ -f "$candidate" ]; then
          path=$candidate
          break
        fi
      done
      [ -n "$path" ] || die "No input found. Looked for:\n  inputs/$YEAR/day$day.txt\n  inputs/$YEAR/input.txt\n  $downloads/input.txt"
      ;;
    *) path=$input ;;
  esac
  case "$path" in
    /* | [A-Za-z]:/*) ;;
    *) path="$PWD/$path" ;;
  esac
  [ -f "$path" ] || die "Input file not found: $path"
  input_is_valid "$path" || die "Input file is empty or is not a puzzle input: $path\nRe-download it with: make download DAY=$day FORCE=1"
  echo "Using input file: $path" >&2
  printf '%s\n' "$path"
}

# Print the answer after "Part N:" in an output file, or nothing.
extract_answer() {
  { grep -m 1 "^Part $1:" "$2" || true; } | cut -d: -f2- | tr -d '[:space:]'
}

# Write answers/YEAR/submit_dayDD.txt. An unchanged answer keeps its
# "[Status: ...]" marker, so re-running doesn't forget an accepted answer.
save_answers() {
  local day=$1 file="answers/$YEAR/submit_day$1.txt" tmp n answer old old_answer
  mkdir -p "answers/$YEAR" || die "Could not create answers/$YEAR"
  tmp=$(mktemp) || die "Could not create a temporary file"
  for n in 1 2; do
    if [ "$n" = 1 ]; then answer=$2; else answer=$3; fi
    [ -n "$answer" ] || continue
    old=$(grep -m 1 "^Part$n: " "$file" 2>/dev/null || true)
    old_answer=$(printf '%s' "$old" | sed -E "s/^Part$n: //; s/[[:space:]]*\[Status:.*\]$//")
    if [ -n "$old" ] && [ "$old_answer" = "$answer" ]; then
      printf '%s\n' "$old" >>"$tmp"
    else
      printf 'Part%s: %s\n' "$n" "$answer" >>"$tmp"
    fi
  done
  mv "$tmp" "$file" || die "Could not write $file"
  echo "Answers saved to $file"
}

# aoc_run DAY INPUT PROFILE SAVE: build YEAR/dayDD, run it on INPUT, and
# (if SAVE is 1) save the answers. Fails if the build or the solution fails.
aoc_run() {
  local day=$1 input=$2 profile=$3 save=$4 dir output part1 part2
  require_day "$day"
  [ -n "$input" ] || die "Please specify an input: INPUT=path/to/input.txt, INPUT=download or INPUT=puzzle_input"
  dir="$YEAR/day$day"
  [ -d "$dir" ] || die "Day directory not found: $dir"
  input=$(resolve_input "$day" "$input") || exit 1
  echo "Building and running day$day ($profile profile)..."
  (cd "$dir" && dune build --profile "$profile") || die "Build failed for $dir"
  output=$(mktemp) || die "Could not create a temporary file"
  # tee shows output live and keeps a copy; pipefail catches a crash.
  if ! (cd "$dir" && dune exec --profile "$profile" ./test.exe <"$input") 2>&1 | tee "$output"; then
    rm -f "$output"
    die "Solution failed to run properly"
  fi
  if [ "$save" = 1 ]; then
    part1=$(extract_answer 1 "$output")
    part2=$(extract_answer 2 "$output")
    rm -f "$output"
    [ -n "$part1$part2" ] || die "No 'Part 1:' or 'Part 2:' line found in the output"
    save_answers "$day" "$part1" "$part2"
  else
    rm -f "$output"
  fi
}
endef
export AOC_LIB := $(value AOC_LIB_SRC)

# Print once, not again for every recursive $(MAKE) call.
ifeq ($(MAKELEVEL),0)
$(info Using year: $(YEAR))
endif

DAYS := $(sort $(wildcard $(YEAR)/day*))
CURRENT_DAY := $(shell ls -dt $(YEAR)/day* 2>/dev/null | head -n 1)
CURRENT_DAY_NUM := $(patsubst $(YEAR)/day%,%,$(CURRENT_DAY))

# Run tests for all days; exits non-zero if any day fails.
test:
	@echo "Running tests for all days..."
	@failed=""; \
	for day in $(DAYS); do \
		echo "Testing $$day..."; \
		if (cd $$day && dune test); then \
			echo "✅ $$day tests passed!"; \
		else \
			echo "❌ $$day tests failed!"; \
			failed="$$failed $$day"; \
		fi; \
	done; \
	if [ -n "$$failed" ]; then \
		echo "❌ Failed:$$failed"; \
		exit 1; \
	fi; \
	echo "🎉 All tests passed!"

# Run tests for a specific day
test-%:
	@echo "Testing day $*..."
	@cd $(YEAR)/day$* && dune test
	@echo "✅ All tests passed for day $*!"

# Format all code using dune fmt
fmt:
	@echo "Formatting all days..."
	@for day in $(DAYS); do \
		echo "Formatting $$day..."; \
		(cd $$day && dune fmt --auto-promote); \
	done
	@echo "✅ All days formatting completed!"

# Format a specific day
fmt-%:
	@echo "Formatting day $*..."
	@cd $(YEAR)/day$* && dune fmt --auto-promote
	@echo "✅ Day $* formatted successfully!"

# Check formatting without changing files (useful in CI)
fmt-check:
	@failed=""; \
	for day in $(DAYS); do \
		(cd $$day && dune build @fmt >/dev/null 2>&1) || failed="$$failed $$day"; \
	done; \
	if [ -n "$$failed" ]; then \
		echo "❌ Needs formatting:$$failed (run: make fmt)"; \
		exit 1; \
	fi; \
	echo "✅ All days are formatted"

fmt-check-%:
	@cd $(YEAR)/day$* && dune build @fmt
	@echo "✅ Day $* is formatted"

# Benchmark the compiled binary directly with hyperfine. (Benchmarking
# "dune exec" would mostly measure dune's own start-up time.)
benchmark-%:
	@input="inputs/$(YEAR)/day$*.txt"; \
	if [ ! -d "$(YEAR)/day$*" ]; then echo "Day $* directory not found!"; exit 1; fi; \
	if [ ! -s "$$input" ]; then \
		echo "No input file found for day $*. Expected: $$input"; \
		echo "You can download it with: make download DAY=$*"; \
		exit 1; \
	fi; \
	mkdir -p benchmark; \
	echo "Building day$* in release mode..."; \
	(cd $(YEAR)/day$* && dune build --profile release) || exit 1; \
	echo "Running hyperfine benchmark on $$input..."; \
	hyperfine --warmup 3 --runs 10 \
		--export-markdown "benchmark/benchmark_day$*.md" \
		"$(YEAR)/day$*/_build/default/test.exe < $$input" || exit 1; \
	echo "✅ Results exported to benchmark/benchmark_day$*.md"

# Clean all build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@for day in $(DAYS); do \
		echo "Cleaning $$day..."; \
		(cd $$day && dune clean); \
	done
	@echo "✅ All build artifacts cleaned successfully!"

# Create a new day from template
new-day:
	@read -p "Enter day number (e.g., 04): " day; \
	DAY_NUM=$$(echo $$day | sed 's/^0*//'); \
	if [ -d "$(YEAR)/day$$day" ]; then \
		echo "$(YEAR)/day$$day already exists!"; \
		exit 1; \
	fi; \
	echo "Creating $(YEAR)/day$$day..."; \
	mkdir -p "$(YEAR)/day$$day"; \
	cp -r day_template/* "$(YEAR)/day$$day/"; \
	cp day_template/.ocamlformat "$(YEAR)/day$$day/"; \
	\
	# Copy .ocamlformat file if it exists in template or use existing one from another day \
	if [ -f "day_template/.ocamlformat" ]; then \
		cp "day_template/.ocamlformat" "$(YEAR)/day$$day/"; \
	elif [ -f "$(YEAR)/day01/.ocamlformat" ]; then \
		cp "$(YEAR)/day01/.ocamlformat" "$(YEAR)/day$$day/"; \
		echo "Copied .ocamlformat from day01"; \
	else \
		echo "No .ocamlformat found, creating default one"; \
		echo 'profile = default\nversion = 0.29.0\ntype-decl = sparse\nbreak-cases = fit-or-vertical\ndoc-comments = before' > "$(YEAR)/day$$day/.ocamlformat"; \
	fi; \
	\
	# Fetch problem title from AOC website \
	SESSION_TOKEN=$$(grep AUTH_TOKEN .env 2>/dev/null | cut -d'=' -f2 2>/dev/null || echo ""); \
	if [ ! -z "$$SESSION_TOKEN" ]; then \
		echo "Fetching problem title from AOC..."; \
		RESPONSE=$$(curl -s --cookie "session=$$SESSION_TOKEN" \
			-H "User-Agent: github.com/advent-of-code-ocaml" \
			"https://adventofcode.com/$(YEAR)/day/$$(echo $$day | sed 's/^0*//')" 2>/dev/null || echo ""); \
		if [ ! -z "$$RESPONSE" ]; then \
			PROBLEM_TITLE=$$(echo "$$RESPONSE" | grep -o -- "--- Day [0-9][0-9]*: .*---" | sed 's/--- Day [0-9][0-9]*: \(.*\) ---/\1/' | head -n 1 | sed 's/[[:space:]]*$$//'); \
			if [ ! -z "$$PROBLEM_TITLE" ]; then \
				echo "Found problem title: $$PROBLEM_TITLE"; \
				sed -i "s/\[\[DAY\]\]/$$day/g" "$(YEAR)/day$$day/day_template.ml"; \
				sed -i "s/\[Problem Title\]/$$PROBLEM_TITLE/g" "$(YEAR)/day$$day/day_template.ml"; \
				sed -i "s/\[YEAR\]/$(YEAR)/g" "$(YEAR)/day$$day/day_template.ml"; \
			else \
				echo "Could not extract problem title, using placeholder"; \
				sed -i "s/\[\[DAY\]\]/$$day/g" "$(YEAR)/day$$day/day_template.ml"; \
				sed -i "s/\[Problem Title\]/Problem Title/g" "$(YEAR)/day$$day/day_template.ml"; \
				sed -i "s/\[YEAR\]/$(YEAR)/g" "$(YEAR)/day$$day/day_template.ml"; \
			fi; \
		else \
			echo "Could not fetch from AOC, using placeholder"; \
			sed -i "s/\[\[DAY\]\]/$$day/g" "$(YEAR)/day$$day/day_template.ml"; \
			sed -i "s/\[Problem Title\]/Problem Title/g" "$(YEAR)/day$$day/day_template.ml"; \
			sed -i "s/\[YEAR\]/$(YEAR)/g" "$(YEAR)/day$$day/day_template.ml"; \
		fi; \
	else \
		echo "No session token found in .env file, using placeholder title"; \
		sed -i "s/\[\[DAY\]\]/$$day/g" "$(YEAR)/day$$day/day_template.ml"; \
		sed -i "s/\[Problem Title\]/Problem Title/g" "$(YEAR)/day$$day/day_template.ml"; \
		sed -i "s/\[YEAR\]/$(YEAR)/g" "$(YEAR)/day$$day/day_template.ml"; \
	fi; \
	# Ensure URL uses unpadded day number for single-digit days \
	sed -i "s|/day/$$day|/day/$$DAY_NUM|g" "$(YEAR)/day$$day/day_template.ml"; \
	\
	# Update module names in the files \
	sed -i "s/day_template/day$$day/g" "$(YEAR)/day$$day/dune"; \
	sed -i "s/Day_template/Day$$day/g" "$(YEAR)/day$$day/test_template.ml"; \
	sed -i "s/test_template/test/g" "$(YEAR)/day$$day/test_template.ml"; \
	sed -i "s/test_template/test/g" "$(YEAR)/day$$day/dune"; \
	\
	# Rename files to follow the convention \
	mv "$(YEAR)/day$$day/day_template.ml" "$(YEAR)/day$$day/day$$day.ml"; \
	mv "$(YEAR)/day$$day/test_template.ml" "$(YEAR)/day$$day/test.ml"; \
	\
	# Update dune-project if it exists \
	if [ -f "$(YEAR)/day$$day/dune-project" ]; then \
		sed -i "s/day_template/day$$day/g" "$(YEAR)/day$$day/dune-project"; \
	fi; \
	\
	echo "Created $(YEAR)/day$$day successfully!"


# Run a specific day: INPUT=download | puzzle_input | path/to/input.txt
run-day:
	@( eval "$$AOC_LIB"; aoc_run "$(DAY)" "$(INPUT)" dev 1 )

# Same, built with the release profile
run-release:
	@( eval "$$AOC_LIB"; aoc_run "$(DAY)" "$(INPUT)" release 1 )

# Run the most recently modified day (no answer saving)
run-current:
	@( eval "$$AOC_LIB"; aoc_run "$(CURRENT_DAY_NUM)" "$(INPUT)" dev 0 )

# Download puzzle input (FORCE=1 to overwrite a valid file)
download:
	@( eval "$$AOC_LIB"; aoc_download "$(DAY)" )

# Check submission status
check-status:
	@if [ -z "$(DAY)" ]; then \
		echo "Please specify a day with DAY=XX"; \
		exit 1; \
	fi; \
	SESSION_TOKEN=$$( eval "$$AOC_LIB"; session_token ) || exit 1; \
	if [ -z "$$SESSION_TOKEN" ]; then \
		echo "No session token found in .env file"; \
		exit 1; \
	fi; \
	echo "Checking status for day $(DAY)..."; \
	DAY_NUM=$$(echo $(DAY) | sed 's/^0*//'); \
	RESPONSE=$$(curl -s --cookie "session=$$SESSION_TOKEN" \
		"https://adventofcode.com/$(YEAR)/day/$$DAY_NUM" \
		-H "User-Agent: github.com/advent-of-code-ocaml"); \
	if echo "$$RESPONSE" | grep -q "Both parts of this puzzle are complete! They provide two gold stars: \*\*"; then \
		echo "Part 1: Completed ✓"; \
		echo "Part 2: Completed ✓"; \
	elif echo "$$RESPONSE" | grep -q "one gold star: \*\|You have completed Part One"; then \
		echo "Part 1: Completed ✓"; \
		echo "Part 2: Not completed"; \
	else \
		echo "Part 1: Not completed"; \
		echo "Part 2: Not completed"; \
	fi


# Submit an answer
submit:
	@if [ -z "$(DAY)" ]; then \
		echo "Please specify a day with DAY=XX"; \
		exit 1; \
	fi; \
	if [ -z "$(PART)" ]; then \
		echo "Please specify a part with PART=1 or PART=2"; \
		exit 1; \
	fi; \
	\
	# First check if the part is already completed online \
	echo "Checking submission status for day $(DAY)..."; \
	SESSION_TOKEN=$$( eval "$$AOC_LIB"; session_token ) || exit 1; \
	if [ -z "$$SESSION_TOKEN" ]; then \
		echo "No session token found in .env file!"; \
		exit 1; \
	fi; \
	DAY_NUM=$$(echo $(DAY) | sed 's/^0*//'); \
	RESPONSE=$$(curl -s --cookie "session=$$SESSION_TOKEN" \
		-H "User-Agent: github.com/advent-of-code-ocaml" \
		"https://adventofcode.com/$(YEAR)/day/$$DAY_NUM"); \
	\
	# Check if part is already completed \
	if [ "$(PART)" = "1" ]; then \
		if echo "$$RESPONSE" | grep -q "Both parts of this puzzle are complete!\|one gold star: \*\|You have completed Part One"; then \
			echo "Part 1 is already completed! ✓"; \
			if [ "$${FORCE:-}" != "1" ]; then \
				echo "Use FORCE=1 to submit anyway."; \
				exit 0; \
			fi; \
		fi; \
	elif [ "$(PART)" = "2" ]; then \
		if echo "$$RESPONSE" | grep -q "Both parts of this puzzle are complete!"; then \
			echo "Part 2 is already completed! ✓"; \
			if [ "$${FORCE:-}" != "1" ]; then \
				echo "Use FORCE=1 to submit anyway."; \
				exit 0; \
			fi; \
		elif ! echo "$$RESPONSE" | grep -q "one gold star: \*\|You have completed Part One"; then \
			echo "You need to complete Part 1 before submitting Part 2."; \
			exit 1; \
		fi; \
	fi; \
	\
	ANSWER_FILE="answers/$(YEAR)/submit_day$(DAY).txt"; \
	if [ "$${SKIP_RUN:-}" != "1" ]; then \
		echo "Running solution to generate/update answers..."; \
		( eval "$$AOC_LIB"; aoc_run "$(DAY)" download release 1 ) || { \
			echo ""; \
			echo "Please fix your solution before submitting."; \
			echo "You can run: make run-day DAY=$(DAY) INPUT=download"; \
			exit 1; \
		}; \
	fi; \
	if [ ! -f "$$ANSWER_FILE" ]; then \
		echo "No answers file found: $$ANSWER_FILE"; \
		exit 1; \
	fi; \
	\
	ANSWER=$$(grep "^Part$(PART):" "$$ANSWER_FILE" 2>/dev/null | cut -d':' -f2 | sed 's/\[Status:.*\]//g' | tr -d ' ' | head -n1); \
	if [ -z "$$ANSWER" ]; then \
		echo "No answer found for Part $(PART) in $$ANSWER_FILE!"; \
		echo "Current contents of answers file:"; \
		cat "$$ANSWER_FILE" 2>/dev/null || echo "(file is empty or doesn't exist)"; \
		exit 1; \
	fi; \
	\
	# Check if this part is already marked as correct in local file \
	if grep -q "^Part$(PART):.*\[Status: Correct\]" "$$ANSWER_FILE" 2>/dev/null; then \
		echo "Part $(PART) is already marked as correct in the local answers file."; \
		echo "Use FORCE=1 to submit anyway."; \
		if [ "$${FORCE:-}" != "1" ]; then \
			exit 0; \
		fi; \
	fi; \
	\
	echo "Found answer for Day $(DAY) Part $(PART): $$ANSWER"; \
	echo "Submitting answer..."; \
	RESPONSE=$$(curl -s -X POST --cookie "session=$$SESSION_TOKEN" \
		-H "User-Agent: github.com/advent-of-code-ocaml" \
		-d "level=$(PART)&answer=$$ANSWER" \
		"https://adventofcode.com/$(YEAR)/day/$$DAY_NUM/answer"); \
	if echo "$$RESPONSE" | grep -q "That's the right answer!"; then \
		echo "Correct answer! Well done. ✓"; \
		sed -i "s/^Part$(PART): $$ANSWER\(\s*\[Status:.*\]\)\?$$/Part$(PART): $$ANSWER [Status: Correct]/" "$$ANSWER_FILE"; \
	elif echo "$$RESPONSE" | grep -q "You gave an answer too recently"; then \
		if echo "$$RESPONSE" | grep -q "You have \([0-9]*m [0-9]*s\)"; then \
			WAIT_TIME=$$(echo "$$RESPONSE" | grep -o "You have [0-9]*m [0-9]*s" | cut -d' ' -f2-); \
			echo "You need to wait $$WAIT_TIME before submitting again."; \
		else \
			echo "You need to wait before submitting again."; \
		fi; \
	elif echo "$$RESPONSE" | grep -q "That's not the right answer"; then \
		if echo "$$RESPONSE" | grep -q "your answer is too \(high\|low\)"; then \
			DIRECTION=$$(echo "$$RESPONSE" | grep -o "too \(high\|low\)" | cut -d' ' -f2); \
			echo "Incorrect answer. Your answer is too $$DIRECTION."; \
		else \
			echo "Incorrect answer."; \
		fi; \
		sed -i "s/^Part$(PART): $$ANSWER\(\s*\[Status:.*\]\)\?$$/Part$(PART): $$ANSWER [Status: Incorrect]/" "$$ANSWER_FILE"; \
	elif echo "$$RESPONSE" | grep -q "You don't seem to be solving the right level"; then \
		echo "You've already solved this part or are not on this level yet."; \
	else \
		echo "Unexpected response. Please check manually."; \
	fi

# Run with auto-submission option
run-submit:
	@( eval "$$AOC_LIB"; aoc_run "$(DAY)" "$(INPUT)" release 1 ) || exit 1; \
	PART1=$$(grep -s '^Part1:' "answers/$(YEAR)/submit_day$(DAY).txt"); \
	PART2=$$(grep -s '^Part2:' "answers/$(YEAR)/submit_day$(DAY).txt"); \
	\
	# Check submission status \
	STATUS_OUTPUT=$$($(MAKE) -s check-status DAY=$(DAY)); \
	PART1_COMPLETED=$$(echo "$$STATUS_OUTPUT" | grep "Part 1: Completed"); \
	PART2_COMPLETED=$$(echo "$$STATUS_OUTPUT" | grep "Part 2: Completed"); \
	\
	echo "$$STATUS_OUTPUT"; \
	\
	# Prompt to submit answers \
	if [ ! -z "$$PART1" ] && [ -z "$$PART1_COMPLETED" ]; then \
		read -p "Do you want to submit Part 1 answer? (y/n) " SUBMIT_PART1; \
		if [ "$$SUBMIT_PART1" = "y" ]; then \
			$(MAKE) submit DAY=$(DAY) PART=1 SKIP_RUN=1; \
			\
			# If Part 1 was successfully submitted and Part 2 is available, check status again and try Part 2 \
			if [ $$? -eq 0 ] && [ ! -z "$$PART2" ]; then \
				echo "Waiting 45 seconds before checking status again..."; \
				sleep 45; \
				\
				# Refresh status after Part 1 submission \
				STATUS_OUTPUT=$$($(MAKE) -s check-status DAY=$(DAY)); \
				PART2_COMPLETED=$$(echo "$$STATUS_OUTPUT" | grep "Part 2: Completed"); \
				\
				if [ -z "$$PART2_COMPLETED" ]; then \
					read -p "Do you want to submit Part 2 answer? (y/n) " SUBMIT_PART2; \
					if [ "$$SUBMIT_PART2" = "y" ]; then \
						$(MAKE) submit DAY=$(DAY) PART=2 SKIP_RUN=1; \
					fi; \
				fi; \
			fi; \
		fi; \
	elif [ ! -z "$$PART2" ] && [ -z "$$PART2_COMPLETED" ] && [ ! -z "$$PART1_COMPLETED" ]; then \
		read -p "Do you want to submit Part 2 answer? (y/n) " SUBMIT_PART2; \
		if [ "$$SUBMIT_PART2" = "y" ]; then \
			$(MAKE) submit DAY=$(DAY) PART=2 SKIP_RUN=1; \
		fi; \
	fi

# Add flake targets

flake-build:
	nix build .#day$(DAY)-$(YEAR)

flake-run:
	@if [ -z "$(DAY)" ]; then \
		echo "Please specify a day number using DAY=XX"; \
		exit 1; \
	fi; \
	INPUT_FILE="inputs/$(YEAR)/day$(DAY).txt"; \
	if [ ! -f "$$INPUT_FILE" ]; then \
		echo "Input file not found: $$INPUT_FILE"; \
		echo "You can download it with: make download DAY=$(DAY)"; \
		exit 1; \
	fi; \
	echo "Running day$(DAY)-$(YEAR) with input $$INPUT_FILE..."; \
	cat "$$INPUT_FILE" | nix run .#day$(DAY)-$(YEAR)

flake:
	nix develop

flake-update:
	nix flake update

# Show help
help:
	@echo "Advent of Code OCaml - Makefile Help"
	@echo ""
	@echo "Available targets:"
	@echo "  help            : Show this help message (default)"
	@echo "  test            : Run tests for all days (fails if any day fails)"
	@echo "  test-XX         : Run tests for a specific day (e.g., test-01)"
	@echo "  fmt             : Format all code"
	@echo "  fmt-XX          : Format code for a specific day (e.g., fmt-01)"
	@echo "  fmt-check       : Check formatting for all code without changing it"
	@echo "  fmt-check-XX    : Check formatting for a specific day (e.g., fmt-check-01)"
	@echo "  benchmark-XX    : Benchmark the release binary for a day (e.g., benchmark-09)"
	@echo "  clean           : Clean all build artifacts"
	@echo "  new-day         : Create a new day from template (interactive)"
	@echo "  run-day         : Run a specific day with input and save answers"
	@echo "  run-release     : Build and run a specific day in release mode and save answers"
	@echo "  run-current     : Run the most recently modified day with input (no answer saving)"
	@echo "  submit          : Submit an answer (DAY=XX PART=1 or 2)"
	@echo "  run-submit      : Run a day in release mode and prompt to submit (DAY=XX INPUT=...)"
	@echo ""
	@echo "  make download DAY=XX [FORCE=1]            : Download puzzle input for day XX"
	@echo "  make check-status DAY=XX                  : Check submission status for day XX"
	@echo "  make submit DAY=XX PART=P                 : Submit answer for day XX part P (1 or 2)"
	@echo "  make run-submit DAY=XX INPUT=path         : Run day XX on <path> and prompt to submit answers"
	@echo "  make run-submit DAY=XX INPUT=download     : Download input, run day XX, and prompt to submit"
	@echo ""
	@echo "INPUT can be a path, 'download', or 'puzzle_input' (looks in inputs/YEAR/dayXX.txt,"
	@echo "inputs/YEAR/input.txt, then \$$AOC_DOWNLOADS/input.txt or your Downloads folder)."
	@echo "Empty or invalid input files are rejected before the solution runs."
	@echo "Override the year with YEAR=2024."
	@echo ""
	@echo "Flake Commands (Nix 2.4+ with flakes enabled):"
	@echo "  make flake                                    : Enter flake development shell"
	@echo "  make flake-build DAY=XX                       : Build specific day with flakes (uses current year)"
	@echo "  make flake-run DAY=XX                         : Run specific day with input from inputs/YEAR/dayXX.txt"
	@echo "  make flake-update                             : Update flake dependencies"
	@echo "  nix flake show                                : Show all available packages"
	@echo "  nix build .#day01-2024                        : Build day01 of 2024"
	@echo "  nix build .#all-2024                          : Build all 2024 solutions"
	@echo "  cat input.txt | nix run .#day01-2025          : Run day01 of 2025 with piped input"
	@echo ""
	@echo "Examples:"
	@echo "  make test-03                                           # Run tests for day03"
	@echo "  make run-day DAY=02 INPUT=download                     # Run day02 with downloaded input"
	@echo "  make run-current INPUT=download                        # Download input and run the most recently modified day"
	@echo "  make run-release DAY=01 INPUT=puzzle_input             # Build and run day01 in release mode and save the answers"
	@echo "  make run-submit DAY=01 INPUT=download                  # Download input, run day01 in release mode, and prompt to submit"
	@echo "  make download DAY=10 FORCE=1                           # Re-download day10's input"
	@echo "  make flake-build DAY=01                                # Build day01 with Nix flake"
	@echo "  make flake-run DAY=02                                  # Run day02 with Nix flake"
	@echo ""

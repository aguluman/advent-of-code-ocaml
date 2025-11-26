.PHONY: all test clean new-day run-day help benchmark-% fmt fmt-% run-release run-current download check-status submit run-submit

# Default target
all: help

# Variables
YEAR := $(shell if ls -d [0-9][0-9][0-9][0-9] >/dev/null 2>&1; then \
	ls -d [0-9][0-9][0-9][0-9] | sort -r | head -n 1; \
else \
	echo ""; \
fi)

ifeq ($(YEAR),)
$(error No year directories found. Please create a directory like 2024 first.)
endif

$(info Using year: $(YEAR))

# Rest of variables
DAYS := $(wildcard $(YEAR)/day*)
CURRENT_DAY := $(shell ls -dt $(YEAR)/day* 2>/dev/null | head -n 1)

# Run tests for all days
test:
	@echo "Running tests for all days..."
	@for day in $(DAYS); do \
		echo "Testing $$day..."; \
		if [ -d "$$day" ]; then \
			(cd $$day && dune test && echo "✅ $$day tests passed!" || echo "❌ $$day tests failed!"); \
		else \
			echo "Warning: Directory $$day does not exist, skipping..."; \
		fi; \
	done
	@echo "🎉 All tests completed!"

# Run tests for a specific day
test-%:
	@echo "Testing day $*..."
	@cd $(YEAR)/day$* && dune test && echo "✅ All tests passed for day $*!" || echo "❌ Tests failed for day $*!"

# Format all code using dune fmt
fmt:
	@echo "Formatting all days..."
	@for day in $(DAYS); do \
		echo "Formatting $$day..."; \
		if [ -d "$$day" ]; then \
			(cd $$day && dune fmt --auto-promote); \
		else \
			echo "Warning: Directory $$day does not exist, skipping..."; \
		fi; \
	done
	@echo "✅ All days formatting completed!"

# Format a specific day
fmt-%:
	@echo "Formatting day $*..."
	@cd $(YEAR)/day$* && dune fmt
	@echo "✅ Day $* formatted successfully!" 

# Run benchmark for a specific day using hyperfine
benchmark-%:
	@echo "Running benchmark for day $*..."
	@mkdir -p benchmark
	@if [ ! -d "$(YEAR)/day$*" ]; then \
		echo "Day $* directory not found!"; \
		exit 1; \
	fi; \
	if [ -f "inputs/$(YEAR)/day$*.txt" ]; then \
		INPUT_FILE="inputs/$(YEAR)/day$*.txt"; \
		echo "Using input file: $$INPUT_FILE"; \
		cd $(YEAR)/day$* && \
		echo "Building in release mode..."; \
		dune build --profile release && \
		echo "Running hyperfine benchmark..."; \
		hyperfine --warmup 3 --runs 10 \
				--export-markdown "../../benchmark/benchmark_day$*.md" \
			"cat ../../$$INPUT_FILE | dune exec --profile release ./test.exe" && \
		cd ../../; \
		echo "✅ Benchmark completed for day $*!"; \
		echo "Results exported to benchmark/benchmark_day$*.md"; \
	else \
		echo "No input file found for day $*. Expected: inputs/$(YEAR)/day$*.txt"; \
		echo "You can download it with: make download DAY=$*"; \
		exit 1; \
	fi

# Clean all build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@for day in $(DAYS); do \
		echo "Cleaning $$day..."; \
		if [ -d "$$day" ]; then \
			(cd $$day && dune clean); \
		else \
			echo "Warning: Directory $$day does not exist, skipping..."; \
		fi; \
	done
	@echo "✅ All build artifacts cleaned successfully!"

# Create a new day from template
new-day:
	@read -p "Enter day number (e.g., 04): " day; \
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
		echo 'profile = default\nversion = 0.28.1\ntype-decl = sparse\nbreak-cases = fit-or-vertical\ndoc-comments = before' > "$(YEAR)/day$$day/.ocamlformat"; \
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

# Run a specific day with input file
run-day:
	@if [ -z "$(DAY)" ]; then \
		echo "Please specify a day number using DAY=XX"; \
		exit 1; \
	fi; \
	if [ -z "$(INPUT)" ]; then \
		echo "Please specify an input file using INPUT=path/to/input.txt"; \
		exit 1; \
	fi; \
	if [ "$(INPUT)" = "puzzle_input" ]; then \
		if [ ! -d "inputs/$(YEAR)" ]; then \
			echo "Notice: Repository inputs directory not found."; \
			echo "Creating directory: inputs/$(YEAR)"; \
			mkdir -p inputs/$(YEAR); \
			echo "Created inputs/$(YEAR) - You can now place your puzzle inputs there."; \
		fi; \
		if [ -f "inputs/$(YEAR)/day$(DAY).txt" ]; then \
			INPUT_PATH="inputs/$(YEAR)/day$(DAY).txt"; \
			echo "Using day-specific input file: $$INPUT_PATH"; \
		elif [ -f "inputs/$(YEAR)/input.txt" ]; then \
			INPUT_PATH="inputs/$(YEAR)/input.txt"; \
			echo "Using generic input file: $$INPUT_PATH"; \
		elif [ -d "/mnt/c" ]; then \
			INPUT_PATH="/mnt/c/Users/chukw/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		elif [ "$(shell uname -s)" = "Linux" ]; then \
			INPUT_PATH="$(HOME)/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		else \
			INPUT_PATH="C:/Users/chukw/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		fi; \
	elif [ "$(INPUT)" = "download" ]; then \
		echo "Downloading input for day $(DAY)..."; \
		$(MAKE) download DAY=$(DAY); \
		INPUT_PATH="inputs/$(YEAR)/day$(DAY).txt"; \
	else \
		INPUT_PATH="$(INPUT)"; \
	fi; \
	if [ ! -f "$$INPUT_PATH" ]; then \
		echo "Input file not found: $$INPUT_PATH"; \
		exit 1; \
	fi; \
	echo "Running day$(DAY) with input $$INPUT_PATH..."; \
	\
	# Create answers directory \
	mkdir -p answers/$(YEAR); \
	ANSWER_FILE="answers/$(YEAR)/submit_day$(DAY).txt"; \
	\
	# Run the solution and capture output \
	if OUTPUT=$$(cd $(YEAR)/day$(DAY) && case "$$INPUT_PATH" in /*) cat $$INPUT_PATH | dune exec ./test.exe;; *) cat $(PWD)/$$INPUT_PATH | dune exec ./test.exe;; esac 2>&1); then \
		echo "$$OUTPUT"; \
		\
		# Extract the answers \
		PART1=$$(echo "$$OUTPUT" | grep "Part 1:" | cut -d':' -f2 | tr -d ' '); \
		PART2=$$(echo "$$OUTPUT" | grep "Part 2:" | cut -d':' -f2 | tr -d ' '); \
		\
		# Save answers if found \
		if [ ! -z "$$PART1" ] || [ ! -z "$$PART2" ]; then \
			if [ ! -z "$$PART1" ]; then \
				echo "Part1: $$PART1" > "$$ANSWER_FILE"; \
			fi; \
			if [ ! -z "$$PART2" ]; then \
				echo "Part2: $$PART2" >> "$$ANSWER_FILE"; \
			fi; \
			echo "Answers saved to $$ANSWER_FILE"; \
		fi; \
	else \
		echo "$$OUTPUT"; \
		echo "Solution failed to run properly"; \
	fi

# Run a specific day with input file in release mode
run-release:
	@if [ -z "$(DAY)" ]; then \
		echo "Please specify a day with DAY=XX"; \
		exit 1; \
	fi; \
	if [ -z "$(INPUT)" ]; then \
		echo "Please specify an input file with INPUT=path/to/input.txt"; \
		exit 1; \
	fi; \
	if [ "$(INPUT)" = "download" ]; then \
		echo "Downloading input for day $(DAY)..."; \
		$(MAKE) download DAY=$(DAY); \
		INPUT_PATH="$(PWD)/inputs/$(YEAR)/day$(DAY).txt"; \
		echo "Using downloaded input file: $$INPUT_PATH"; \
	elif [ "$(INPUT)" = "puzzle_input" ]; then \
		if [ ! -d "inputs/$(YEAR)" ]; then \
			echo "Notice: Repository inputs directory not found."; \
			echo "Creating directory: inputs/$(YEAR)"; \
			mkdir -p inputs/$(YEAR); \
			echo "Created inputs/$(YEAR) - You can now place your puzzle inputs there."; \
			echo "- Day-specific files: inputs/$(YEAR)/day01.txt, etc."; \
			echo "- Generic input file: inputs/$(YEAR)/input.txt"; \
		fi; \
		if [ -f "inputs/$(YEAR)/day$(DAY).txt" ]; then \
			INPUT_PATH="inputs/$(YEAR)/day$(DAY).txt"; \
			echo "Using day-specific input file: $$INPUT_PATH"; \
		elif [ -f "inputs/$(YEAR)/input.txt" ]; then \
			INPUT_PATH="inputs/$(YEAR)/input.txt"; \
			echo "Using generic input file: $$INPUT_PATH"; \
		elif [ -d "/mnt/c" ]; then \
			INPUT_PATH="/mnt/c/Users/chukw/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		elif [ "$(shell uname -s)" = "Linux" ]; then \
			INPUT_PATH="$(HOME)/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		else \
			INPUT_PATH="C:/Users/chukw/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		fi; \
	else \
		INPUT_PATH="$(INPUT)"; \
	fi; \
	if [ ! -f "$$INPUT_PATH" ]; then \
		echo "Input file not found: $$INPUT_PATH"; \
		exit 1; \
	fi; \
	echo "Building and running day$(DAY) in release mode with input $$INPUT_PATH..."; \
	cd $(YEAR)/day$(DAY) && dune build --profile release && cd ../../; \
	\
	# Create answers directory \
	mkdir -p answers/$(YEAR); \
	ANSWER_FILE="answers/$(YEAR)/submit_day$(DAY).txt"; \
	\
	# Run the solution and capture output \
	if OUTPUT=$$(cd $(YEAR)/day$(DAY) && case "$$INPUT_PATH" in /*) cat "$$INPUT_PATH" | dune exec --profile release ./test.exe;; *) cat "$(PWD)/$$INPUT_PATH" | dune exec --profile release ./test.exe;; esac 2>&1); then \
		echo "$$OUTPUT"; \
		\
		# Extract the answers \
		PART1=$$(echo "$$OUTPUT" | grep "Part 1:" | cut -d':' -f2 | tr -d ' '); \
		PART2=$$(echo "$$OUTPUT" | grep "Part 2:" | cut -d':' -f2 | tr -d ' '); \
		\
		# Save answers if found \
		if [ ! -z "$$PART1" ] || [ ! -z "$$PART2" ]; then \
			if [ ! -z "$$PART1" ]; then \
				echo "Part1: $$PART1" > "$$ANSWER_FILE"; \
			fi; \
			if [ ! -z "$$PART2" ]; then \
				echo "Part2: $$PART2" >> "$$ANSWER_FILE"; \
			fi; \
			echo "Answers saved to $$ANSWER_FILE"; \
		fi; \
	else \
		echo "$$OUTPUT"; \
		echo "Solution failed to run properly"; \
	fi

# Run the current day (most recent) with input file
run-current:
	@if [ -z "$(INPUT)" ]; then \
		echo "Please specify an input file with INPUT=path/to/input.txt"; \
		exit 1; \
	fi; \
	DAY_NUM=$$(echo $(CURRENT_DAY) | sed 's/.*day//'); \
	if [ "$(INPUT)" = "download" ]; then \
		echo "Downloading input for current day $$DAY_NUM..."; \
		$(MAKE) download DAY=$$DAY_NUM; \
		INPUT_PATH="$(PWD)/inputs/$(YEAR)/day$$DAY_NUM.txt"; \
		echo "Using downloaded input file: $$INPUT_PATH"; \
	elif [ "$(INPUT)" = "puzzle_input" ]; then \
		if [ ! -d "inputs/$(YEAR)" ]; then \
			echo "Notice: Repository inputs directory not found."; \
			echo "Creating directory: inputs/$(YEAR)"; \
			mkdir -p inputs/$(YEAR); \
			echo "Created inputs/$(YEAR) - You can now place your puzzle inputs there."; \
		fi; \
		if [ -f "inputs/$(YEAR)/day$$DAY_NUM.txt" ]; then \
			INPUT_PATH="$(PWD)/inputs/$(YEAR)/day$$DAY_NUM.txt"; \
			echo "Using day-specific input file: $$INPUT_PATH"; \
		elif [ -f "inputs/$(YEAR)/input.txt" ]; then \
			INPUT_PATH="$(PWD)/inputs/$(YEAR)/input.txt"; \
			echo "Using generic input file: $$INPUT_PATH"; \
		elif [ -d "/mnt/c" ]; then \
			INPUT_PATH="/mnt/c/Users/chukw/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		elif [ "$(shell uname -s)" = "Linux" ]; then \
			INPUT_PATH="$(HOME)/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		else \
			INPUT_PATH="C:/Users/chukw/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT_PATH"; \
		fi; \
	else \
		INPUT_PATH="$(INPUT)"; \
	fi; \
	echo "Running $(CURRENT_DAY) with input $$INPUT_PATH..."; \
	if [ -f "$$INPUT_PATH" ]; then \
		cd $(CURRENT_DAY) && cat "$$INPUT_PATH" | dune exec ./test.exe; \
	else \
		echo "Input file not found: $$INPUT_PATH"; \
		exit 1; \
	fi

# Download puzzle input
download:
	@if [ -z "$(DAY)" ]; then \
		echo "Please specify a day number using DAY=XX"; \
		exit 1; \
	fi; \
	echo "Downloading input for day $(DAY)..."; \
	mkdir -p inputs/$(YEAR); \
	if [ -f "inputs/$(YEAR)/day$(DAY).txt" ] && [ "$${FORCE:-}" != "1" ]; then \
		echo "Input file already exists. Use FORCE=1 to overwrite."; \
		exit 0; \
	fi; \
	SESSION_TOKEN=$$(grep AUTH_TOKEN .env | cut -d'=' -f2); \
	if [ -z "$$SESSION_TOKEN" ]; then \
		echo "No session token found in .env file. Please add AUTH_TOKEN=your_token"; \
		exit 1; \
	fi; \
	DAY_NUM=$$(echo $(DAY) | sed 's/^0*//'); \
	echo "Downloading from https://adventofcode.com/$(YEAR)/day/$$DAY_NUM/input"; \
	curl -s --cookie "session=$$SESSION_TOKEN" \
		"https://adventofcode.com/$(YEAR)/day/$$DAY_NUM/input" \
		-o "inputs/$(YEAR)/day$(DAY).txt"; \
	if [ $$? -eq 0 ]; then \
		echo "Successfully downloaded input to inputs/$(YEAR)/day$(DAY).txt"; \
	else \
		echo "Failed to download input"; \
		exit 1; \
	fi

# Check submission status
check-status:
	@if [ -z "$(DAY)" ]; then \
		echo "Please specify a day with DAY=XX"; \
		exit 1; \
	fi; \
	SESSION_TOKEN=$$(grep AUTH_TOKEN .env | cut -d'=' -f2); \
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

# Submit an answer (improved)
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
	SESSION_TOKEN=$$(grep AUTH_TOKEN .env 2>/dev/null | cut -d'=' -f2); \
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
	if [ ! -f "$$ANSWER_FILE" ]; then \
		echo "No answers file found at $$ANSWER_FILE"; \
		echo "Running solution to generate answers..."; \
		if [ ! -f "inputs/$(YEAR)/day$(DAY).txt" ]; then \
			echo "Input file not found. Downloading..."; \
			$(MAKE) download DAY=$(DAY); \
		fi; \
		INPUT_PATH="$(PWD)/inputs/$(YEAR)/day$(DAY).txt"; \
		echo "Building day$(DAY) in release mode..."; \
		cd $(YEAR)/day$(DAY) && dune build --profile release && cd ../../; \
		echo "Running day$(DAY) with input $$INPUT_PATH..."; \
		if OUTPUT=$$(cd $(YEAR)/day$(DAY) && cat "$$INPUT_PATH" | dune exec --profile release ./test.exe 2>&1); then \
			echo "$$OUTPUT"; \
			PART1=$$(echo "$$OUTPUT" | grep "Part 1:" | cut -d':' -f2 | tr -d ' '); \
			PART2=$$(echo "$$OUTPUT" | grep "Part 2:" | cut -d':' -f2 | tr -d ' '); \
			mkdir -p answers/$(YEAR); \
			if [ ! -z "$$PART1" ]; then \
				echo "Part1: $$PART1" > "$$ANSWER_FILE"; \
			fi; \
			if [ ! -z "$$PART2" ]; then \
				echo "Part2: $$PART2" >> "$$ANSWER_FILE"; \
			fi; \
			if [ -f "$$ANSWER_FILE" ]; then \
				echo "Answers saved to $$ANSWER_FILE"; \
			else \
				echo "Failed to save answers - no valid output found"; \
				exit 1; \
			fi; \
		else \
			echo "Solution failed to run:"; \
			echo "$$OUTPUT"; \
			echo ""; \
			echo "Please fix your solution before submitting."; \
			echo "You can run: make run-day DAY=$(DAY) INPUT=download"; \
			exit 1; \
		fi; \
	fi; \
	\
	if [ ! -f "$$ANSWER_FILE" ]; then \
		echo "Answers file still doesn't exist after running solution!"; \
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
	@if [ -z "$(DAY)" ]; then \
		echo "Please specify a day number using DAY=XX"; \
		exit 1; \
	fi; \
	INPUT="$(INPUT)"; \
	if [ -z "$$INPUT" ]; then \
		echo "Please specify an input file using INPUT=path/to/input.txt, INPUT=download, or INPUT=puzzle_input"; \
		exit 1; \
	fi; \
	\
	# If input is "download", get the input from AoC \
	if [ "$$INPUT" = "download" ]; then \
		$(MAKE) download DAY=$(DAY); \
		INPUT="$(PWD)/inputs/$(YEAR)/day$(DAY).txt"; \
	fi; \
	\
	# If INPUT is "puzzle_input", resolve it like run-release does \
	if [ "$$INPUT" = "puzzle_input" ]; then \
		if [ ! -d "inputs/$(YEAR)" ]; then \
			echo "Notice: Repository inputs directory not found."; \
			echo "Creating directory: inputs/$(YEAR)"; \
			mkdir -p inputs/$(YEAR); \
			echo "Created inputs/$(YEAR) - You can now place your puzzle inputs there."; \
		fi; \
		if [ -f "inputs/$(YEAR)/day$(DAY).txt" ]; then \
			INPUT="$(PWD)/inputs/$(YEAR)/day$(DAY).txt"; \
			echo "Using day-specific input file: $$INPUT"; \
		elif [ -f "inputs/$(YEAR)/input.txt" ]; then \
			INPUT="$(PWD)/inputs/$(YEAR)/input.txt"; \
			echo "Using generic input file: $$INPUT"; \
		elif [ -d "/mnt/c" ]; then \
			INPUT="/mnt/c/Users/chukw/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT"; \
		elif [ "$(shell uname -s)" = "Linux" ]; then \
			INPUT="$(HOME)/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT"; \
		else \
			INPUT="C:/Users/chukw/Downloads/input.txt"; \
			echo "Using default download path: $$INPUT"; \
		fi; \
	fi; \
	\
	# Convert INPUT to absolute path if it's relative \
	case "$$INPUT" in \
		/*) ;; \
		*) INPUT="$(PWD)/$$INPUT" ;; \
	esac; \
	\
	# Check if input file exists \
	if [ ! -f "$$INPUT" ]; then \
		echo "Input file not found: $$INPUT"; \
		exit 1; \
	fi; \
	\
	# Check if day directory exists \
	if [ ! -d "$(YEAR)/day$(DAY)" ]; then \
		echo "Day $(DAY) does not exist!"; \
		exit 1; \
	fi; \
	\
	# Create answers directory \
	mkdir -p answers/$(YEAR); \
	ANSWER_FILE="answers/$(YEAR)/submit_day$(DAY).txt"; \
	\
	# Build the release binary \
	echo "Building day$(DAY) in release mode..."; \
	cd $(YEAR)/day$(DAY) && dune build --profile release && cd ../../; \
	\
	# Run the solution and capture output \
	echo "Running day$(DAY) with input $$INPUT..."; \
	OUTPUT=$$(cd $(YEAR)/day$(DAY) && cat "$$INPUT" | dune exec --profile release ./test.exe); \
	echo "$$OUTPUT"; \
	\
	# Extract the answers \
	PART1=$$(echo "$$OUTPUT" | grep "Part 1:" | cut -d':' -f2 | tr -d ' '); \
	PART2=$$(echo "$$OUTPUT" | grep "Part 2:" | cut -d':' -f2 | tr -d ' '); \
	\
	# Save answers \
	if [ ! -z "$$PART1" ]; then \
		echo "Part1: $$PART1" > "$$ANSWER_FILE"; \
	fi; \
	if [ ! -z "$$PART2" ]; then \
		echo "Part2: $$PART2" >> "$$ANSWER_FILE"; \
	fi; \
	\
	if [ -z "$$PART1" ] && [ -z "$$PART2" ]; then \
		echo "Could not extract answers from output"; \
		exit 1; \
	fi; \
	\
	echo "Answers saved to $$ANSWER_FILE"; \
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
			$(MAKE) submit DAY=$(DAY) PART=1; \
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
						$(MAKE) submit DAY=$(DAY) PART=2; \
					fi; \
				fi; \
			fi; \
		fi; \
	elif [ ! -z "$$PART2" ] && [ -z "$$PART2_COMPLETED" ] && [ ! -z "$$PART1_COMPLETED" ]; then \
		read -p "Do you want to submit Part 2 answer? (y/n) " SUBMIT_PART2; \
		if [ "$$SUBMIT_PART2" = "y" ]; then \
			$(MAKE) submit DAY=$(DAY) PART=2; \
		fi; \
	fi

# Add flake targets
.PHONY: flake-build flake-run flake-dev

flake-build:
	nix build .#day$(DAY)-$(YEAR)

flake-run:
	nix run .#day$(DAY)-$(YEAR)

flake-dev:
	nix develop

flake-update:
	nix flake update

# Show help
help:
	@echo "Advent of Code OCaml - Makefile Help"
	@echo ""
	@echo "Available targets:"
	@echo "  all             : Run tests and linting (default)"
	@echo "  test            : Run tests for all days"
	@echo "  test-XX         : Run tests for a specific day (e.g., test-01)"
	@echo "  fmt             : Format all code"
	@echo "  fmt-XX          : Format code for a specific day (e.g., fmt-01)"
	@echo "  fmt-check       : Check formatting for all code"
	@echo "  fmt-check-XX    : Check formatting for a specific day (e.g., fmt-check-01)"
	@echo "  benchmark-XX    : Run benchmark for a specific day (e.g., benchmark-09)"
	@echo "  clean           : Clean all build artifacts"
	@echo "  new-day         : Create a new day from template (interactive)"
	@echo "  run-day         : Run a specific day with input and save answers"
	@echo "  run-release     : Build and run a specific day in release mode and save answers"
	@echo "  run-current     : Run the most recently modified day with input (workflow-aware, no answer saving)"
	@echo ""
	@echo "  make download DAY=XX                      : Download puzzle input for day XX"
	@echo "  make check-status DAY=XX                  : Check submission status for day XX"
	@echo "  make submit DAY=XX PART=P                 : Submit answer for day XX part P (1 or 2)"
	@echo "  make run-submit DAY=XX INPUT=path         : Run day XX and use the <input> path stated and prompt to submit answers"
	@echo "  make run-submit DAY=XX INPUT=download     : Download input, run day XX, and prompt to submit"
	@echo ""
	@echo "Flake Commands (Nix 24.XX+):"
	@echo "  make flake-dev                                : Enter flake development shell"
	@echo "  make flake-build DAY=XX YEAR=YYYY             : Build specific day with flakes"
	@echo "  make flake-run DAY=XX YEAR=YYYY               : Run specific day with flakes"
	@echo "  nix flake show                                : Show all available packages"
	@echo "  nix build .#all-2024                          : Build all 2024 solutions"
	@echo ""
	@echo "  make help                                     : Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make build-01                                          # Build day01"
	@echo "  make test-03                                           # Run tests for day03"
	@echo "  make run-day DAY=02 INPUT=download                     # Run day02 with specified input"
	@echo "  make run-current INPUT=download                        # Download input and run the most recently modified day"
	@echo "  make run-release DAY=01 INPUT=puzzle_input             # Build and run day01 in release mode with default  and save the answers"
	@echo "  make run-submit DAY=01 INPUT=download                  # Download input, run day01 in release mode, and prompt to submit"
	@echo ""

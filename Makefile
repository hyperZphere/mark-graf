# mark-graf Makefile

EMACS ?= emacs
BATCH = $(EMACS) -Q -batch -L lisp -L test

EL_FILES = $(wildcard lisp/*.el)
ELC_FILES = $(EL_FILES:.el=.elc)
TEST_FILES = $(wildcard test/*-test.el)

.PHONY: all compile test test-export test-cmd test-regex test-integration clean lint package help

# Default target
all: compile

#─────────────────────────────────────────────────────────────────────────────
# Compilation
#─────────────────────────────────────────────────────────────────────────────

compile: $(ELC_FILES)

lisp/%.elc: lisp/%.el
	@echo "Compiling $<..."
	@$(BATCH) -f batch-byte-compile $<

#─────────────────────────────────────────────────────────────────────────────
# Testing
#─────────────────────────────────────────────────────────────────────────────

# Run all tests
test:
	@echo "Running all mark-graf tests..."
	@$(BATCH) \
		-l mark-graf \
		-l mark-graf-export-test \
		-l mark-graf-commands-test \
		-l mark-graf-regex-test \
		-l mark-graf-integration-test \
		-f ert-run-tests-batch-and-exit

# Run only export tests
test-export:
	@echo "Running export tests..."
	@$(BATCH) \
		-l mark-graf \
		-l mark-graf-export-test \
		--eval "(ert-run-tests-batch-and-exit \"^export/\")"

# Run only command tests
test-cmd:
	@echo "Running command tests..."
	@$(BATCH) \
		-l mark-graf \
		-l mark-graf-commands-test \
		--eval "(ert-run-tests-batch-and-exit \"^cmd/\")"

# Run only regex tests
test-regex:
	@echo "Running regex tests..."
	@$(BATCH) \
		-l mark-graf \
		-l mark-graf-regex-test \
		--eval "(ert-run-tests-batch-and-exit \"^regex/\")"

# Run only integration tests
test-integration:
	@echo "Running integration tests..."
	@$(BATCH) \
		-l mark-graf \
		-l mark-graf-integration-test \
		--eval "(ert-run-tests-batch-and-exit \"^integration/\")"

# Run tests with verbose output
test-verbose:
	@echo "Running all tests (verbose)..."
	@$(BATCH) \
		-l mark-graf \
		-l mark-graf-export-test \
		-l mark-graf-commands-test \
		-l mark-graf-regex-test \
		-l mark-graf-integration-test \
		--eval "(ert-run-tests-batch \"^\\\\(export\\\\|cmd\\\\|regex\\\\|integration\\\\)/\")"

# Count tests
test-count:
	@echo "Counting tests..."
	@$(BATCH) \
		-l mark-graf \
		-l mark-graf-export-test \
		-l mark-graf-commands-test \
		-l mark-graf-regex-test \
		-l mark-graf-integration-test \
		--eval "(let ((count 0)) \
			(mapatoms (lambda (s) (when (and (ert-test-boundp s) \
				(string-match-p \"^\\\\(export\\\\|cmd\\\\|regex\\\\|integration\\\\)/\" (symbol-name s))) \
				(setq count (1+ count))))) \
			(message \"Total test count: %d\" count))"

#─────────────────────────────────────────────────────────────────────────────
# Quality Checks
#─────────────────────────────────────────────────────────────────────────────

# Check for compilation warnings
check-syntax:
	@echo "Checking for compilation warnings..."
	@$(BATCH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(EL_FILES)

# Run checkdoc
checkdoc:
	@echo "Running checkdoc..."
	@for f in $(EL_FILES); do \
		echo "Checking $$f..."; \
		$(BATCH) --eval "(checkdoc-file \"$$f\")" || true; \
	done

# Run package-lint (if available)
lint:
	@echo "Running package-lint..."
	@$(BATCH) \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) \
			(package-refresh-contents) \
			(package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit $(EL_FILES) || echo "(package-lint not available)"

#─────────────────────────────────────────────────────────────────────────────
# Packaging
#─────────────────────────────────────────────────────────────────────────────

# Build distributable package
package:
	@echo "Building package..."
	@mkdir -p dist
	@tar -cvf dist/mark-graf.tar \
		--transform 's,^lisp/,mark-graf-1.0.0/,' \
		--transform 's,^README,mark-graf-1.0.0/README,' \
		lisp/*.el README.md
	@echo "Package created: dist/mark-graf.tar"

#─────────────────────────────────────────────────────────────────────────────
# Cleanup
#─────────────────────────────────────────────────────────────────────────────

clean:
	@echo "Cleaning..."
	@rm -f $(ELC_FILES)
	@rm -f test/*.elc
	@rm -rf dist

#─────────────────────────────────────────────────────────────────────────────
# CI Support
#─────────────────────────────────────────────────────────────────────────────

# CI pipeline: compile, lint, test
ci: clean compile test
	@echo "CI checks passed!"

#─────────────────────────────────────────────────────────────────────────────
# Help
#─────────────────────────────────────────────────────────────────────────────

help:
	@echo "mark-graf Makefile"
	@echo ""
	@echo "Build targets:"
	@echo "  all              - Compile all files (default)"
	@echo "  compile          - Byte-compile elisp files"
	@echo "  clean            - Remove compiled files"
	@echo ""
	@echo "Test targets:"
	@echo "  test             - Run all tests"
	@echo "  test-export      - Run export tests only"
	@echo "  test-cmd         - Run command tests only"
	@echo "  test-regex       - Run regex tests only"
	@echo "  test-integration - Run integration tests only"
	@echo "  test-verbose     - Run tests with verbose output"
	@echo "  test-count       - Count total tests"
	@echo ""
	@echo "Quality targets:"
	@echo "  check-syntax     - Check for compilation warnings"
	@echo "  checkdoc         - Run checkdoc on all files"
	@echo "  lint             - Run package-lint"
	@echo ""
	@echo "Other targets:"
	@echo "  package          - Build distributable package"
	@echo "  ci               - Run full CI pipeline"
	@echo "  help             - Show this help"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS            - Emacs executable (default: emacs)"

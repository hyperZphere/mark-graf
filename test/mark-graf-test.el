;;; mark-graf-test.el --- Test runner for mark-graf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;;; Commentary:

;; Main test runner for mark-graf.
;; Loads all test modules and provides test execution functions.
;;
;; Test organization:
;;   - mark-graf-export-test.el    : Export/HTML conversion tests (~45 tests)
;;   - mark-graf-commands-test.el  : Interactive command tests (~35 tests)
;;   - mark-graf-regex-test.el     : Regex pattern tests (~40 tests)
;;   - mark-graf-integration-test.el : End-to-end tests (~15 tests)
;;
;; Run tests:
;;   M-x mark-graf-run-all-tests
;;   M-x ert RET t RET
;;   make test

;;; Code:

(require 'ert)

;; Add test directory to load path
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-dir)
  (add-to-list 'load-path (expand-file-name "../lisp" test-dir)))

;; Load mark-graf
(require 'mark-graf)

;; Load test modules
(require 'mark-graf-export-test)
(require 'mark-graf-commands-test)
(require 'mark-graf-regex-test)
(require 'mark-graf-integration-test)

;;; Test Runner Functions

(defun mark-graf-run-all-tests ()
  "Run all mark-graf tests interactively."
  (interactive)
  (ert-run-tests-interactively
   "^\\(export\\|cmd\\|regex\\|integration\\)/"))

(defun mark-graf-run-export-tests ()
  "Run only export tests."
  (interactive)
  (ert-run-tests-interactively "^export/"))

(defun mark-graf-run-command-tests ()
  "Run only command tests."
  (interactive)
  (ert-run-tests-interactively "^cmd/"))

(defun mark-graf-run-regex-tests ()
  "Run only regex tests."
  (interactive)
  (ert-run-tests-interactively "^regex/"))

(defun mark-graf-run-integration-tests ()
  "Run only integration tests."
  (interactive)
  (ert-run-tests-interactively "^integration/"))

;;; Batch Test Runner (for CI/Makefile)

(defun mark-graf-run-tests-batch-and-exit ()
  "Run all tests in batch mode and exit with appropriate code."
  (let ((test-selector "^\\(export\\|cmd\\|regex\\|integration\\)/"))
    (ert-run-tests-batch-and-exit test-selector)))

;;; Test Statistics

(defun mark-graf-test-stats ()
  "Display test statistics."
  (interactive)
  (let ((export-count 0)
        (cmd-count 0)
        (regex-count 0)
        (integration-count 0))
    (mapatoms
     (lambda (sym)
       (when (ert-test-boundp sym)
         (let ((name (symbol-name sym)))
           (cond
            ((string-prefix-p "export/" name) (cl-incf export-count))
            ((string-prefix-p "cmd/" name) (cl-incf cmd-count))
            ((string-prefix-p "regex/" name) (cl-incf regex-count))
            ((string-prefix-p "integration/" name) (cl-incf integration-count)))))))
    (message "mark-graf test counts:
  Export tests:      %d
  Command tests:     %d
  Regex tests:       %d
  Integration tests: %d
  ─────────────────────
  Total:             %d"
             export-count cmd-count regex-count integration-count
             (+ export-count cmd-count regex-count integration-count))))

(provide 'mark-graf-test)
;;; mark-graf-test.el ends here

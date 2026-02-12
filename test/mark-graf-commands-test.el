;;; mark-graf-commands-test.el --- Command tests for mark-graf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;;; Commentary:

;; Unit tests for mark-graf interactive commands.
;; Tests buffer manipulation, cursor positioning, and user-facing functions.

;;; Code:

(require 'ert)
(require 'mark-graf-commands)
(require 'mark-graf-elements)
(require 'mark-graf-render)

;;; Test Helper Macro

(defmacro mg-cmd-test (name doc initial expected &rest body)
  "Define a command test NAME with DOC.
INITIAL is the starting buffer state (| marks cursor).
EXPECTED is the expected final state (| marks expected cursor).
BODY contains the commands to execute."
  (declare (indent 3))
  `(ert-deftest ,(intern (format "cmd/%s" name)) ()
     ,doc
     (with-temp-buffer
       ;; Parse initial state
       (let* ((init-str ,initial)
              (cursor-pos (string-match "|" init-str))
              (content (replace-regexp-in-string "|" "" init-str)))
         (insert content)
         (goto-char (1+ (or cursor-pos 0))))
       ;; Execute body
       ,@body
       ;; Check expected state
       (let* ((exp-str ,expected)
              (exp-cursor (string-match "|" exp-str))
              (exp-content (replace-regexp-in-string "|" "" exp-str)))
         (should (equal (buffer-string) exp-content))
         (when exp-cursor
           (should (= (point) (1+ exp-cursor))))))))

(defmacro mg-cmd-test-region (name doc initial expected &rest body)
  "Like `mg-cmd-test' but with region support.
Use [ and ] to mark region boundaries in INITIAL."
  (declare (indent 3))
  `(ert-deftest ,(intern (format "cmd/%s" name)) ()
     ,doc
     (with-temp-buffer
       ;; Parse initial state with region markers
       (let* ((init-str ,initial)
              (region-start (string-match "\\[" init-str))
              (region-end (1- (string-match "\\]" init-str)))  ; Adjust for [ removal
              (content (replace-regexp-in-string "\\[\\|\\]" "" init-str)))
         (insert content)
         (when (and region-start region-end)
           (set-mark (1+ region-start))
           (goto-char (1+ region-end))
           (activate-mark)))
       ;; Execute body
       ,@body
       ;; Check expected state
       (let* ((exp-str ,expected)
              (exp-content (replace-regexp-in-string "|\\|\\[\\|\\]" "" exp-str)))
         (should (equal (buffer-string) exp-content))))))

;;; Style Insertion Tests - No Region

(mg-cmd-test bold-insert-empty
  "Bold inserts ** pair at point"
  "|" "**|**"
  (mark-graf-insert-bold))

(mg-cmd-test italic-insert-empty
  "Italic inserts * pair at point"
  "|" "*|*"
  (mark-graf-insert-italic))

(mg-cmd-test code-insert-empty
  "Code inserts ` pair at point"
  "|" "`|`"
  (mark-graf-insert-code))

(mg-cmd-test strike-insert-empty
  "Strike inserts ~~ pair at point"
  "|" "~~|~~"
  (mark-graf-insert-strike))

(mg-cmd-test kbd-insert-empty
  "Kbd inserts <kbd> pair at point"
  "|" "<kbd>|</kbd>"
  (mark-graf-insert-kbd))

;;; Style Insertion Tests - With Region

(mg-cmd-test-region bold-wrap-region
  "Bold wraps selected text"
  "[hello]" "**hello**"
  (mark-graf-insert-bold))

(mg-cmd-test-region italic-wrap-region
  "Italic wraps selected text"
  "[world]" "*world*"
  (mark-graf-insert-italic))

(mg-cmd-test-region code-wrap-region
  "Code wraps selected text"
  "[foo]" "`foo`"
  (mark-graf-insert-code))

(mg-cmd-test-region strike-wrap-region
  "Strike wraps selected text"
  "[bar]" "~~bar~~"
  (mark-graf-insert-strike))

;;; Heading Tests

(mg-cmd-test heading-level-1
  "Insert level 1 heading"
  "|" "# |"
  (mark-graf-insert-heading-1))

(mg-cmd-test heading-level-2
  "Insert level 2 heading"
  "|" "## |"
  (mark-graf-insert-heading-2))

(mg-cmd-test heading-level-3
  "Insert level 3 heading"
  "|" "### |"
  (mark-graf-insert-heading-3))

(mg-cmd-test heading-level-6
  "Insert level 6 heading"
  "|" "###### |"
  (mark-graf-insert-heading-6))

(mg-cmd-test heading-at-existing-text
  "Heading at line start prepends markers"
  "|text" "## |text"
  (mark-graf-insert-heading 2))

(mg-cmd-test heading-promote
  "Promote heading decreases level"
  "|## Title" "|# Title"
  (mark-graf-promote-heading))

(mg-cmd-test heading-demote
  "Demote heading increases level"
  "|# Title" "|## Title"
  (mark-graf-demote-heading))

(mg-cmd-test heading-promote-at-h1
  "Promote at H1 does nothing"
  "|# Title" "|# Title"
  (mark-graf-promote-heading))

;;; List Tests

(mg-cmd-test list-item-new
  "New list item at empty buffer"
  "|" "- |"
  (mark-graf-insert-list-item))

(mg-cmd-test list-item-continue
  "Continue existing list"
  "- item|" "- item\n- |"
  (mark-graf-insert-list-item))

(mg-cmd-test list-item-ordered-continue
  "Continue ordered list increments number"
  "1. first|" "1. first\n2. |"
  (mark-graf-insert-list-item))

(mg-cmd-test checkbox-toggle-unchecked
  "Toggle unchecked checkbox to checked"
  "|- [ ] task" "|- [x] task"
  (mark-graf-toggle-checkbox))

(mg-cmd-test checkbox-toggle-checked
  "Toggle checked checkbox to unchecked"
  "|- [x] task" "|- [ ] task"
  (mark-graf-toggle-checkbox))

(mg-cmd-test checkbox-toggle-uppercase
  "Toggle uppercase X checkbox"
  "|- [X] task" "|- [ ] task"
  (mark-graf-toggle-checkbox))

(mg-cmd-test list-promote
  "Promote list item decreases indent"
  "|  - item" "|- item"
  (mark-graf-promote-item))

(mg-cmd-test list-demote
  "Demote list item increases indent"
  "|- item" "|  - item"
  (mark-graf-demote-item))

;;; Blockquote Tests

(mg-cmd-test blockquote-new-line
  "Blockquote at empty line"
  "|" "> |"
  (mark-graf-insert-blockquote))

(mg-cmd-test blockquote-increase-level
  "Blockquote on existing quote adds level"
  "|> text" "|>> text"
  (mark-graf-insert-blockquote))

;;; Code Block Tests

(ert-deftest cmd/code-block-insert ()
  "Code block inserts fences."
  (with-temp-buffer
    (mark-graf-insert-code-block "python")
    (should (string-match-p "```python" (buffer-string)))
    (should (string-match-p "```$" (buffer-string)))))

(ert-deftest cmd/code-block-empty-language ()
  "Code block without language."
  (with-temp-buffer
    (mark-graf-insert-code-block "")
    (should (string-match-p "^```\n" (buffer-string)))))

;;; Link and Image Tests

(ert-deftest cmd/link-insert ()
  "Link insertion creates proper markdown."
  (with-temp-buffer
    (mark-graf-insert-link "http://example.com" "Example")
    (should (equal (buffer-string) "[Example](http://example.com)"))))

(ert-deftest cmd/link-insert-empty-url ()
  "Link with empty URL positions cursor."
  (with-temp-buffer
    (mark-graf-insert-link "" "text")
    (should (equal (buffer-string) "[text]()"))))

(ert-deftest cmd/image-insert ()
  "Image insertion creates proper markdown."
  (with-temp-buffer
    (mark-graf-insert-image "photo.jpg" "My Photo")
    (should (equal (buffer-string) "![My Photo](photo.jpg)"))))

;;; Table Tests

(ert-deftest cmd/table-insert ()
  "Table creation with specified dimensions."
  (with-temp-buffer
    (mark-graf-insert-table 2 3)
    (let ((content (buffer-string)))
      (should (string-match-p "| Header 1" content))
      (should (string-match-p "| Header 2" content))
      (should (string-match-p "| Header 3" content))
      (should (string-match-p "|---" content)))))

;;; Navigation Tests

(ert-deftest cmd/next-heading ()
  "Navigate to next heading."
  (with-temp-buffer
    (insert "# First\n\ntext\n\n## Second")
    (goto-char (point-min))
    (mark-graf-next-heading)
    (should (looking-at "## Second"))))

(ert-deftest cmd/prev-heading ()
  "Navigate to previous heading."
  (with-temp-buffer
    (insert "# First\n\ntext\n\n## Second")
    (goto-char (point-max))
    (mark-graf-prev-heading)
    (should (looking-at "## Second"))
    (mark-graf-prev-heading)
    (should (looking-at "# First"))))

(ert-deftest cmd/next-heading-same-level ()
  "Navigate to next heading at same level."
  (with-temp-buffer
    (insert "## A\n\n### Sub\n\n## B")
    (goto-char (point-min))
    (mark-graf-next-heading-same-level)
    (should (looking-at "## B"))))

(ert-deftest cmd/up-heading ()
  "Navigate to parent heading."
  (with-temp-buffer
    (insert "# Parent\n\n## Child\n\n### Grandchild")
    (goto-char (point-max))
    (beginning-of-line)
    (mark-graf-up-heading)
    (should (looking-at "## Child"))))

;;; View Toggle Tests

(ert-deftest cmd/toggle-view ()
  "Toggle between source and rendered view."
  (with-temp-buffer
    (setq-local mark-graf--rendering-enabled t)
    (mark-graf-show-source)
    (should-not mark-graf--rendering-enabled)
    (mark-graf-show-rendered)
    (should mark-graf--rendering-enabled)))

;;; Utility Function Tests

(ert-deftest cmd/wrap-region-or-insert-no-region ()
  "Wrap helper inserts pair when no region."
  (with-temp-buffer
    (mark-graf--wrap-region-or-insert "<<" ">>")
    (should (equal (buffer-string) "<<>>"))
    (should (= (point) 3))))  ; Cursor between

(ert-deftest cmd/wrap-region-or-insert-with-region ()
  "Wrap helper wraps text when region active."
  (with-temp-buffer
    (insert "text")
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (mark-graf--wrap-region-or-insert "<<" ">>")
    (should (equal (buffer-string) "<<text>>"))))

(ert-deftest cmd/at-line-start-p ()
  "Line start detection."
  (with-temp-buffer
    (insert "  text")
    (goto-char 1)
    (should (mark-graf--at-line-start-p))
    (goto-char 3)
    (should (mark-graf--at-line-start-p))  ; After whitespace
    (goto-char 5)
    (should-not (mark-graf--at-line-start-p))))  ; After text

(provide 'mark-graf-commands-test)
;;; mark-graf-commands-test.el ends here

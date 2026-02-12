;;; mark-graf-regex-test.el --- Regex pattern tests for mark-graf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;;; Commentary:

;; Unit tests for mark-graf regex patterns used in fallback parsing.
;; These patterns are critical for when tree-sitter is unavailable.

;;; Code:

(require 'ert)
(require 'mark-graf-ts)

;;; Test Helper Macro

(defmacro mg-regex-test (name pattern input &rest expected)
  "Define regex test NAME.
Test that PATTERN matches INPUT and captures EXPECTED groups."
  (declare (indent 2))
  `(ert-deftest ,(intern (format "regex/%s" name)) ()
     ,(format "Test regex pattern for %s." name)
     (with-temp-buffer
       (insert ,input)
       (goto-char (point-min))
       (should (re-search-forward ,pattern nil t))
       ,@(cl-loop for exp in expected
                  for i from 1
                  collect `(should (equal (match-string ,i) ,exp))))))

(defmacro mg-regex-no-match (name pattern input)
  "Define negative regex test - PATTERN should NOT match INPUT."
  (declare (indent 2))
  `(ert-deftest ,(intern (format "regex/%s-no-match" name)) ()
     ,(format "Test that %s pattern doesn't match invalid input." name)
     (with-temp-buffer
       (insert ,input)
       (goto-char (point-min))
       (should-not (re-search-forward ,pattern nil t)))))

;;; Heading Pattern Tests

(mg-regex-test heading-h1
  mark-graf-ts--heading-regex
  "# Title"
  "#" "Title")

(mg-regex-test heading-h2
  mark-graf-ts--heading-regex
  "## Subtitle"
  "##" "Subtitle")

(mg-regex-test heading-h3
  mark-graf-ts--heading-regex
  "### Section Name"
  "###" "Section Name")

(mg-regex-test heading-h6
  mark-graf-ts--heading-regex
  "###### Deep heading"
  "######" "Deep heading")

(mg-regex-test heading-with-trailing-space
  mark-graf-ts--heading-regex
  "## Title  "
  "##" "Title  ")

(mg-regex-no-match heading-no-space
  mark-graf-ts--heading-regex
  "#NoSpace")

(mg-regex-no-match heading-seven-hashes
  mark-graf-ts--heading-regex
  "####### Invalid")

;;; Bold Pattern Tests

(mg-regex-test bold-asterisks
  mark-graf-ts--strong-regex
  "**bold text**"
  "**bold text**" "bold text")

(mg-regex-test bold-underscores
  mark-graf-ts--strong-regex
  "__bold text__"
  "__bold text__" nil "bold text")

(mg-regex-test bold-single-word
  mark-graf-ts--strong-regex
  "**word**"
  "**word**" "word")

(mg-regex-no-match bold-single-asterisk
  "\\*\\*[^*]+\\*\\*"
  "*not bold*")

;;; Italic Pattern Tests

(mg-regex-test italic-asterisk
  mark-graf-ts--emphasis-regex
  "*italic text*"
  "*italic text*" "italic text")

(mg-regex-test italic-underscore
  mark-graf-ts--emphasis-regex
  "_italic text_"
  "_italic text_" nil "italic text")

;;; Code Span Pattern Tests

(mg-regex-test code-span-basic
  mark-graf-ts--code-span-regex
  "`code`"
  "code")

(mg-regex-test code-span-with-spaces
  mark-graf-ts--code-span-regex
  "`code with spaces`"
  "code with spaces")

(mg-regex-test code-span-special-chars
  mark-graf-ts--code-span-regex
  "`<div class=\"foo\">`"
  "<div class=\"foo\">")

;;; Link Pattern Tests

(mg-regex-test link-basic
  mark-graf-ts--link-regex
  "[text](http://example.com)"
  "text" "http://example.com")

(mg-regex-test link-with-path
  mark-graf-ts--link-regex
  "[docs](/path/to/doc)"
  "docs" "/path/to/doc")

(mg-regex-test link-with-spaces-in-text
  mark-graf-ts--link-regex
  "[link with spaces](http://x)"
  "link with spaces" "http://x")

(mg-regex-test link-complex-url
  mark-graf-ts--link-regex
  "[api](https://api.example.com/v1/users?id=123)"
  "api" "https://api.example.com/v1/users?id=123")

;;; Code Block Pattern Tests

(mg-regex-test code-block-with-language
  mark-graf-ts--code-block-regex
  "```python"
  "python")

(mg-regex-test code-block-javascript
  mark-graf-ts--code-block-regex
  "```javascript"
  "javascript")

(mg-regex-test code-block-no-language
  "^```\\([a-zA-Z0-9]*\\)?$"
  "```"
  "")

(mg-regex-test code-block-tilde
  "^~~~\\([a-zA-Z0-9]*\\)?$"
  "~~~ruby"
  "ruby")

;;; List Pattern Tests

(ert-deftest regex/unordered-list-dash ()
  "Unordered list with dash."
  (with-temp-buffer
    (insert "- Item")
    (goto-char (point-min))
    (should (looking-at "^\\([ \t]*\\)\\([-*+]\\)[ \t]+"))))

(ert-deftest regex/unordered-list-asterisk ()
  "Unordered list with asterisk."
  (with-temp-buffer
    (insert "* Item")
    (goto-char (point-min))
    (should (looking-at "^\\([ \t]*\\)\\([-*+]\\)[ \t]+"))))

(ert-deftest regex/unordered-list-plus ()
  "Unordered list with plus."
  (with-temp-buffer
    (insert "+ Item")
    (goto-char (point-min))
    (should (looking-at "^\\([ \t]*\\)\\([-*+]\\)[ \t]+"))))

(ert-deftest regex/ordered-list-dot ()
  "Ordered list with dot."
  (with-temp-buffer
    (insert "1. Item")
    (goto-char (point-min))
    (should (looking-at "^\\([ \t]*\\)\\([0-9]+\\)\\([.)]\\)[ \t]+"))))

(ert-deftest regex/ordered-list-paren ()
  "Ordered list with parenthesis."
  (with-temp-buffer
    (insert "1) Item")
    (goto-char (point-min))
    (should (looking-at "^\\([ \t]*\\)\\([0-9]+\\)\\([.)]\\)[ \t]+"))))

(ert-deftest regex/nested-list ()
  "Nested list indentation captured."
  (with-temp-buffer
    (insert "  - Nested")
    (goto-char (point-min))
    (should (looking-at "^\\([ \t]*\\)\\([-*+]\\)"))
    (should (equal (match-string 1) "  "))))

;;; Task List Pattern Tests

(ert-deftest regex/task-unchecked ()
  "Unchecked task list item."
  (with-temp-buffer
    (insert "- [ ] Todo")
    (goto-char (point-min))
    (should (looking-at "^[ \t]*[-*+][ \t]+\\(\\[[ xX]\\]\\)"))
    (should (equal (match-string 1) "[ ]"))))

(ert-deftest regex/task-checked-lower ()
  "Checked task list item (lowercase x)."
  (with-temp-buffer
    (insert "- [x] Done")
    (goto-char (point-min))
    (should (looking-at "^[ \t]*[-*+][ \t]+\\(\\[[ xX]\\]\\)"))
    (should (equal (match-string 1) "[x]"))))

(ert-deftest regex/task-checked-upper ()
  "Checked task list item (uppercase X)."
  (with-temp-buffer
    (insert "- [X] Done")
    (goto-char (point-min))
    (should (looking-at "^[ \t]*[-*+][ \t]+\\(\\[[ xX]\\]\\)"))
    (should (equal (match-string 1) "[X]"))))

;;; Table Pattern Tests

(ert-deftest regex/table-row ()
  "Table row pattern."
  (with-temp-buffer
    (insert "| A | B | C |")
    (goto-char (point-min))
    (should (looking-at "^|\\(.+\\)|[ \t]*$"))))

(ert-deftest regex/table-separator ()
  "Table separator row."
  (with-temp-buffer
    (insert "|---|---|---|")
    (goto-char (point-min))
    (should (looking-at "^|\\([-:|]+\\)|[ \t]*$"))))

(ert-deftest regex/table-aligned-separator ()
  "Table separator with alignment."
  (with-temp-buffer
    (insert "|:---|:---:|---:|")
    (goto-char (point-min))
    (should (looking-at "^|\\([-:|]+\\)|"))))

;;; Blockquote Pattern Tests

(ert-deftest regex/blockquote-single ()
  "Single level blockquote."
  (with-temp-buffer
    (insert "> Quote")
    (goto-char (point-min))
    (should (looking-at "^\\(>+\\)[ \t]?"))
    (should (equal (match-string 1) ">"))))

(ert-deftest regex/blockquote-nested ()
  "Nested blockquote."
  (with-temp-buffer
    (insert ">> Nested")
    (goto-char (point-min))
    (should (looking-at "^\\(>+\\)[ \t]?"))
    (should (equal (match-string 1) ">>"))))

(ert-deftest regex/blockquote-triple ()
  "Triple nested blockquote."
  (with-temp-buffer
    (insert ">>> Deep")
    (goto-char (point-min))
    (should (looking-at "^\\(>+\\)"))
    (should (equal (match-string 1) ">>>"))))

;;; Horizontal Rule Pattern Tests

(ert-deftest regex/hr-dashes ()
  "HR with dashes."
  (with-temp-buffer
    (insert "---")
    (goto-char (point-min))
    (should (looking-at "^\\(---+\\|\\*\\*\\*+\\|___+\\)$"))))

(ert-deftest regex/hr-asterisks ()
  "HR with asterisks."
  (with-temp-buffer
    (insert "***")
    (goto-char (point-min))
    (should (looking-at "^\\(---+\\|\\*\\*\\*+\\|___+\\)$"))))

(ert-deftest regex/hr-underscores ()
  "HR with underscores."
  (with-temp-buffer
    (insert "___")
    (goto-char (point-min))
    (should (looking-at "^\\(---+\\|\\*\\*\\*+\\|___+\\)$"))))

(ert-deftest regex/hr-long ()
  "HR with many characters."
  (with-temp-buffer
    (insert "----------")
    (goto-char (point-min))
    (should (looking-at "^---+$"))))

;;; Image Pattern Tests

(ert-deftest regex/image-basic ()
  "Basic image pattern."
  (with-temp-buffer
    (insert "![alt](image.png)")
    (goto-char (point-min))
    (should (looking-at "!\\[\\([^]]*\\)\\](\\([^)]+\\))"))
    (should (equal (match-string 1) "alt"))
    (should (equal (match-string 2) "image.png"))))

(ert-deftest regex/image-empty-alt ()
  "Image with empty alt."
  (with-temp-buffer
    (insert "![](image.png)")
    (goto-char (point-min))
    (should (looking-at "!\\[\\([^]]*\\)\\](\\([^)]+\\))"))
    (should (equal (match-string 1) ""))))

(ert-deftest regex/image-url ()
  "Image with URL source."
  (with-temp-buffer
    (insert "![logo](https://example.com/logo.png)")
    (goto-char (point-min))
    (should (looking-at "!\\[\\([^]]*\\)\\](\\([^)]+\\))"))
    (should (equal (match-string 2) "https://example.com/logo.png"))))

;;; Strikethrough Pattern Tests

(ert-deftest regex/strikethrough ()
  "Strikethrough pattern."
  (with-temp-buffer
    (insert "~~struck~~")
    (goto-char (point-min))
    (should (looking-at "~~\\([^~]+\\)~~"))
    (should (equal (match-string 1) "struck"))))

(ert-deftest regex/strikethrough-with-spaces ()
  "Strikethrough with spaces."
  (with-temp-buffer
    (insert "~~struck text~~")
    (goto-char (point-min))
    (should (looking-at "~~\\([^~]+\\)~~"))
    (should (equal (match-string 1) "struck text"))))

(provide 'mark-graf-regex-test)
;;; mark-graf-regex-test.el ends here

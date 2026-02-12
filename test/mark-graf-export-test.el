;;; mark-graf-export-test.el --- Export tests for mark-graf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;;; Commentary:

;; Unit tests for mark-graf HTML export functionality.
;; These tests verify pure transformation functions with no side effects.

;;; Code:

(require 'ert)
(require 'mark-graf-export)

;;; Helper Functions

(defun mg-export (markdown)
  "Convert MARKDOWN to HTML for testing."
  (mark-graf-export--markdown-to-html markdown))

(defun mg-html-contains (markdown &rest patterns)
  "Test that MARKDOWN export contains all PATTERNS."
  (let ((html (mg-export markdown)))
    (dolist (pat patterns)
      (should (string-match-p (regexp-quote pat) html)))))

(defun mg-html-matches (markdown regex)
  "Test that MARKDOWN export matches REGEX."
  (should (string-match-p regex (mg-export markdown))))

;;; Heading Tests

(ert-deftest export/heading-h1 ()
  "H1 heading converts correctly."
  (mg-html-contains "# Title" "<h1" ">Title</h1>"))

(ert-deftest export/heading-h2 ()
  "H2 heading converts correctly."
  (mg-html-contains "## Subtitle" "<h2" ">Subtitle</h2>"))

(ert-deftest export/heading-h3 ()
  "H3 heading converts correctly."
  (mg-html-contains "### Section" "<h3" ">Section</h3>"))

(ert-deftest export/heading-h4 ()
  "H4 heading converts correctly."
  (mg-html-contains "#### Subsection" "<h4" ">Subsection</h4>"))

(ert-deftest export/heading-h5 ()
  "H5 heading converts correctly."
  (mg-html-contains "##### Minor" "<h5" ">Minor</h5>"))

(ert-deftest export/heading-h6 ()
  "H6 heading converts correctly."
  (mg-html-contains "###### Smallest" "<h6" ">Smallest</h6>"))

(ert-deftest export/heading-id-generation ()
  "Headings get appropriate IDs."
  (mg-html-matches "# Hello World" "id=\"hello-world\""))

(ert-deftest export/heading-id-special-chars ()
  "Heading IDs handle special characters."
  (let ((id (mark-graf-export--heading-id "Test: Special (Chars)!")))
    (should (string-match-p "^[a-z0-9-]+$" id))
    (should-not (string-match-p "[^a-z0-9-]" id))))

;;; Inline Formatting Tests

(ert-deftest export/bold-asterisks ()
  "Bold with ** converts correctly."
  (mg-html-contains "**bold**" "<strong>bold</strong>"))

(ert-deftest export/bold-underscores ()
  "Bold with __ converts correctly."
  (mg-html-contains "__bold__" "<strong>bold</strong>"))

(ert-deftest export/italic-asterisk ()
  "Italic with * converts correctly."
  (mg-html-contains "*italic*" "<em>italic</em>"))

(ert-deftest export/italic-underscore ()
  "Italic with _ converts correctly."
  (mg-html-contains "_italic_" "<em>italic</em>"))

(ert-deftest export/strikethrough ()
  "Strikethrough converts correctly."
  (mg-html-contains "~~struck~~" "<del>struck</del>"))

(ert-deftest export/inline-code ()
  "Inline code converts correctly."
  (mg-html-contains "`code`" "<code>code</code>"))

(ert-deftest export/inline-code-escapes-html ()
  "Inline code escapes HTML entities."
  (mg-html-contains "`<div>`" "&lt;div&gt;"))

(ert-deftest export/mixed-inline ()
  "Mixed inline formatting in paragraph."
  (let ((html (mg-export "Text with **bold** and *italic* and `code`.")))
    (should (string-match-p "<strong>bold</strong>" html))
    (should (string-match-p "<em>italic</em>" html))
    (should (string-match-p "<code>code</code>" html))))

;;; Link Tests

(ert-deftest export/inline-link ()
  "Inline link converts correctly."
  (mg-html-contains "[text](http://example.com)"
                    "<a href=\"http://example.com\">text</a>"))

(ert-deftest export/link-with-title ()
  "Link text preserved correctly."
  (mg-html-matches "[Click here](http://x)" ">Click here</a>"))

(ert-deftest export/multiple-links ()
  "Multiple links in same paragraph."
  (let ((html (mg-export "[a](http://a) and [b](http://b)")))
    (should (string-match-p "href=\"http://a\"" html))
    (should (string-match-p "href=\"http://b\"" html))))

;;; Image Tests

(ert-deftest export/image-basic ()
  "Basic image converts correctly."
  (mg-html-contains "![alt](image.png)"
                    "<img src=\"image.png\" alt=\"alt\">"))

(ert-deftest export/image-empty-alt ()
  "Image with empty alt text."
  (mg-html-contains "![](image.png)"
                    "<img src=\"image.png\" alt=\"\">"))

(ert-deftest export/image-url ()
  "Image with URL source."
  (mg-html-contains "![logo](https://example.com/logo.png)"
                    "src=\"https://example.com/logo.png\""))

;;; Code Block Tests

(ert-deftest export/fenced-code-no-language ()
  "Fenced code block without language."
  (let ((html (mg-export "```\ncode here\n```")))
    (should (string-match-p "<pre><code>" html))
    (should (string-match-p "code here" html))))

(ert-deftest export/fenced-code-with-language ()
  "Fenced code block with language."
  (mg-html-contains "```python\nprint('hi')\n```"
                    "<code class=\"language-python\">"))

(ert-deftest export/fenced-code-escapes-html ()
  "Code block escapes HTML."
  (mg-html-contains "```\n<script>alert('xss')</script>\n```"
                    "&lt;script&gt;"))

(ert-deftest export/fenced-code-preserves-content ()
  "Code block preserves content."
  (let ((html (mg-export "```js\nfunction() {\n  return 42;\n}\n```")))
    (should (string-match-p "function" html))
    (should (string-match-p "return 42" html))))

;;; List Tests

(ert-deftest export/unordered-list-dash ()
  "Unordered list with - markers."
  (let ((html (mg-export "- Item 1\n- Item 2\n- Item 3")))
    (should (string-match-p "<ul>" html))
    (should (string-match-p "<li>Item 1</li>" html))
    (should (string-match-p "<li>Item 2</li>" html))
    (should (string-match-p "</ul>" html))))

(ert-deftest export/unordered-list-asterisk ()
  "Unordered list with * markers."
  (mg-html-contains "* Item 1\n* Item 2" "<ul>" "<li>Item 1</li>"))

(ert-deftest export/ordered-list ()
  "Ordered list converts correctly."
  (let ((html (mg-export "1. First\n2. Second\n3. Third")))
    (should (string-match-p "<ol>" html))
    (should (string-match-p "<li>First</li>" html))
    (should (string-match-p "</ol>" html))))

(ert-deftest export/task-list-unchecked ()
  "Task list with unchecked items."
  (mg-html-contains "- [ ] Todo"
                    "task-list-item"
                    "type=\"checkbox\""))

(ert-deftest export/task-list-checked ()
  "Task list with checked items."
  (mg-html-contains "- [x] Done"
                    "checked"
                    "type=\"checkbox\""))

;;; Table Tests

(ert-deftest export/table-basic ()
  "Basic table structure."
  (let ((html (mg-export "| A | B |\n|---|---|\n| 1 | 2 |")))
    (should (string-match-p "<table>" html))
    (should (string-match-p "<th>A</th>" html))
    (should (string-match-p "<td>1</td>" html))
    (should (string-match-p "</table>" html))))

(ert-deftest export/table-header ()
  "Table header row marked correctly."
  (mg-html-contains "| H1 | H2 |\n|---|---|\n| D1 | D2 |"
                    "<thead>" "<th>H1</th>" "</thead>"))

(ert-deftest export/table-body ()
  "Table body rows marked correctly."
  (mg-html-contains "| H |\n|---|\n| D |"
                    "<tbody>" "<td>D</td>" "</tbody>"))

;;; Blockquote Tests

(ert-deftest export/blockquote-single-line ()
  "Single line blockquote."
  (mg-html-contains "> Quote" "<blockquote>" "Quote" "</blockquote>"))

(ert-deftest export/blockquote-multi-line ()
  "Multi-line blockquote."
  (let ((html (mg-export "> Line 1\n> Line 2")))
    (should (string-match-p "<blockquote>" html))
    (should (string-match-p "Line 1" html))
    (should (string-match-p "Line 2" html))))

;;; Horizontal Rule Tests

(ert-deftest export/hr-dashes ()
  "HR with dashes."
  (mg-html-contains "---" "<hr>"))

(ert-deftest export/hr-asterisks ()
  "HR with asterisks."
  (mg-html-contains "***" "<hr>"))

(ert-deftest export/hr-underscores ()
  "HR with underscores."
  (mg-html-contains "___" "<hr>"))

;;; Paragraph Tests

(ert-deftest export/paragraph-basic ()
  "Basic paragraph wrapping."
  (mg-html-matches "Plain text" "<p>Plain text</p>"))

(ert-deftest export/paragraph-multiple ()
  "Multiple paragraphs separated by blank lines."
  (let ((html (mg-export "Para 1\n\nPara 2")))
    (should (string-match-p "<p>Para 1</p>" html))
    (should (string-match-p "<p>Para 2</p>" html))))

;;; HTML Escaping Tests

(ert-deftest export/escape-ampersand ()
  "Ampersand escaped correctly."
  (should (equal (mark-graf-export--escape-html "A & B") "A &amp; B")))

(ert-deftest export/escape-less-than ()
  "Less-than escaped correctly."
  (should (equal (mark-graf-export--escape-html "a < b") "a &lt; b")))

(ert-deftest export/escape-greater-than ()
  "Greater-than escaped correctly."
  (should (equal (mark-graf-export--escape-html "a > b") "a &gt; b")))

(ert-deftest export/escape-quotes ()
  "Quotes escaped correctly."
  (should (equal (mark-graf-export--escape-html "say \"hi\"") "say &quot;hi&quot;")))

(ert-deftest export/escape-combined ()
  "Multiple special chars escaped."
  (should (equal (mark-graf-export--escape-html "<a & b>")
                 "&lt;a &amp; b&gt;")))

(provide 'mark-graf-export-test)
;;; mark-graf-export-test.el ends here

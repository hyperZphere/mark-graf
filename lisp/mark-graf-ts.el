;;; mark-graf-ts.el --- Tree-sitter integration for mark-graf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;; This file is part of mark-graf.

;;; Commentary:

;; Tree-sitter integration layer for mark-graf.
;; Provides parsing, AST queries, and element detection.

;;; Code:

(require 'treesit)
(require 'cl-lib)

;;; Data Structures

(cl-defstruct mark-graf-node
  "Represents a parsed markdown element."
  type        ; Symbol: 'heading, 'emphasis, 'code-block, etc.
  start       ; Buffer position (1-indexed)
  end         ; Buffer position (1-indexed)
  level       ; For headings: 1-6; for lists: nesting depth
  language    ; For code blocks: language identifier
  children    ; List of child mark-graf-node
  properties  ; Plist of additional properties
  treesit-node) ; The underlying treesit node

;;; Internal Variables

(defvar-local mark-graf-ts--parser nil
  "Tree-sitter parser for markdown.")

(defvar-local mark-graf-ts--inline-parser nil
  "Tree-sitter parser for markdown-inline.")

(defvar-local mark-graf-ts--parse-cache nil
  "Cache of parsed regions.")

;;; Grammar Management

(defconst mark-graf-ts--grammar-sources
  '((markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 "split_parser"
                 "tree-sitter-markdown/src"))
    (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                        "split_parser"
                        "tree-sitter-markdown-inline/src")))
  "Tree-sitter grammar sources for markdown.")

(defun mark-graf-ts--ensure-grammar ()
  "Ensure markdown tree-sitter grammar is available."
  (when mark-graf-ts--use-tree-sitter
    (condition-case err
        (unless (treesit-language-available-p 'markdown)
          (if (yes-or-no-p "Markdown tree-sitter grammar not found. Install it? ")
              (mark-graf-ts--install-grammar)
            (mark-graf-ts--enable-fallback-mode)))
      (error
       (message "mark-graf: Grammar check failed (%s), using fallback" (error-message-string err))
       (mark-graf-ts--enable-fallback-mode)))))

(defun mark-graf-ts--install-grammar ()
  "Install markdown tree-sitter grammar."
  (condition-case err
      (let ((treesit-language-source-alist mark-graf-ts--grammar-sources))
        (message "Installing markdown grammar...")
        (treesit-install-language-grammar 'markdown)
        (message "Installing markdown-inline grammar...")
        (treesit-install-language-grammar 'markdown-inline)
        (message "Grammars installed successfully."))
    (error
     (message "mark-graf: Grammar installation failed (%s), using fallback" (error-message-string err))
     (mark-graf-ts--enable-fallback-mode))))

(defvar-local mark-graf-ts--use-tree-sitter nil
  "Whether tree-sitter is being used for parsing.
Set to t before loading mark-graf to enable tree-sitter
\(requires markdown grammar to be installed).")

(defun mark-graf-ts--enable-fallback-mode ()
  "Enable regex-based parsing fallback when tree-sitter unavailable."
  (setq mark-graf-ts--use-tree-sitter nil)
  (message "mark-graf: Using fallback regex parser (limited functionality)"))

;;; Parser Initialization

(defun mark-graf-ts--init ()
  "Initialize tree-sitter parsers for the current buffer."
  (when mark-graf-ts--use-tree-sitter
    (condition-case err
        (progn
          (when (treesit-language-available-p 'markdown)
            (setq mark-graf-ts--parser
                  (treesit-parser-create 'markdown)))
          (when (treesit-language-available-p 'markdown-inline)
            (setq mark-graf-ts--inline-parser
                  (treesit-parser-create 'markdown-inline)))
          (setq mark-graf-ts--parse-cache (make-hash-table :test 'equal)))
      (error
       (message "mark-graf: Tree-sitter init failed (%s), using fallback" (error-message-string err))
       (mark-graf-ts--enable-fallback-mode)))))

;;; Node Type Mapping

(defconst mark-graf-ts--node-type-map
  '(;; Block elements
    ("atx_heading" . heading)
    ("setext_heading" . heading)
    ("paragraph" . paragraph)
    ("fenced_code_block" . code-block)
    ("indented_code_block" . code-block-indented)
    ("block_quote" . blockquote)
    ("list" . list)
    ("list_item" . list-item)
    ("task_list_marker_checked" . task-checked)
    ("task_list_marker_unchecked" . task-unchecked)
    ("thematic_break" . hr)
    ("html_block" . html-block)
    ("link_reference_definition" . link-ref-def)
    ("pipe_table" . table)
    ("pipe_table_header" . table-header)
    ("pipe_table_delimiter_row" . table-delimiter)
    ("pipe_table_row" . table-row)
    ("pipe_table_cell" . table-cell)
    ;; Inline elements
    ("emphasis" . emphasis)
    ("strong_emphasis" . strong)
    ("strikethrough" . strikethrough)
    ("code_span" . code-span)
    ("inline_link" . link)
    ("full_reference_link" . link-ref)
    ("collapsed_reference_link" . link-ref-collapsed)
    ("shortcut_link" . link-shortcut)
    ("image" . image)
    ("uri_autolink" . autolink)
    ("email_autolink" . autolink-email)
    ("hard_line_break" . hard-break)
    ("backslash_escape" . escape)
    ;; Markers/delimiters
    ("atx_h1_marker" . h1-marker)
    ("atx_h2_marker" . h2-marker)
    ("atx_h3_marker" . h3-marker)
    ("atx_h4_marker" . h4-marker)
    ("atx_h5_marker" . h5-marker)
    ("atx_h6_marker" . h6-marker)
    ("list_marker_minus" . list-marker)
    ("list_marker_plus" . list-marker)
    ("list_marker_star" . list-marker)
    ("list_marker_dot" . list-marker-ordered)
    ("list_marker_parenthesis" . list-marker-ordered)
    ("block_quote_marker" . quote-marker)
    ("fenced_code_block_delimiter" . code-fence)
    ("code_fence_content" . code-content)
    ("info_string" . code-language)
    ("link_text" . link-text)
    ("link_destination" . link-url)
    ("link_title" . link-title)
    ("image_description" . image-alt))
  "Mapping from tree-sitter node types to mark-graf element types.")

(defun mark-graf-ts--map-node-type (ts-type)
  "Map tree-sitter node type TS-TYPE to mark-graf type."
  (or (cdr (assoc ts-type mark-graf-ts--node-type-map))
      (intern ts-type)))

;;; Queries

(defconst mark-graf-ts--heading-query
  (treesit-query-compile
   'markdown
   '((atx_heading) @heading))
  "Query for finding headings.")

(defconst mark-graf-ts--block-query
  (treesit-query-compile
   'markdown
   '([(atx_heading)
      (paragraph)
      (fenced_code_block)
      (indented_code_block)
      (block_quote)
      (list)
      (thematic_break)
      (pipe_table)
      (html_block)] @block))
  "Query for finding block elements.")

;;; Node Access Functions

(defun mark-graf-ts--root-node ()
  "Get the root node of the markdown parse tree."
  (when mark-graf-ts--parser
    (treesit-parser-root-node mark-graf-ts--parser)))

(defun mark-graf-ts--node-at (pos)
  "Get the smallest tree-sitter node at position POS."
  (when mark-graf-ts--parser
    (treesit-node-at pos 'markdown)))

(defun mark-graf-ts--element-at (pos)
  "Get the mark-graf element at buffer position POS."
  (when-let ((ts-node (mark-graf-ts--node-at pos)))
    (mark-graf-ts--make-element ts-node)))

(defun mark-graf-ts--make-element (ts-node)
  "Create a mark-graf-node from tree-sitter node TS-NODE."
  (when ts-node
    (let* ((type-str (treesit-node-type ts-node))
           (type (mark-graf-ts--map-node-type type-str))
           (start (treesit-node-start ts-node))
           (end (treesit-node-end ts-node)))
      (make-mark-graf-node
       :type type
       :start start
       :end end
       :level (mark-graf-ts--get-heading-level ts-node)
       :language (mark-graf-ts--get-code-language ts-node)
       :treesit-node ts-node
       :properties nil))))

(defun mark-graf-ts--get-heading-level (ts-node)
  "Get heading level from TS-NODE if it's a heading, nil otherwise."
  (when (string-match-p "heading" (treesit-node-type ts-node))
    (let ((marker (treesit-node-child-by-field-name ts-node "marker")))
      (if marker
          (length (string-trim (treesit-node-text marker)))
        ;; Try to detect from marker node type
        (let ((first-child (treesit-node-child ts-node 0)))
          (when first-child
            (pcase (treesit-node-type first-child)
              ("atx_h1_marker" 1)
              ("atx_h2_marker" 2)
              ("atx_h3_marker" 3)
              ("atx_h4_marker" 4)
              ("atx_h5_marker" 5)
              ("atx_h6_marker" 6)
              (_ 1))))))))

(defun mark-graf-ts--get-code-language (ts-node)
  "Get code language from TS-NODE if it's a code block, nil otherwise."
  (when (string-match-p "code_block" (treesit-node-type ts-node))
    (when-let ((info (treesit-node-child-by-field-name ts-node "info_string")))
      (string-trim (treesit-node-text info)))))

(defun mark-graf-ts--heading-text (node)
  "Get the text content of heading NODE (without markers)."
  (when (eq (mark-graf-node-type node) 'heading)
    (let* ((ts-node (mark-graf-node-treesit-node node))
           (text (treesit-node-text ts-node)))
      ;; Remove leading # markers and whitespace
      (string-trim (replace-regexp-in-string "^#+ *" "" text)))))

;;; Traversal Functions

(defun mark-graf-ts--walk-headings (callback)
  "Walk all headings in buffer, calling CALLBACK with each node."
  (when-let ((root (mark-graf-ts--root-node)))
    (dolist (capture (treesit-query-capture root mark-graf-ts--heading-query))
      (funcall callback (mark-graf-ts--make-element (cdr capture))))))

(defun mark-graf-ts--walk-blocks (callback &optional start end)
  "Walk block elements, calling CALLBACK with each.
Optional START and END limit the range."
  (when-let ((root (mark-graf-ts--root-node)))
    (let ((captures (treesit-query-capture
                     root mark-graf-ts--block-query
                     (or start (point-min))
                     (or end (point-max)))))
      (dolist (capture captures)
        (funcall callback (mark-graf-ts--make-element (cdr capture)))))))

(defun mark-graf-ts--children (node)
  "Get children of NODE as mark-graf-nodes."
  (when-let ((ts-node (mark-graf-node-treesit-node node)))
    (let ((children '())
          (count (treesit-node-child-count ts-node)))
      (dotimes (i count)
        (push (mark-graf-ts--make-element
               (treesit-node-child ts-node i))
              children))
      (nreverse children))))

(defun mark-graf-ts--parent (node)
  "Get parent of NODE as mark-graf-node."
  (when-let* ((ts-node (mark-graf-node-treesit-node node))
              (parent (treesit-node-parent ts-node)))
    (mark-graf-ts--make-element parent)))

;;; Block Boundary Detection

(defun mark-graf-ts--containing-block (pos)
  "Get the block element containing position POS."
  (when-let ((node (mark-graf-ts--node-at pos)))
    ;; Walk up to find block-level element
    (let ((current node))
      (while (and current
                  (not (mark-graf-ts--block-element-p current)))
        (setq current (treesit-node-parent current)))
      (when current
        (mark-graf-ts--make-element current)))))

(defun mark-graf-ts--block-element-p (ts-node)
  "Return non-nil if TS-NODE is a block-level element."
  (member (treesit-node-type ts-node)
          '("atx_heading" "setext_heading" "paragraph"
            "fenced_code_block" "indented_code_block"
            "block_quote" "list" "list_item"
            "thematic_break" "pipe_table" "html_block")))

(defun mark-graf-ts--containing-block-bounds (start end)
  "Get bounds of block containing region START to END."
  (let ((block-start start)
        (block-end end))
    ;; Expand to block boundaries
    (when-let ((start-block (mark-graf-ts--containing-block start)))
      (setq block-start (min block-start (mark-graf-node-start start-block))))
    (when-let ((end-block (mark-graf-ts--containing-block end)))
      (setq block-end (max block-end (mark-graf-node-end end-block))))
    (cons block-start block-end)))

;;; Line/Region Queries

(defun mark-graf-ts--elements-in-region (start end)
  "Get all elements in region from START to END."
  (let ((elements '()))
    (mark-graf-ts--walk-blocks
     (lambda (node)
       (push node elements))
     start end)
    (nreverse elements)))

(defun mark-graf-ts--elements-on-line (line-num)
  "Get elements on line number LINE-NUM."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-num))
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (mark-graf-ts--elements-in-region start end))))

;;; Inline Element Detection

(defun mark-graf-ts--inline-elements-in (start end)
  "Get inline elements within range START to END."
  (when mark-graf-ts--inline-parser
    (let ((elements '())
          (text (buffer-substring-no-properties start end)))
      ;; Parse the text range for inline elements
      (treesit-parser-set-included-ranges
       mark-graf-ts--inline-parser
       (list (cons start end)))
      ;; Query for inline elements
      (when-let ((root (treesit-parser-root-node mark-graf-ts--inline-parser)))
        (dolist (child (mark-graf-ts--collect-inline-nodes root))
          (push (mark-graf-ts--make-element child) elements)))
      (nreverse elements))))

(defun mark-graf-ts--collect-inline-nodes (node)
  "Recursively collect all inline element nodes from NODE."
  (let ((result '())
        (type (treesit-node-type node)))
    (when (member type '("emphasis" "strong_emphasis" "strikethrough"
                        "code_span" "inline_link" "full_reference_link"
                        "image" "uri_autolink" "email_autolink"))
      (push node result))
    ;; Recurse into children
    (dotimes (i (treesit-node-child-count node))
      (setq result (append result
                           (mark-graf-ts--collect-inline-nodes
                            (treesit-node-child node i)))))
    result))

;;; Fallback Regex-based Parsing

(defconst mark-graf-ts--heading-regex
  "^\\(#\\{1,6\\}\\) +\\(.*\\)"
  "Regex for ATX headings.")

(defconst mark-graf-ts--emphasis-regex
  "\\(?:^\\|[^\\*_]\\)\\(\\*\\([^\\*\n\r]+\\)\\*\\|_\\([^_\n\r]+\\)_\\)"
  "Regex for emphasis (italic). Only matches within a single line.")

(defconst mark-graf-ts--strong-regex
  "\\(?:^\\|[^\\*_]\\)\\(\\*\\*\\([^\\*\n\r]+\\)\\*\\*\\|__\\([^_\n\r]+\\)__\\)"
  "Regex for strong (bold). Only matches within a single line.")

(defconst mark-graf-ts--code-span-regex
  "`\\([^`\n\r]+\\)`"
  "Regex for inline code. Only matches within a single line.")

(defconst mark-graf-ts--image-regex
  "!\\[\\([^]]*\\)\\](\\([^)]+\\))"
  "Regex for inline images.")

(defconst mark-graf-ts--link-regex
  "\\[\\([^]]+\\)\\](\\([^)]+\\))"
  "Regex for inline links.")

(defconst mark-graf-ts--code-block-regex
  "^[ \t]?[ \t]?[ \t]?\\(```\\|~~~\\)\\([a-zA-Z0-9_+-]*\\)?[ \t]*\r?$"
  "Regex for fenced code block start/end.
Allows up to 3 spaces indent per CommonMark spec.
Handles Windows CRLF line endings with \\r?.")

(defun mark-graf-ts--fallback-parse-region (start end)
  "Parse region from START to END using regex fallback."
  (condition-case err
      (let ((elements '())
            (code-block-regions '())    ; Track code block regions to exclude
            (blockquote-regions '()))   ; Track blockquote regions to exclude
        (save-excursion
      ;; FIRST: Find all fenced code blocks to know what regions to skip
      ;; Search for closing fence beyond region boundary if needed
      ;; Note: Use \r? for Windows CRLF
      ;; Important: closing fence must match opening fence type AND indentation
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward "^\\([ \t]*\\)\\(```\\|~~~\\)\\([a-zA-Z0-9_+-]*\\)?[ \t]*\r?$" end t))
        (let* ((block-start (match-beginning 0))
               (indent (match-string 1))      ; Capture the indentation
               (fence-char (match-string 2))  ; "```" or "~~~"
               (lang (match-string 3))
               ;; Build regex requiring SAME fence character for closing
               ;; Allow any leading whitespace (indented code blocks in lists)
               (closing-regex (concat "^[ \t]*" (regexp-quote fence-char) "[ \t]*\r?$"))
               ;; Limit search to reasonable distance (500 lines max)
               (search-limit (save-excursion (forward-line 500) (point))))
          ;; Search for closing fence with same character and compatible indentation
          (when (re-search-forward closing-regex search-limit t)
            (let ((block-end (match-end 0)))
              (push (cons block-start block-end) code-block-regions)
              (push (make-mark-graf-node
                     :type 'code-block
                     :start block-start
                     :end block-end
                     :properties (list :language lang))
                    elements)))))

      ;; SECOND: Find all blockquotes to know what regions to skip for inline elements
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward "^\\(>+\\)[ \t]?" end t))
        (let ((quote-start (match-beginning 0))
              (quote-end (line-end-position)))
          ;; Extend to include consecutive blockquote lines
          (save-excursion
            (forward-line 1)
            (while (and (< (point) end)
                        (looking-at "^>"))
              (setq quote-end (line-end-position))
              (forward-line 1)))
          (push (cons quote-start (min quote-end end)) blockquote-regions)
          (goto-char (min quote-end end))))

      ;; Helper to check if position is inside a code block or blockquote
      (cl-flet ((in-code-block-p (pos)
                  (cl-some (lambda (region)
                            (and (>= pos (car region))
                                 (<= pos (cdr region))))
                          code-block-regions))
                (in-special-block-p (pos)
                  (or (cl-some (lambda (region)
                                (and (>= pos (car region))
                                     (<= pos (cdr region))))
                              code-block-regions)
                      (cl-some (lambda (region)
                                (and (>= pos (car region))
                                     (<= pos (cdr region))))
                              blockquote-regions))))

        ;; Find headings (not in code blocks)
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward mark-graf-ts--heading-regex end t))
          (unless (in-code-block-p (match-beginning 0))
            (push (make-mark-graf-node
                   :type 'heading
                   :start (match-beginning 0)
                   :end (min (match-end 0) end)
                   :level (length (match-string 1)))
                  elements)))

        ;; Find horizontal rules (not in code blocks)
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "^\\(---+\\|\\*\\*\\*+\\|___+\\)[ \t]*$" end t))
          (unless (in-code-block-p (match-beginning 0))
            (push (make-mark-graf-node
                   :type 'hr
                   :start (match-beginning 0)
                   :end (min (match-end 0) end))
                  elements)))

        ;; Find blockquotes (not in code blocks) - group consecutive lines
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "^\\(>+\\)[ \t]?" end t))
          (unless (in-code-block-p (match-beginning 0))
            (let ((quote-start (match-beginning 0))
                  (level (length (match-string 1)))
                  (quote-end (line-end-position)))
              ;; Extend to include consecutive blockquote lines
              (save-excursion
                (forward-line 1)
                (while (and (< (point) end)
                            (looking-at "^>"))
                  (setq quote-end (line-end-position))
                  (forward-line 1)))
              (push (make-mark-graf-node
                     :type 'blockquote
                     :start quote-start
                     :end (min quote-end end)
                     :level level)
                    elements)
              ;; Skip to end of this blockquote
              (goto-char (min quote-end end)))))

        ;; Find unordered list items (not in code blocks)
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "^\\([ \t]*\\)\\([-*+]\\)[ \t]+" end t))
          (unless (in-code-block-p (match-beginning 0))
            (let ((item-start (match-beginning 0))
                  (indent (length (match-string 1))))
              (push (make-mark-graf-node
                     :type 'list-item
                     :start item-start
                     :end (min (line-end-position) end)
                     :level (/ indent 2)
                     :properties (list :ordered nil))
                    elements))))

        ;; Find ordered list items (not in code blocks)
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "^\\([ \t]*\\)\\([0-9]+\\)[.)][ \t]+" end t))
          (unless (in-code-block-p (match-beginning 0))
            (let ((item-start (match-beginning 0))
                  (indent (length (match-string 1)))
                  (num (string-to-number (match-string 2))))
              (push (make-mark-graf-node
                     :type 'list-item
                     :start item-start
                     :end (min (line-end-position) end)
                     :level (/ indent 2)
                     :properties (list :ordered t :number num))
                    elements))))

        ;; Find tables - group consecutive table rows into single table element
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "^|.+|[ \t]*$" end t))
          (unless (in-code-block-p (match-beginning 0))
            (let ((table-start (match-beginning 0))
                  (table-end (match-end 0)))
              ;; Extend to include all consecutive table rows
              (save-excursion
                (forward-line 1)
                (while (and (< (point) end)
                            (looking-at "^|.+|[ \t]*$"))
                  (setq table-end (match-end 0))
                  (forward-line 1)))
              (push (make-mark-graf-node
                     :type 'table
                     :start table-start
                     :end (min table-end end))
                    elements)
              ;; Skip to end of this table
              (goto-char (min table-end end)))))

        ;; Find display math blocks ($$...$$) - two-pass like code blocks
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "^\\$\\$[ \t]*\r?$" end t))
          (let ((block-start (match-beginning 0)))
            (unless (in-code-block-p block-start)
              (if (re-search-forward "^\\$\\$[ \t]*\r?$" end t)
                  (let ((block-end (match-end 0)))
                    (push (make-mark-graf-node
                           :type 'math-block
                           :start block-start
                           :end (min block-end end))
                          elements))
                ;; No closing $$, skip
                (goto-char end)))))

        ;; Find inline math ($...$) - not in code blocks, not display math
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward "\\$\\([^$\n]+\\)\\$" end t))
          (let ((pos (match-beginning 0))
                (mend (match-end 0)))
            (unless (or (in-code-block-p pos)
                        (and (> pos (point-min))
                             (eq (char-before pos) ?$))
                        (and (< mend (point-max))
                             (eq (char-after mend) ?$)))
              (push (make-mark-graf-node
                     :type 'math
                     :start pos
                     :end (min mend end))
                    elements))))

        ;; Find inline elements - ONLY outside code blocks
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward mark-graf-ts--strong-regex end t))
          (unless (in-code-block-p (match-beginning 1))
            (push (make-mark-graf-node
                   :type 'strong
                   :start (match-beginning 1)
                   :end (min (match-end 1) end))
                  elements)))

        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward mark-graf-ts--emphasis-regex end t))
          (unless (in-code-block-p (match-beginning 1))
            (push (make-mark-graf-node
                   :type 'emphasis
                   :start (match-beginning 1)
                   :end (min (match-end 1) end))
                  elements)))

        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward mark-graf-ts--code-span-regex end t))
          (unless (in-special-block-p (match-beginning 0))
            (push (make-mark-graf-node
                   :type 'code-span
                   :start (match-beginning 0)
                   :end (min (match-end 0) end))
                  elements)))

        ;; Find images (before links so links can skip image positions)
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward mark-graf-ts--image-regex end t))
          (unless (in-code-block-p (match-beginning 0))
            (push (make-mark-graf-node
                   :type 'image
                   :start (match-beginning 0)
                   :end (min (match-end 0) end)
                   :properties (list :alt (match-string 1)
                                    :url (match-string 2)))
                  elements)))

        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward mark-graf-ts--link-regex end t))
          (unless (or (in-code-block-p (match-beginning 0))
                      ;; Skip if preceded by ! (that's an image, not a link)
                      (and (> (match-beginning 0) (point-min))
                           (eq (char-before (match-beginning 0)) ?!)))
            (push (make-mark-graf-node
                   :type 'link
                   :start (match-beginning 0)
                   :end (min (match-end 0) end)
                   :properties (list :text (match-string 1)
                                    :url (match-string 2)))
                  elements)))))
      (nreverse elements))
    (error
     (message "mark-graf: Parse error: %s" (error-message-string err))
     nil)))

(provide 'mark-graf-ts)
;;; mark-graf-ts.el ends here

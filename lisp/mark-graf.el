;;; mark-graf.el --- Modern WYSIWYG-style markdown editing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;; Author: Marc Ansset <info@ansset.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: markdown, wp, text
;; URL: https://github.com/hyperZphere/mark-graf

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; mark-graf is a modern WYSIWYG-style markdown editing mode for Emacs 30+.
;; It provides inline WYSIWYG rendering using the text-property-based
;; approach of org-modern.
;;
;; Features:
;; - Inline rendering of markdown using text properties and overlays
;; - Three editing paradigms: line-at-point, block-at-point, hybrid
;; - Full GFM support including tables, task lists, and fenced code blocks
;; - Image and diagram rendering
;; - Built-in HTML export with optional Pandoc integration
;;
;; Usage:
;;   (require 'mark-graf)
;;   ;; Automatically activates for .md files
;;
;; Customization:
;;   M-x customize-group RET mark-graf RET

;;; Code:

(require 'treesit)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Load submodules
(require 'mark-graf-ts)
(require 'mark-graf-mermaid)
(require 'mark-graf-render)
(require 'mark-graf-elements)
(require 'mark-graf-commands)
(require 'mark-graf-export)

;;; Customization Groups

(defgroup mark-graf nil
  "Modern WYSIWYG-style markdown editing."
  :group 'text
  :group 'wp
  :prefix "mark-graf-")

(defgroup mark-graf-faces nil
  "Faces for mark-graf rendering."
  :group 'mark-graf
  :group 'faces)

(defgroup mark-graf-performance nil
  "Performance tuning for mark-graf."
  :group 'mark-graf)

(defgroup mark-graf-code nil
  "Code block settings for mark-graf."
  :group 'mark-graf)

(defgroup mark-graf-media nil
  "Image and media settings for mark-graf."
  :group 'mark-graf)

(defgroup mark-graf-math nil
  "Math rendering settings for mark-graf."
  :group 'mark-graf)

(defgroup mark-graf-export nil
  "Export settings for mark-graf."
  :group 'mark-graf)

;;; Customization Variables

(defcustom mark-graf-edit-style 'line
  "Editing paradigm for revealing raw markdown syntax.
`line'   - Reveal syntax on current line only
`block'  - Reveal syntax for entire containing block
`hybrid' - Keep syntax hidden, reveal on demand"
  :type '(choice (const :tag "Line at point" line)
                 (const :tag "Block at point" block)
                 (const :tag "Hybrid overlay" hybrid))
  :group 'mark-graf)

(defcustom mark-graf-update-delay 'on-leave
  "When to re-render markdown after editing.
`immediate' or 0 - Re-render on every keystroke
Number (e.g., 0.3) - Re-render after idle delay in seconds
`on-leave' - Re-render when cursor leaves element"
  :type '(choice (const :tag "Immediate" immediate)
                 (const :tag "On leave" on-leave)
                 (number :tag "Delay (seconds)"))
  :group 'mark-graf)

(defcustom mark-graf-edit-reveal-delay 0.1
  "Seconds to wait before revealing syntax after cursor movement.
Set to 0 for immediate reveal, higher values reduce flicker."
  :type 'number
  :group 'mark-graf)

(defcustom mark-graf-edit-hide-delay 0.3
  "Seconds to wait before hiding syntax after cursor leaves element.
Set to 0 for immediate hide."
  :type 'number
  :group 'mark-graf)

(defcustom mark-graf-heading-scale '(1.8 1.5 1.3 1.1 1.05 1.0)
  "Height scale factors for heading levels 1-6."
  :type '(list number number number number number number)
  :group 'mark-graf-faces)

(defcustom mark-graf-heading-use-variable-pitch t
  "Whether headings should use variable-pitch font."
  :type 'boolean
  :group 'mark-graf-faces)

(defcustom mark-graf-display-images t
  "Whether to display images inline."
  :type 'boolean
  :group 'mark-graf-media)

(defcustom mark-graf-image-max-width 600
  "Maximum width in pixels for inline images."
  :type 'integer
  :group 'mark-graf-media)

(defcustom mark-graf-image-max-height 400
  "Maximum height in pixels for inline images."
  :type 'integer
  :group 'mark-graf-media)

(defcustom mark-graf-text-width 90
  "Maximum width in characters for rendered text content.
Content will be visually constrained to this width.
Set to nil to use the full window width."
  :type '(choice (integer :tag "Character width")
                 (const :tag "Full window width" nil))
  :group 'mark-graf)

(defcustom mark-graf-left-margin 4
  "Left margin width in characters.
This creates whitespace on the left side of the buffer for better readability."
  :type 'integer
  :group 'mark-graf)

(defcustom mark-graf-code-block-syntax-highlight t
  "Whether to apply syntax highlighting to code blocks.
When enabled, code blocks use Emacs font-lock for the specified language."
  :type 'boolean
  :group 'mark-graf-code)

(defcustom mark-graf-code-block-full-width t
  "Whether code block backgrounds extend to the window edge.
When enabled, the background color extends to the right margin."
  :type 'boolean
  :group 'mark-graf-code)

(defcustom mark-graf-lazy-rendering t
  "Whether to use lazy rendering for off-screen content."
  :type 'boolean
  :group 'mark-graf-performance)

(defcustom mark-graf-large-file-threshold 100000
  "Character count above which large file optimizations activate."
  :type 'integer
  :group 'mark-graf-performance)

;;; Faces

(defface mark-graf-default
  '((t :inherit default))
  "Default face for mark-graf content."
  :group 'mark-graf-faces)

(defface mark-graf-heading-1
  '((t :height 1.8 :weight bold :inherit default))
  "Face for level 1 headings."
  :group 'mark-graf-faces)

(defface mark-graf-heading-2
  '((t :height 1.5 :weight bold :inherit default))
  "Face for level 2 headings."
  :group 'mark-graf-faces)

(defface mark-graf-heading-3
  '((t :height 1.3 :weight bold :inherit default))
  "Face for level 3 headings."
  :group 'mark-graf-faces)

(defface mark-graf-heading-4
  '((t :height 1.1 :weight bold :inherit default))
  "Face for level 4 headings."
  :group 'mark-graf-faces)

(defface mark-graf-heading-5
  '((t :height 1.05 :weight bold :inherit default))
  "Face for level 5 headings."
  :group 'mark-graf-faces)

(defface mark-graf-heading-6
  '((t :height 1.0 :weight bold :inherit default))
  "Face for level 6 headings."
  :group 'mark-graf-faces)

(defface mark-graf-bold
  '((t :weight bold :inherit default))
  "Face for bold/strong text."
  :group 'mark-graf-faces)

(defface mark-graf-italic
  '((t :slant italic))
  "Face for italic/emphasis text."
  :group 'mark-graf-faces)

(defface mark-graf-bold-italic
  '((t :weight bold :slant italic))
  "Face for bold italic text."
  :group 'mark-graf-faces)

(defface mark-graf-strikethrough
  '((t :strike-through t))
  "Face for strikethrough text."
  :group 'mark-graf-faces)

(defface mark-graf-inline-code
  '((((background light))
     :foreground "#c7254e")
    (((background dark))
     :foreground "#e06c75"))
  "Face for inline code spans.
Uses distinct color only, no background to keep clean appearance."
  :group 'mark-graf-faces)

(defface mark-graf-code-block
  '((((background light))
     :background "#f0f0f0"
     :foreground "#383a42"
     :inherit fixed-pitch
     :extend t)
    (((background dark))
     :background "#252530"
     :foreground "#abb2bf"
     :inherit fixed-pitch
     :extend t))
  "Face for code block content.
Includes foreground color to override markdown-mode's inline styling."
  :group 'mark-graf-faces)

(defface mark-graf-code-block-language
  '((((background light))
     :height 0.85
     :foreground "#666666"
     :slant italic)
    (((background dark))
     :height 0.85
     :foreground "#7a7a8a"
     :slant italic))
  "Face for code block language label."
  :group 'mark-graf-faces)

(defface mark-graf-link
  '((t :underline t :inherit link))
  "Face for link text."
  :group 'mark-graf-faces)

(defface mark-graf-link-url
  '((t :foreground "#888888" :height 0.9))
  "Face for link URLs when displayed."
  :group 'mark-graf-faces)

(defface mark-graf-image-alt
  '((t :foreground "#888888" :slant italic))
  "Face for image alt text placeholders."
  :group 'mark-graf-faces)

(defface mark-graf-blockquote
  '((((background light))
     :foreground "#555555"
     :slant italic)
    (((background dark))
     :foreground "#999999"
     :slant italic))
  "Face for blockquote text.
No background is used to avoid issues with visual-line-mode wrapping."
  :group 'mark-graf-faces)

(defface mark-graf-blockquote-marker
  '((t :foreground "#5588cc" :weight bold))
  "Face for blockquote left border marker."
  :group 'mark-graf-faces)

(defface mark-graf-list-bullet
  '((t :foreground "#5588cc"))
  "Face for list bullet characters."
  :group 'mark-graf-faces)

(defface mark-graf-list-number
  '((t :foreground "#5588cc" :weight bold))
  "Face for ordered list numbers."
  :group 'mark-graf-faces)

(defface mark-graf-task-unchecked
  '((t :foreground "#888888"))
  "Face for unchecked task checkboxes."
  :group 'mark-graf-faces)

(defface mark-graf-task-checked
  '((t :foreground "#22aa22"))
  "Face for checked task checkboxes."
  :group 'mark-graf-faces)

(defface mark-graf-task-done-text
  '((t :strike-through t :foreground "#888888"))
  "Face for completed task text."
  :group 'mark-graf-faces)

(defface mark-graf-table
  '((((background light))
     :inherit default
     :background "#e0e0f0")
    (((background dark))
     :inherit default
     :background "#2a2a45"))
  "Face for table data rows."
  :group 'mark-graf-faces)

(defface mark-graf-table-header
  '((((background light))
     :inherit mark-graf-table
     :weight bold)
    (((background dark))
     :inherit mark-graf-table
     :weight bold))
  "Face for table header cells (same background as table, bold text)."
  :group 'mark-graf-faces)

(defface mark-graf-table-border
  '((((background light))
     :inherit default
     :foreground "#888888"
     :background "#d8d8e0")
    (((background dark))
     :inherit default
     :foreground "#666666"
     :background "#202038"))
  "Face for table separator row."
  :group 'mark-graf-faces)

(defface mark-graf-table-cell
  '((t :inherit mark-graf-default))
  "Face for table cell content."
  :group 'mark-graf-faces)

(defface mark-graf-hr
  '((t :foreground "#cccccc"))
  "Face for horizontal rules."
  :group 'mark-graf-faces)

(defface mark-graf-footnote-ref
  '((t :height 0.8 :foreground "#5588cc" :underline t))
  "Face for footnote references."
  :group 'mark-graf-faces)

(defface mark-graf-math
  '((t :foreground "#aa5588"))
  "Face for math expressions."
  :group 'mark-graf-faces)

(defface mark-graf-delimiter
  '((t :foreground "#888888"))
  "Face for visible markdown delimiters."
  :group 'mark-graf-faces)

;; Force-update face attributes that defface won't change on reload
(dolist (face '(mark-graf-table mark-graf-table-border))
  (set-face-attribute face nil :extend nil))
;; Force-update math face in case it was previously defined differently
(set-face-attribute 'mark-graf-math nil
                    :foreground "#aa5588"
                    :inherit nil)

;;; Internal Variables

(defvar-local mark-graf--rendering-enabled t
  "Whether rendering is currently enabled in this buffer.")

(defvar-local mark-graf--current-element nil
  "The element currently being edited (revealed).")

(defvar-local mark-graf--reveal-timer nil
  "Timer for delayed syntax reveal.")

(defvar-local mark-graf--hide-timer nil
  "Timer for delayed syntax hide.")

(defvar-local mark-graf--update-timer nil
  "Timer for delayed re-rendering after edits.")

(defvar-local mark-graf--dirty-regions nil
  "List of (START . END) regions needing re-render.")

(defvar-local mark-graf--overlay-pool nil
  "Pool of reusable overlays.")

(defvar-local mark-graf--last-point nil
  "Last known cursor position for movement detection.")

(defvar-local mark-graf--revealed-line nil
  "Line number currently revealed in `line' edit-style.")

(defvar-local mark-graf--revealed-code-block nil
  "Cons (START . END) of the currently block-revealed code block, or nil.
When cursor is inside a fenced code block in `line' edit-style,
the entire block is revealed instead of just one line.")

(defvar-local mark-graf--revealed-math-block nil
  "Cons (START . END) of the currently block-revealed display math block, or nil.
When cursor is inside a display math block in `line' edit-style,
the entire block is revealed instead of just one line.")

(defvar-local mark-graf--revealed-table nil
  "Cons (START . END) of the table the cursor is currently inside, or nil.
Tables need full-region re-rendering because per-line rendering via
tree-sitter cannot find the pipe_table node from a single-line range.")

(defvar-local mark-graf--code-edit-buffer nil
  "Indirect buffer currently editing a code block, or nil.")

(defvar-local mark-graf--full-render-done-p nil
  "Non-nil when the entire buffer has been rendered at least once.
Used to skip destructive scroll-triggered re-renders for small files.")

(defvar-local mark-graf--initial-render-p nil
  "Non-nil when buffer has just been rendered and cursor line is still rendered.
Design decision: on initial file open, all lines including the cursor line
are rendered for a clean appearance.  The reveal-on-edit behavior only kicks
in when the cursor actually moves to a different line.")

;;; Mode Definition

(defvar mark-graf-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Style insertion (C-c C-s prefix)
    (define-key map (kbd "C-c C-s b") #'mark-graf-insert-bold)
    (define-key map (kbd "C-c C-s i") #'mark-graf-insert-italic)
    (define-key map (kbd "C-c C-s c") #'mark-graf-insert-code)
    (define-key map (kbd "C-c C-s s") #'mark-graf-insert-strike)
    (define-key map (kbd "C-c C-s q") #'mark-graf-insert-blockquote)
    (define-key map (kbd "C-c C-s p") #'mark-graf-insert-code-block)
    (define-key map (kbd "C-c C-s k") #'mark-graf-insert-kbd)

    ;; Headings (C-c C-t prefix)
    (define-key map (kbd "C-c C-t h") #'mark-graf-insert-heading)
    (define-key map (kbd "C-c C-t 1") #'mark-graf-insert-heading-1)
    (define-key map (kbd "C-c C-t 2") #'mark-graf-insert-heading-2)
    (define-key map (kbd "C-c C-t 3") #'mark-graf-insert-heading-3)
    (define-key map (kbd "C-c C-t 4") #'mark-graf-insert-heading-4)
    (define-key map (kbd "C-c C-t 5") #'mark-graf-insert-heading-5)
    (define-key map (kbd "C-c C-t 6") #'mark-graf-insert-heading-6)
    (define-key map (kbd "C-c C-t !") #'mark-graf-promote-heading)
    (define-key map (kbd "C-c C-t @") #'mark-graf-demote-heading)

    ;; Links and images
    (define-key map (kbd "C-c C-l") #'mark-graf-insert-link)
    (define-key map (kbd "C-c C-i") #'mark-graf-insert-image)
    (define-key map (kbd "C-c C-x C-i") #'mark-graf-toggle-images)

    ;; Follow link at point
    (define-key map (kbd "C-c C-o") #'mark-graf-follow-link-at-point)

    ;; Navigation
    (define-key map (kbd "C-c C-n") #'mark-graf-next-heading)
    (define-key map (kbd "C-c C-p") #'mark-graf-prev-heading)
    (define-key map (kbd "C-c C-f") #'mark-graf-next-heading-same-level)
    (define-key map (kbd "C-c C-b") #'mark-graf-prev-heading-same-level)
    (define-key map (kbd "C-c C-u") #'mark-graf-up-heading)
    (define-key map (kbd "TAB") #'mark-graf-tab)
    (define-key map (kbd "<backtab>") #'mark-graf-backtab)

    ;; Lists
    (define-key map (kbd "M-RET") #'mark-graf-insert-list-item)
    (define-key map (kbd "C-c <up>") #'mark-graf-move-item-up)
    (define-key map (kbd "C-c <down>") #'mark-graf-move-item-down)
    (define-key map (kbd "C-c <left>") #'mark-graf-promote-item)
    (define-key map (kbd "C-c <right>") #'mark-graf-demote-item)
    (define-key map (kbd "<f8>") #'mark-graf-toggle-checkbox)
    (define-key map (kbd "C-c C-x C-b") #'mark-graf-toggle-checkbox)

    ;; Tables
    (define-key map (kbd "C-c |") #'mark-graf-insert-table)
    (define-key map (kbd "C-c C-c ^") #'mark-graf-table-sort)

    ;; View toggles
    (define-key map (kbd "C-c C-v s") #'mark-graf-show-source)
    (define-key map (kbd "C-c C-v r") #'mark-graf-show-rendered)
    (define-key map (kbd "C-c C-v v") #'mark-graf-toggle-view)
    (define-key map (kbd "C-c C-v e") #'mark-graf-toggle-element-at-point)
    (define-key map (kbd "C-c C-x C-v") #'mark-graf-toggle-element-at-point)

    ;; Code block editing
    (define-key map (kbd "C-c '") #'mark-graf-edit-code-block)

    ;; Export
    (define-key map (kbd "C-c C-e h") #'mark-graf-export-html)
    (define-key map (kbd "C-c C-e p") #'mark-graf-export-pdf)
    (define-key map (kbd "C-c C-e d") #'mark-graf-export-docx)
    (define-key map (kbd "C-c C-c p") #'mark-graf-preview-html)

    map)
  "Keymap for `mark-graf-mode'.")

(defun mark-graf--setup-buffer ()
  "Set up the current buffer for mark-graf-mode."
  (condition-case err
      (progn
        ;; Disable font-lock and remove any face properties left by
        ;; a previous major mode (e.g. markdown-mode)
        (font-lock-mode -1)
        (with-silent-modifications
          (remove-text-properties (point-min) (point-max) '(face nil)))

        ;; Ensure tree-sitter is available
        (mark-graf-ts--ensure-grammar)

        ;; Initialize parser
        (mark-graf-ts--init)

        ;; Enable line numbers
        (display-line-numbers-mode 1)

        ;; Set up left indentation using line-prefix
        (let ((indent-str (propertize (make-string mark-graf-left-margin ?\s)
                                      'face 'default)))
          (setq-local line-prefix indent-str)
          (setq-local wrap-prefix indent-str))

        ;; Set up text width for readable line lengths
        (when mark-graf-text-width
          (setq-local fill-column mark-graf-text-width)
          (visual-line-mode 1)
          ;; Use visual-fill-column if available for proper width limiting
          (if (fboundp 'visual-fill-column-mode)
              (progn
                (setq-local visual-fill-column-width mark-graf-text-width)
                (setq-local visual-fill-column-center-text nil)
                (visual-fill-column-mode 1))
            ;; Fallback: use window margins to constrain width
            (mark-graf--apply-text-width)
            (add-hook 'window-size-change-functions #'mark-graf--on-window-size-change)))

        ;; Set up hooks
        (add-hook 'post-command-hook #'mark-graf--post-command nil t)
        (add-hook 'after-change-functions #'mark-graf--after-change nil t)
        (add-hook 'window-scroll-functions #'mark-graf--on-scroll nil t)
        ;; Clean up when switching to another major mode
        (add-hook 'change-major-mode-hook #'mark-graf--teardown-buffer nil t)

        ;; Initialize rendering
        (mark-graf-render--init)

        ;; Render buffer - for small files render all, otherwise defer visible region
        (if (< (buffer-size) mark-graf-large-file-threshold)
            ;; Small file: render entire buffer immediately
            (progn
              (mark-graf-render--render-region (point-min) (point-max))
              (setq mark-graf--full-render-done-p t)
              ;; Track cursor line but keep it rendered on initial open.
              (when (eq mark-graf-edit-style 'line)
                (setq mark-graf--revealed-line (line-number-at-pos (point)))
                (setq mark-graf--initial-render-p t))
              (redisplay t))
          ;; Large file: defer rendering of visible region
          (let ((buf (current-buffer)))
            (run-with-timer 0 nil
                            (lambda ()
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (mark-graf--render-visible)
                                  ;; Track cursor line but keep it rendered (same as small file path)
                                  (when (eq mark-graf-edit-style 'line)
                                    (setq mark-graf--revealed-line (line-number-at-pos (point)))
                                    (setq mark-graf--initial-render-p t))
                                  (redisplay t)))))))

        ;; Set up imenu
        (setq-local imenu-create-index-function #'mark-graf-imenu-create-index))
    (error
     (message "mark-graf: Setup error (%s)" (error-message-string err)))))

(defun mark-graf--apply-text-width ()
  "Apply text width constraint using window margins."
  (when (and mark-graf-text-width (get-buffer-window))
    (let* ((win (get-buffer-window))
           (width (window-total-width win))
           (text-width (+ mark-graf-text-width mark-graf-left-margin))
           (right-margin (max 0 (- width text-width))))
      (set-window-margins win mark-graf-left-margin right-margin))))

(defun mark-graf--on-window-size-change (frame)
  "Update text width when window size changes."
  (dolist (win (window-list frame))
    (with-current-buffer (window-buffer win)
      (when (eq major-mode 'mark-graf-mode)
        (mark-graf--apply-text-width)))))

(defun mark-graf--teardown-buffer ()
  "Clean up mark-graf-mode resources from buffer."
  ;; Cancel timers
  (when mark-graf--reveal-timer
    (cancel-timer mark-graf--reveal-timer))
  (when mark-graf--hide-timer
    (cancel-timer mark-graf--hide-timer))
  (when mark-graf--update-timer
    (cancel-timer mark-graf--update-timer))

  ;; Remove hooks
  (remove-hook 'post-command-hook #'mark-graf--post-command t)
  (remove-hook 'after-change-functions #'mark-graf--after-change t)
  (remove-hook 'window-scroll-functions #'mark-graf--on-scroll t)
  (remove-hook 'change-major-mode-hook #'mark-graf--teardown-buffer t)
  (remove-hook 'window-size-change-functions #'mark-graf--on-window-size-change)

  ;; Reset state
  (setq mark-graf--full-render-done-p nil)
  (setq mark-graf--revealed-code-block nil)
  (setq mark-graf--revealed-math-block nil)
  (setq mark-graf--revealed-table nil)
  (when (and mark-graf--code-edit-buffer
             (buffer-live-p mark-graf--code-edit-buffer))
    (kill-buffer mark-graf--code-edit-buffer))
  (setq mark-graf--code-edit-buffer nil)

  ;; Disable visual modes
  (visual-line-mode -1)
  (when (fboundp 'visual-fill-column-mode)
    (visual-fill-column-mode -1))

  ;; Reset line prefix
  (setq-local line-prefix nil)
  (setq-local wrap-prefix nil)

  ;; Reset window margins
  (when (get-buffer-window)
    (set-window-margins (get-buffer-window) nil nil))

  ;; Clear overlays
  (mark-graf-render--clear-all))

;;;###autoload
(define-derived-mode mark-graf-mode text-mode "MG"
  "Major mode for editing Markdown with inline WYSIWYG rendering.

mark-graf renders markdown content inline using text properties and overlays,
providing a seamless reading/writing experience.

\\{mark-graf-mode-map}"
  :group 'mark-graf
  :syntax-table nil

  ;; Mode setup
  (mark-graf--setup-buffer)

  ;; Mode line
  (setq mode-name
        '(:eval (if mark-graf--rendering-enabled "MG" "MG[src]"))))

;; Auto-mode disabled by default - users can enable with:
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . mark-graf-mode))
;; Or call M-x mark-graf-mode manually in a markdown buffer

;;; Fenced Code Block Helpers

(defun mark-graf--fenced-code-block-at (pos)
  "Return (START . END) if POS is inside a fenced code block, nil otherwise.
Scans from buffer start, matching opening/closing fence pairs via regex."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((fence-re "^[ \t]*\\(```\\|~~~\\)"))
        (catch 'found
          (while (re-search-forward fence-re nil t)
            (let ((open-start (line-beginning-position))
                  (fence-char (match-string 1)))
              (forward-line 1)
              (let ((close-re (concat "^[ \t]*" (regexp-quote fence-char) "[ \t]*$")))
                (if (re-search-forward close-re nil t)
                    (let ((close-end (line-end-position)))
                      ;; Include trailing newline if present
                      (when (< close-end (point-max))
                        (setq close-end (1+ close-end)))
                      (when (and (>= pos open-start) (<= pos close-end))
                        (throw 'found (cons open-start close-end))))
                  ;; No closing fence found, skip to end
                  (goto-char (point-max))))))
          nil)))))

(defun mark-graf--fenced-code-block-content-at (pos)
  "Return plist describing the fenced code block at POS, or nil.
Plist keys: :block-start :block-end :content-start :content-end :language."
  (when-let ((block (mark-graf--fenced-code-block-at pos)))
    (save-match-data
      (save-excursion
        (goto-char (car block))
        (when (looking-at "^[ \t]*\\(```\\|~~~\\)\\([a-zA-Z0-9_+-]*\\)?[ \t]*\r?$")
          (let* ((language (match-string-no-properties 2))
                 (content-start (1+ (line-end-position)))
                 (content-end (save-excursion
                                (goto-char (cdr block))
                                (if (re-search-backward
                                     "^[ \t]*\\(```\\|~~~\\)[ \t]*\r?$"
                                     content-start t)
                                    (match-beginning 0)
                                  (cdr block)))))
            (list :block-start (car block)
                  :block-end (cdr block)
                  :content-start content-start
                  :content-end content-end
                  :language (if (and language (not (string-empty-p language)))
                                language
                              nil))))))))

(defun mark-graf--display-math-block-at (pos)
  "Return (START . END) if POS is inside a display math block, nil otherwise.
Display math blocks are delimited by $$ on their own lines."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward "^\\$\\$[ \t]*$" nil t)
          (let ((open-start (line-beginning-position)))
            (forward-line 1)
            (when (re-search-forward "^\\$\\$[ \t]*$" nil t)
              (let ((close-end (line-end-position)))
                ;; Include trailing newline if present
                (when (< close-end (point-max))
                  (setq close-end (1+ close-end)))
                (when (and (>= pos open-start) (<= pos close-end))
                  (throw 'found (cons open-start close-end)))))))
        nil))))

(defun mark-graf--table-at (pos)
  "Return (START . END) if POS is inside a markdown table, nil otherwise.
Scans backward and forward from POS looking for consecutive pipe-table lines."
  (save-match-data
    (save-excursion
      (goto-char pos)
      (beginning-of-line)
      ;; Check if current line is a table row
      (when (looking-at "^[ \t]*|.+|[ \t]*$")
        ;; Scan backward for the start of the table
        (let ((table-start (line-beginning-position)))
          (save-excursion
            (while (and (> (point) (point-min))
                        (progn (forward-line -1)
                               (looking-at "^[ \t]*|.+|[ \t]*$")))
              (setq table-start (line-beginning-position))))
          ;; Scan forward for the end of the table
          (let ((table-end (line-end-position)))
            (save-excursion
              (while (and (progn (forward-line 1)
                                 (not (eobp)))
                          (looking-at "^[ \t]*|.+|[ \t]*$"))
                (setq table-end (line-end-position))))
            (cons table-start table-end)))))))

;;; Core Functions

(defun mark-graf--post-command ()
  "Handle post-command processing for cursor movement."
  (when mark-graf--rendering-enabled
    (if (eq mark-graf-edit-style 'line)
        (mark-graf--update-revealed-line)
      ;; Block/hybrid mode
      (let ((current-pos (point)))
        (unless (eq current-pos mark-graf--last-point)
          (let ((old-element (when mark-graf--last-point
                               (mark-graf-ts--element-at mark-graf--last-point)))
                (new-element (mark-graf-ts--element-at current-pos)))
            (unless (mark-graf--same-element-p old-element new-element)
              (when old-element
                (mark-graf--schedule-hide old-element))
              (when new-element
                (mark-graf--schedule-reveal new-element))))
          (setq mark-graf--last-point current-pos))))))

(defun mark-graf--point-on-link-overlay-p ()
  "Return non-nil if point is on a rendered link overlay."
  (cl-some (lambda (ov)
             (and (overlay-get ov 'mark-graf)
                  (overlay-get ov 'mark-graf-url)))
           (overlays-at (point))))

(defun mark-graf--leave-special-block ()
  "Re-render any previously revealed special block (code, math, table).
Clears the block tracking variables and nulls `mark-graf--revealed-line'
so that the caller does not attempt a per-line render-line that would
destroy the freshly created overlays."
  (when mark-graf--revealed-code-block
    (mark-graf-render--render-region
     (car mark-graf--revealed-code-block)
     (cdr mark-graf--revealed-code-block))
    (setq mark-graf--revealed-code-block nil)
    (setq mark-graf--revealed-line nil))
  (when mark-graf--revealed-math-block
    (mark-graf-render--render-region
     (car mark-graf--revealed-math-block)
     (cdr mark-graf--revealed-math-block))
    (setq mark-graf--revealed-math-block nil)
    (setq mark-graf--revealed-line nil))
  (when mark-graf--revealed-table
    (mark-graf-render--render-region
     (car mark-graf--revealed-table)
     (cdr mark-graf--revealed-table))
    (setq mark-graf--revealed-table nil)
    (setq mark-graf--revealed-line nil)))

(defun mark-graf--update-revealed-line ()
  "Ensure exactly the current line is unrendered and all others are rendered.
On initial file open, all lines stay rendered until the cursor moves
to a different line (see `mark-graf--initial-render-p').
Suppresses unrendering when point lands on a link overlay, so that
mouse clicks on rendered links work correctly.
When cursor is inside a fenced code block, display math block, or table,
special handling ensures the full block is re-rendered correctly."
  (let ((cur-line (line-number-at-pos (point)))
        (code-block (mark-graf--fenced-code-block-at (point)))
        (math-block nil)
        (table nil))
    ;; Detect block type at point (check in priority order)
    (unless code-block
      (setq math-block (mark-graf--display-math-block-at (point))))
    (unless (or code-block math-block)
      (setq table (mark-graf--table-at (point))))
    (cond
     ;; Entering or staying in a code block
     (code-block
      ;; Leave any other special block
      (when mark-graf--revealed-math-block
        (mark-graf-render--render-region
         (car mark-graf--revealed-math-block)
         (cdr mark-graf--revealed-math-block))
        (setq mark-graf--revealed-math-block nil))
      (when mark-graf--revealed-table
        (mark-graf-render--render-region
         (car mark-graf--revealed-table)
         (cdr mark-graf--revealed-table))
        (setq mark-graf--revealed-table nil))
      (let ((old-block mark-graf--revealed-code-block))
        (cond
         ;; Same code block - just update revealed line tracking
         ((equal old-block code-block)
          (unless (eq cur-line mark-graf--revealed-line)
            (setq mark-graf--revealed-line cur-line)))
         ;; Different or new code block
         (t
          (setq mark-graf--initial-render-p nil)
          ;; Re-render old code block if leaving one
          (when old-block
            (mark-graf-render--render-region (car old-block) (cdr old-block)))
          ;; Re-render old single line if leaving line mode
          (when (and (not old-block) mark-graf--revealed-line)
            (mark-graf-render--render-line mark-graf--revealed-line))
          ;; Reveal entire new code block
          (setq mark-graf--revealed-code-block code-block)
          (setq mark-graf--revealed-line cur-line)
          (mark-graf-render--unrender-region (car code-block) (cdr code-block))))))

     ;; Entering or staying in a display math block
     (math-block
      ;; Leave any other special block
      (when mark-graf--revealed-code-block
        (mark-graf-render--render-region
         (car mark-graf--revealed-code-block)
         (cdr mark-graf--revealed-code-block))
        (setq mark-graf--revealed-code-block nil))
      (when mark-graf--revealed-table
        (mark-graf-render--render-region
         (car mark-graf--revealed-table)
         (cdr mark-graf--revealed-table))
        (setq mark-graf--revealed-table nil))
      (let ((old-block mark-graf--revealed-math-block))
        (cond
         ;; Same math block - just update revealed line tracking
         ((equal old-block math-block)
          (unless (eq cur-line mark-graf--revealed-line)
            (setq mark-graf--revealed-line cur-line)))
         ;; Different or new math block
         (t
          (setq mark-graf--initial-render-p nil)
          ;; Re-render old math block if leaving one
          (when old-block
            (mark-graf-render--render-region (car old-block) (cdr old-block)))
          ;; Re-render old single line if leaving line mode
          (when (and (not old-block) mark-graf--revealed-line)
            (mark-graf-render--render-line mark-graf--revealed-line))
          ;; Reveal entire math block
          (setq mark-graf--revealed-math-block math-block)
          (setq mark-graf--revealed-line cur-line)
          (mark-graf-render--unrender-region (car math-block) (cdr math-block))))))

     ;; Entering or staying in a table
     (table
      ;; Leave any other special block
      (when mark-graf--revealed-code-block
        (mark-graf-render--render-region
         (car mark-graf--revealed-code-block)
         (cdr mark-graf--revealed-code-block))
        (setq mark-graf--revealed-code-block nil))
      (when mark-graf--revealed-math-block
        (mark-graf-render--render-region
         (car mark-graf--revealed-math-block)
         (cdr mark-graf--revealed-math-block))
        (setq mark-graf--revealed-math-block nil))
      (let ((old-table mark-graf--revealed-table))
        (cond
         ;; Same table, same line - nothing to do
         ((and (equal old-table table) (eq cur-line mark-graf--revealed-line))
          nil)
         ;; Same table, different line - re-render full table then reveal new line
         ((equal old-table table)
          (mark-graf-render--render-region (car table) (cdr table))
          (setq mark-graf--revealed-line cur-line)
          (mark-graf-render--unrender-line cur-line))
         ;; New or different table
         (t
          (setq mark-graf--initial-render-p nil)
          ;; Re-render old table if leaving one
          (when old-table
            (mark-graf-render--render-region (car old-table) (cdr old-table)))
          ;; Re-render old single line if not from a table
          (when (and (not old-table) mark-graf--revealed-line)
            (mark-graf-render--render-line mark-graf--revealed-line))
          ;; Track new table and reveal current line
          (setq mark-graf--revealed-table table)
          (setq mark-graf--revealed-line cur-line)
          (mark-graf-render--unrender-line cur-line)))))

     ;; Not in any special block
     (t
      ;; Re-render any previously revealed special blocks
      (mark-graf--leave-special-block)
      ;; Standard line reveal behavior
      (unless (eq cur-line mark-graf--revealed-line)
        (unless (mark-graf--point-on-link-overlay-p)
          (setq mark-graf--initial-render-p nil)
          (let ((old-line mark-graf--revealed-line))
            (setq mark-graf--revealed-line cur-line)
            (when old-line
              (mark-graf-render--render-line old-line)))
          (mark-graf-render--unrender-line cur-line)))))))

(defun mark-graf--same-element-p (elem1 elem2)
  "Return non-nil if ELEM1 and ELEM2 are the same element."
  (and elem1 elem2
       (eq (mark-graf-node-start elem1) (mark-graf-node-start elem2))
       (eq (mark-graf-node-end elem1) (mark-graf-node-end elem2))))

(defun mark-graf--schedule-reveal (element)
  "Schedule ELEMENT to be revealed after delay."
  (when mark-graf--reveal-timer
    (cancel-timer mark-graf--reveal-timer))
  (if (zerop mark-graf-edit-reveal-delay)
      (mark-graf--reveal-element element)
    (setq mark-graf--reveal-timer
          (run-with-timer mark-graf-edit-reveal-delay nil
                          #'mark-graf--reveal-element element))))

(defun mark-graf--schedule-hide (element)
  "Schedule ELEMENT to be hidden (re-rendered) after delay."
  (when mark-graf--hide-timer
    (cancel-timer mark-graf--hide-timer))
  (if (zerop mark-graf-edit-hide-delay)
      (mark-graf--hide-element element)
    (setq mark-graf--hide-timer
          (run-with-timer mark-graf-edit-hide-delay nil
                          #'mark-graf--hide-element element))))

(defun mark-graf--reveal-element (element)
  "Reveal raw markdown for ELEMENT (remove rendering)."
  (when (buffer-live-p (current-buffer))
    (setq mark-graf--current-element element)
    (pcase mark-graf-edit-style
      ('block
       (mark-graf-render--unrender-region
        (mark-graf-node-start element)
        (mark-graf-node-end element)))
      ('hybrid
       nil))))

(defun mark-graf--hide-element (element)
  "Hide raw markdown for ELEMENT (re-render it)."
  (when (buffer-live-p (current-buffer))
    (when (eq element mark-graf--current-element)
      (setq mark-graf--current-element nil))
    (pcase mark-graf-edit-style
      ('block
       (mark-graf-render--render-region
        (mark-graf-node-start element)
        (mark-graf-node-end element)))
      ('hybrid
       nil))))

(defun mark-graf--after-change (start end _old-len)
  "Handle buffer modification from START to END with _OLD-LEN removed."
  (when mark-graf--rendering-enabled
    ;; Mark region as dirty
    (let ((block-bounds (mark-graf-ts--containing-block-bounds start end)))
      (push block-bounds mark-graf--dirty-regions))
    ;; Schedule update
    (mark-graf--schedule-update)))

(defun mark-graf--schedule-update ()
  "Schedule deferred update after modifications."
  (when mark-graf--update-timer
    (cancel-timer mark-graf--update-timer))
  (let ((delay (pcase mark-graf-update-delay
                 ('immediate 0)
                 ('on-leave nil)
                 ((pred numberp) mark-graf-update-delay)
                 (_ 0.3))))
    (when delay
      (setq mark-graf--update-timer
            (run-with-timer delay nil
                            #'mark-graf--process-dirty-regions)))))

(defun mark-graf--process-dirty-regions ()
  "Process pending dirty regions and re-render."
  (when (buffer-live-p (current-buffer))
    (dolist (region mark-graf--dirty-regions)
      (when region
        (mark-graf-render--render-region (car region) (cdr region))))
    (setq mark-graf--dirty-regions nil)
    ;; Re-assert revealed state (rendering may have covered it)
    (when (and (eq mark-graf-edit-style 'line)
               (not mark-graf--initial-render-p))
      (cond
       (mark-graf--revealed-code-block
        (mark-graf-render--unrender-region
         (car mark-graf--revealed-code-block)
         (cdr mark-graf--revealed-code-block)))
       (mark-graf--revealed-math-block
        (mark-graf-render--unrender-region
         (car mark-graf--revealed-math-block)
         (cdr mark-graf--revealed-math-block)))
       ;; For tables, only unrender the current line (table stays rendered)
       ((and mark-graf--revealed-table mark-graf--revealed-line)
        (mark-graf-render--unrender-line mark-graf--revealed-line))
       (mark-graf--revealed-line
        (mark-graf-render--unrender-line mark-graf--revealed-line))))))

(defun mark-graf--render-visible ()
  "Render only the currently visible region."
  (let* ((win (get-buffer-window (current-buffer)))
         (start (if win (window-start win) (point-min)))
         (end (if win (window-end win t) (min (point-max) (+ (point-min) 10000))))
         (margin (if mark-graf-lazy-rendering
                     (* 50 (frame-char-height))
                   0)))
    ;; Ensure we have reasonable bounds
    (when (or (null start) (null end) (<= end start))
      (setq start (point-min)
            end (min (point-max) (+ (point-min) 10000))))
    (mark-graf-render--render-region
     (max (point-min) (- start margin))
     (min (point-max) (+ end margin)))))

(defun mark-graf--on-scroll (window start)
  "Handle scroll event for WINDOW starting at START."
  (when mark-graf--rendering-enabled
    ;; Only call ensure-rendered for lazy-rendered (large) files.
    ;; For fully-rendered small files, this is unnecessary and harmful:
    ;; ensure-rendered → render-region → clear-region destroys existing
    ;; overlays, and the re-render may fail to recreate all of them.
    (unless mark-graf--full-render-done-p
      (let ((end (window-end window t)))
        (mark-graf-render--ensure-rendered start end)))
    ;; Re-assert revealed state
    (when (and (eq mark-graf-edit-style 'line)
               (not mark-graf--initial-render-p))
      (cond
       (mark-graf--revealed-code-block
        (mark-graf-render--unrender-region
         (car mark-graf--revealed-code-block)
         (cdr mark-graf--revealed-code-block)))
       (mark-graf--revealed-math-block
        (mark-graf-render--unrender-region
         (car mark-graf--revealed-math-block)
         (cdr mark-graf--revealed-math-block)))
       ;; For tables, only unrender the current line (table stays rendered)
       ((and mark-graf--revealed-table mark-graf--revealed-line)
        (mark-graf-render--unrender-line mark-graf--revealed-line))
       (mark-graf--revealed-line
        (mark-graf-render--unrender-line mark-graf--revealed-line))))
    ;; Schedule deferred redisplay to fix artifacts after large scrolls
    ;; (e.g. page-down) where overlays with display properties may not
    ;; redisplay correctly on the first pass
    (run-with-timer 0 nil
                    (lambda ()
                      (when (window-live-p window)
                        (redisplay))))))

(defun mark-graf--large-file-p ()
  "Return non-nil if current buffer is considered large."
  (> (buffer-size) mark-graf-large-file-threshold))

;;; Imenu Support

(defun mark-graf-imenu-create-index ()
  "Create Imenu index for current markdown buffer."
  (let ((headings '()))
    (mark-graf-ts--walk-headings
     (lambda (node)
       (let* ((level (mark-graf-node-level node))
              (text (mark-graf-ts--heading-text node))
              (prefix (make-string level ?*)))
         (push (cons (format "%s %s" prefix text)
                     (mark-graf-node-start node))
               headings))))
    (nreverse headings)))

(provide 'mark-graf)
;;; mark-graf.el ends here

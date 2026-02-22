;;; mark-graf-commands.el --- Commands for mark-graf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;; This file is part of mark-graf.

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

;; Interactive commands for mark-graf-mode.
;; Includes insertion, navigation, and manipulation commands.

;;; Code:

(require 'cl-lib)

;; Functions defined in other mark-graf files
(declare-function mark-graf-in-heading-p "mark-graf-elements")
(declare-function mark-graf-in-table-p "mark-graf-elements")
(declare-function mark-graf-in-list-p "mark-graf-elements")
(declare-function mark-graf-table-bounds "mark-graf-elements")
(declare-function mark-graf-table-column-at-point "mark-graf-elements")
(declare-function mark-graf-list-item-bounds "mark-graf-elements")
(declare-function mark-graf-list-marker-at-point "mark-graf-elements")
(declare-function mark-graf-heading-level-at-point "mark-graf-elements")
(declare-function mark-graf--at-line-start-p "mark-graf-elements")
(declare-function mark-graf--ensure-blank-line-before "mark-graf-elements")
(declare-function mark-graf--toggle-markup "mark-graf-elements")
(declare-function mark-graf--wrap-region-or-insert "mark-graf-elements")

;; Variables defined in mark-graf.el
(defvar mark-graf-display-images)
(defvar mark-graf--rendering-enabled)
(defvar mark-graf--full-render-done-p)
(defvar mark-graf--code-edit-buffer)

;; Functions defined in mark-graf.el
(declare-function mark-graf--fenced-code-block-content-at "mark-graf")

;; Functions defined in mark-graf-render.el
(declare-function mark-graf-render--render-region "mark-graf-render")
(declare-function mark-graf-render--clear-all "mark-graf-render")
(declare-function mark-graf-render--unrender-region "mark-graf-render")
(declare-function mark-graf-render--language-to-mode "mark-graf-render")
(declare-function mark-graf-render--follow-link "mark-graf-render")

;; Functions defined in mark-graf-ts.el
(declare-function mark-graf-ts--element-at "mark-graf-ts")
(declare-function mark-graf-node-start "mark-graf-ts")
(declare-function mark-graf-node-end "mark-graf-ts")

;; Functions from outline.el (may not be loaded)
(declare-function outline-toggle-children "outline")
(declare-function outline-hide-body "outline")
(declare-function outline-hide-sublevels "outline")
(declare-function outline-show-all "outline")

;;; Style Insertion Commands

;;;###autoload
(defun mark-graf-insert-bold ()
  "Insert bold markers or toggle bold on region/word."
  (interactive)
  (mark-graf--toggle-markup "**" "**"))

;;;###autoload
(defun mark-graf-insert-italic ()
  "Insert italic markers or toggle italic on region/word."
  (interactive)
  (mark-graf--toggle-markup "*" "*"))

;;;###autoload
(defun mark-graf-insert-code ()
  "Insert inline code markers or toggle on region/word."
  (interactive)
  (mark-graf--toggle-markup "`" "`"))

;;;###autoload
(defun mark-graf-insert-strike ()
  "Insert strikethrough markers or toggle on region/word."
  (interactive)
  (mark-graf--toggle-markup "~~" "~~"))

;;;###autoload
(defun mark-graf-insert-kbd ()
  "Insert <kbd> tags or wrap region."
  (interactive)
  (mark-graf--wrap-region-or-insert "<kbd>" "</kbd>"))

;;;###autoload
(defun mark-graf-insert-blockquote ()
  "Insert blockquote markers on current line or region."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (unless (looking-at "^>")
              (insert "> ")
              (setq end (+ end 2)))
            (forward-line 1))))
    (beginning-of-line)
    (if (looking-at "^>+ ?")
        ;; Add another level - preserve relative position
        (save-excursion
          (insert ">"))
      ;; Start new blockquote - move point after markers
      (insert "> "))))

;;;###autoload
(defun mark-graf-insert-code-block (&optional language)
  "Insert a fenced code block with optional LANGUAGE."
  (interactive
   (list (read-string "Language (optional): ")))
  (mark-graf--ensure-blank-line-before)
  (let ((lang (or language "")))
    (insert (format "```%s\n\n```" lang))
    (forward-line -1)))

;;; Heading Commands

;;;###autoload
(defun mark-graf-insert-heading (&optional level)
  "Insert a heading at LEVEL (prompts if not specified)."
  (interactive
   (list (read-number "Heading level (1-6): " 2)))
  (let ((lvl (max 1 (min 6 (or level 2)))))
    (if (mark-graf--at-line-start-p)
        (progn
          (beginning-of-line)
          ;; Remove existing heading markers if any
          (when (looking-at "^#+ *")
            (delete-region (match-beginning 0) (match-end 0)))
          (insert (make-string lvl ?#) " "))
      ;; Not at line start - insert new line
      (end-of-line)
      (insert "\n\n" (make-string lvl ?#) " "))))

;;;###autoload
(defun mark-graf-insert-heading-1 ()
  "Insert a level 1 heading."
  (interactive)
  (mark-graf-insert-heading 1))

;;;###autoload
(defun mark-graf-insert-heading-2 ()
  "Insert a level 2 heading."
  (interactive)
  (mark-graf-insert-heading 2))

;;;###autoload
(defun mark-graf-insert-heading-3 ()
  "Insert a level 3 heading."
  (interactive)
  (mark-graf-insert-heading 3))

;;;###autoload
(defun mark-graf-insert-heading-4 ()
  "Insert a level 4 heading."
  (interactive)
  (mark-graf-insert-heading 4))

;;;###autoload
(defun mark-graf-insert-heading-5 ()
  "Insert a level 5 heading."
  (interactive)
  (mark-graf-insert-heading 5))

;;;###autoload
(defun mark-graf-insert-heading-6 ()
  "Insert a level 6 heading."
  (interactive)
  (mark-graf-insert-heading 6))

;;;###autoload
(defun mark-graf-promote-heading ()
  "Promote current heading (decrease level, e.g., ## -> #)."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(#\\{2,6\\}\\) +")
      (delete-char 1))))

;;;###autoload
(defun mark-graf-demote-heading ()
  "Demote current heading (increase level, e.g., # -> ##)."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(#\\{1,5\\}\\) +")
      (goto-char (match-end 1))
      (insert "#"))))

;;; Link and Image Commands

;;;###autoload
(defun mark-graf-insert-link (&optional url text)
  "Insert a markdown link with URL and TEXT."
  (interactive
   (let* ((default-text (if (use-region-p)
                            (buffer-substring-no-properties
                             (region-beginning) (region-end))
                          ""))
          (text (read-string "Link text: " default-text))
          (url (read-string "URL: ")))
     (list url text)))
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert (format "[%s](%s)" (or text "link") (or url "")))
  (when (string-empty-p (or url ""))
    (backward-char 1)))

;;;###autoload
(defun mark-graf-insert-image (&optional path alt)
  "Insert a markdown image with PATH and ALT text."
  (interactive
   (let* ((default-path (read-file-name "Image file: " nil nil nil nil
                                        (lambda (f)
                                          (or (file-directory-p f)
                                              (string-match-p
                                               "\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\)$"
                                               f)))))
          (alt (read-string "Alt text: ")))
     (list default-path alt)))
  (let ((relative-path (if (and buffer-file-name
                                (file-name-absolute-p path))
                           (file-relative-name path
                                              (file-name-directory buffer-file-name))
                         path)))
    (insert (format "![%s](%s)" (or alt "") relative-path))))

;;;###autoload
(defun mark-graf-toggle-images ()
  "Toggle display of inline images in buffer."
  (interactive)
  (setq mark-graf-display-images (not mark-graf-display-images))
  (mark-graf-render--render-region (point-min) (point-max))
  (message "Image display %s" (if mark-graf-display-images "enabled" "disabled")))

;;; Navigation Commands

;;;###autoload
(defun mark-graf-next-heading ()
  "Move to next heading."
  (interactive)
  (let ((pos (point)))
    (end-of-line)
    (if (re-search-forward "^#+ " nil t)
        (beginning-of-line)
      (goto-char pos)
      (message "No more headings"))))

;;;###autoload
(defun mark-graf-prev-heading ()
  "Move to previous heading."
  (interactive)
  (let ((pos (point)))
    ;; Search backward from current position - re-search-backward finds
    ;; matches starting strictly before point
    (if (re-search-backward "^#\\{1,6\\} " nil t)
        (beginning-of-line)
      (goto-char pos)
      (message "No previous heading"))))

;;;###autoload
(defun mark-graf-next-heading-same-level ()
  "Move to next heading at the same level."
  (interactive)
  (let ((level (mark-graf-heading-level-at-point)))
    (if level
        (let ((pattern (format "^%s " (make-string level ?#)))
              (pos (point)))
          (end-of-line)
          (if (re-search-forward pattern nil t)
              (beginning-of-line)
            (goto-char pos)
            (message "No more headings at level %d" level)))
      (mark-graf-next-heading))))

;;;###autoload
(defun mark-graf-prev-heading-same-level ()
  "Move to previous heading at the same level."
  (interactive)
  (let ((level (mark-graf-heading-level-at-point)))
    (if level
        (let ((pattern (format "^%s " (make-string level ?#)))
              (pos (point)))
          (beginning-of-line)
          (if (re-search-backward pattern nil t)
              (beginning-of-line)
            (goto-char pos)
            (message "No previous heading at level %d" level)))
      (mark-graf-prev-heading))))

;;;###autoload
(defun mark-graf-up-heading ()
  "Move to parent heading (one level up)."
  (interactive)
  (let ((level (or (mark-graf-heading-level-at-point)
                   (save-excursion
                     (mark-graf-prev-heading)
                     (mark-graf-heading-level-at-point))
                   1)))
    (if (> level 1)
        (let ((pattern (format "^%s " (make-string (1- level) ?#)))
              (pos (point)))
          (beginning-of-line)
          (if (re-search-backward pattern nil t)
              (beginning-of-line)
            (goto-char pos)
            (message "No parent heading")))
      (message "Already at top level"))))

;;; List Commands

;;;###autoload
(defun mark-graf-insert-list-item ()
  "Insert a new list item.
Continues the current list type or creates a new unordered list."
  (interactive)
  (let ((marker-type (mark-graf-list-marker-at-point))
        (at-empty-line (save-excursion
                         (beginning-of-line)
                         (looking-at "^[ \t]*$"))))
    (cond
     ;; If at empty line without existing list marker, just insert list marker
     ((and (not marker-type) at-empty-line)
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (insert "- "))
     ;; Otherwise add new line and continue list
     (t
      (end-of-line)
      (insert "\n")
      (cond
       ((eq marker-type :task)
        (let ((indent (save-excursion
                        (forward-line -1)
                        (if (looking-at "^\\([ \t]*\\)")
                            (match-string 1)
                          ""))))
          (insert indent "- [ ] ")))
       ((eq marker-type :unordered)
        (let ((indent (save-excursion
                        (forward-line -1)
                        (if (looking-at "^\\([ \t]*\\)\\([-*+]\\)")
                            (concat (match-string 1) (match-string 2) " ")
                          "- "))))
          (insert indent)))
       ((eq marker-type :ordered)
        (let* ((prev-line (save-excursion
                            (forward-line -1)
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
               (num (if (string-match "^[ \t]*\\([0-9]+\\)" prev-line)
                        (1+ (string-to-number (match-string 1 prev-line)))
                      1))
               (indent (if (string-match "^\\([ \t]*\\)" prev-line)
                           (match-string 1 prev-line)
                         "")))
          (insert (format "%s%d. " indent num))))
       (t
        (insert "- ")))))))

;;;###autoload
(defun mark-graf-toggle-checkbox ()
  "Toggle task list checkbox at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "^\\([ \t]*[-*+][ \t]+\\)\\[ \\]")
      (replace-match "\\1[x]"))
     ((looking-at "^\\([ \t]*[-*+][ \t]+\\)\\[[xX]\\]")
      (replace-match "\\1[ ]"))
     (t
      (message "No checkbox on current line")))))

;;;###autoload
(defun mark-graf-move-item-up ()
  "Move current list item up."
  (interactive)
  (when-let ((bounds (mark-graf-list-item-bounds)))
    (let ((start (car bounds))
          (end (cdr bounds)))
      (when (> start (point-min))
        (let ((text (buffer-substring start end)))
          (delete-region start end)
          (forward-line -1)
          (beginning-of-line)
          (insert text))))))

;;;###autoload
(defun mark-graf-move-item-down ()
  "Move current list item down."
  (interactive)
  (when-let ((bounds (mark-graf-list-item-bounds)))
    (let ((start (car bounds))
          (end (cdr bounds)))
      (when (< end (point-max))
        (let ((text (buffer-substring start end)))
          (delete-region start end)
          (forward-line 1)
          (end-of-line)
          (insert "\n" (string-trim-right text)))))))

;;;###autoload
(defun mark-graf-promote-item ()
  "Decrease indentation of current list item."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^  ")
      (delete-char 2))))

;;;###autoload
(defun mark-graf-demote-item ()
  "Increase indentation of current list item."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "  ")))

;;; Table Commands

;;;###autoload
(defun mark-graf-insert-table (&optional rows cols)
  "Insert a markdown table with ROWS rows and COLS columns."
  (interactive
   (list (read-number "Rows: " 3)
         (read-number "Columns: " 3)))
  (mark-graf--ensure-blank-line-before)
  (let ((col-width 10))
    ;; Header row
    (insert "|")
    (dotimes (c cols)
      (let ((header (format "Header %d" (1+ c))))
        (insert (format " %-10s |" header))))
    (insert "\n")
    ;; Separator
    (insert "|")
    (dotimes (_ cols)
      (insert (make-string (+ col-width 2) ?-) "|"))
    (insert "\n")
    ;; Data rows
    (dotimes (_r rows)
      (insert "|")
      (dotimes (_c cols)
        (insert (format " %-10s |" "")))
      (insert "\n")))
  ;; Position cursor in first data cell
  (forward-line (- (1+ rows)))
  (search-forward "|" nil t)
  (forward-char 1))

;;;###autoload
(defun mark-graf-table-sort (&optional _column)
  "Sort table by _COLUMN (0-indexed)."
  (interactive
   (list (mark-graf-table-column-at-point)))
  (when-let ((bounds (mark-graf-table-bounds)))
    (let ((start (car bounds))
          (end (cdr bounds)))
      (save-excursion
        (goto-char start)
        ;; Skip header and separator
        (forward-line 2)
        (let ((data-start (point)))
          (goto-char end)
          (sort-lines nil data-start (point)))))))

;;; Tab Commands (Context-Sensitive)

;;;###autoload
(defun mark-graf-tab ()
  "Context-sensitive TAB command.
In tables: move to next cell.
In lists: increase indentation.
Elsewhere: normal tab behavior or cycle visibility."
  (interactive)
  (cond
   ((mark-graf-in-table-p)
    (mark-graf-table-next-cell))
   ((mark-graf-in-list-p)
    (mark-graf-demote-item))
   ((mark-graf-in-heading-p)
    (mark-graf-cycle-visibility))
   (t
    (indent-for-tab-command))))

;;;###autoload
(defun mark-graf-backtab ()
  "Context-sensitive Shift-TAB command.
In tables: move to previous cell.
In lists: decrease indentation.
Elsewhere: cycle global visibility."
  (interactive)
  (cond
   ((mark-graf-in-table-p)
    (mark-graf-table-prev-cell))
   ((mark-graf-in-list-p)
    (mark-graf-promote-item))
   (t
    (mark-graf-cycle-global-visibility))))

;;;###autoload
(defun mark-graf-table-next-cell ()
  "Move to the next table cell."
  (interactive)
  (when (mark-graf-in-table-p)
    (if (search-forward "|" (line-end-position) t)
        (if (looking-at "[ \t]*$\\|[ \t]*|")
            ;; End of row, go to next row
            (progn
              (forward-line 1)
              (when (looking-at "^|[-:|]+|")  ; Skip separator row
                (forward-line 1))
              (search-forward "|" (line-end-position) t))
          (skip-chars-forward " \t"))
      ;; No more cells on this line
      (forward-line 1)
      (when (looking-at "^|[-:|]+|")
        (forward-line 1))
      (search-forward "|" (line-end-position) t))))

;;;###autoload
(defun mark-graf-table-prev-cell ()
  "Move to the previous table cell."
  (interactive)
  (when (mark-graf-in-table-p)
    (skip-chars-backward " \t")
    (if (search-backward "|" (line-beginning-position) t)
        (progn
          (search-backward "|" (line-beginning-position) t)
          (forward-char 1)
          (skip-chars-forward " \t"))
      ;; Beginning of row, go to previous row
      (forward-line -1)
      (when (looking-at "^|[-:|]+|")  ; Skip separator row
        (forward-line -1))
      (end-of-line)
      (search-backward "|" (line-beginning-position) t)
      (search-backward "|" (line-beginning-position) t)
      (forward-char 1)
      (skip-chars-forward " \t"))))

;;; Visibility Commands

;;;###autoload
(defun mark-graf-cycle-visibility ()
  "Cycle visibility of current heading subtree."
  (interactive)
  ;; Simplified - just toggle children visibility
  (outline-toggle-children))

;;;###autoload
(defun mark-graf-cycle-global-visibility ()
  "Cycle global visibility of all headings."
  (interactive)
  ;; Simplified global cycling
  (cond
   ((and (boundp 'mark-graf--visibility-state)
         (eq mark-graf--visibility-state 'all))
    (outline-hide-body)
    (setq-local mark-graf--visibility-state 'headings))
   ((and (boundp 'mark-graf--visibility-state)
         (eq mark-graf--visibility-state 'headings))
    (outline-hide-sublevels 1)
    (setq-local mark-graf--visibility-state 'top))
   (t
    (outline-show-all)
    (setq-local mark-graf--visibility-state 'all))))

;;; View Toggle Commands

;;;###autoload
(defun mark-graf-show-source ()
  "Show entire buffer as raw markdown source."
  (interactive)
  (setq mark-graf--rendering-enabled nil)
  (mark-graf-render--clear-all)
  (force-mode-line-update))

;;;###autoload
(defun mark-graf-show-rendered ()
  "Show entire buffer with rendering enabled."
  (interactive)
  (setq mark-graf--rendering-enabled t)
  (mark-graf-render--render-region (point-min) (point-max))
  (setq mark-graf--full-render-done-p t)
  (force-mode-line-update))

;;;###autoload
(defun mark-graf-toggle-view ()
  "Toggle between rendered and source view."
  (interactive)
  (if mark-graf--rendering-enabled
      (mark-graf-show-source)
    (mark-graf-show-rendered)))

;;;###autoload
(defun mark-graf-toggle-element-at-point ()
  "Toggle rendering of element at point."
  (interactive)
  (if-let ((elem (mark-graf-ts--element-at (point))))
      (let* ((start (mark-graf-node-start elem))
             (end (mark-graf-node-end elem))
             (has-overlay (cl-some
                          (lambda (ov)
                            (overlay-get ov 'mark-graf))
                          (overlays-in start end))))
        (if has-overlay
            (mark-graf-render--unrender-region start end)
          (mark-graf-render--render-region start end)))
    (message "No markdown element at point")))

;;;###autoload
(defun mark-graf-follow-link-at-point ()
  "Follow the link at point.
Works both on rendered lines (via overlay properties) and on
revealed lines (by parsing raw markdown syntax at point)."
  (interactive)
  ;; First try overlay property (rendered line)
  (let ((url (get-char-property (point) 'mark-graf-url)))
    (unless url
      ;; Fallback: parse raw markdown on the current line
      (save-excursion
        (let ((pos (point))
              (bol (line-beginning-position))
              (eol (line-end-position)))
          ;; Check for inline link: [text](url)
          (goto-char bol)
          (while (and (not url)
                      (re-search-forward "\\[\\([^]]*\\)\\](\\([^)]+\\))" eol t))
            (when (and (>= pos (match-beginning 0))
                       (<= pos (match-end 0)))
              (setq url (match-string-no-properties 2))))
          ;; Check for autolink: <url>
          (unless url
            (goto-char bol)
            (while (and (not url)
                        (re-search-forward "<\\([a-zA-Z]+://[^>]+\\)>" eol t))
              (when (and (>= pos (match-beginning 0))
                         (<= pos (match-end 0)))
                (setq url (match-string-no-properties 1)))))
          ;; Check for bare URL at point
          (unless url
            (goto-char bol)
            (while (and (not url)
                        (re-search-forward "https?://[^ \t\n\r>)\"']+" eol t))
              (when (and (>= pos (match-beginning 0))
                         (<= pos (match-end 0)))
                (setq url (match-string-no-properties 0))))))))
    (if url
        (mark-graf-render--follow-link url)
      (message "No link at point"))))

;;; Code Block Edit Mode

(defvar-local mark-graf-code-edit--source-buffer nil
  "Source buffer for code edit indirect buffer.")

(defvar-local mark-graf-code-edit--block-bounds nil
  "Cons (START . END) of the full code block in the source buffer.")

(defvar-local mark-graf-code-edit--content-bounds nil
  "Cons (START . END) of the content region in the source buffer.")

(defvar-local mark-graf-code-edit--original-content nil
  "Original content of the code block, for abort/revert.")

(defvar mark-graf-code-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") #'mark-graf-code-edit-finish)
    (define-key map (kbd "C-c C-k") #'mark-graf-code-edit-abort)
    map)
  "Keymap for `mark-graf-code-edit-mode'.")

(define-minor-mode mark-graf-code-edit-mode
  "Minor mode for editing a code block in an indirect buffer."
  :lighter " MG-Edit"
  :keymap mark-graf-code-edit-mode-map)

;;;###autoload
(defun mark-graf-edit-code-block ()
  "Edit code block at point in an indirect buffer with the correct major mode.
Like org-mode's `org-edit-special': opens a narrowed indirect buffer
with the language's major mode."
  (interactive)
  (when (and (boundp 'mark-graf--code-edit-buffer)
             mark-graf--code-edit-buffer
             (buffer-live-p mark-graf--code-edit-buffer))
    (user-error "Already editing a code block; finish or abort first"))
  (let ((info (mark-graf--fenced-code-block-content-at (point))))
    (unless info
      (user-error "Not inside a fenced code block"))
    (let* ((source-buf (current-buffer))
           (block-start (plist-get info :block-start))
           (block-end (plist-get info :block-end))
           (content-start (plist-get info :content-start))
           (content-end (plist-get info :content-end))
           (language (plist-get info :language))
           (original (buffer-substring-no-properties content-start content-end))
           (buf-name (format "*MG-Edit:%s*"
                             (or language "code")))
           (edit-buf (make-indirect-buffer source-buf buf-name t)))
      ;; Unrender the block so raw text is visible
      (when (and (boundp 'mark-graf--rendering-enabled)
                 mark-graf--rendering-enabled)
        (mark-graf-render--unrender-region block-start block-end))
      ;; Set up state in source buffer
      (setq mark-graf--code-edit-buffer edit-buf)
      ;; Set up the indirect buffer
      (with-current-buffer edit-buf
        (narrow-to-region content-start content-end)
        ;; Set major mode based on language.
        ;; Must run without delay-mode-hooks so font-lock, indentation,
        ;; completion, and other mode features fully activate.
        (when language
          (let ((mode (mark-graf-render--language-to-mode language)))
            (when mode
              (funcall mode))))
        ;; Set buffer-local state
        (setq mark-graf-code-edit--source-buffer source-buf)
        (setq mark-graf-code-edit--block-bounds (cons block-start block-end))
        (setq mark-graf-code-edit--content-bounds (cons content-start content-end))
        (setq mark-graf-code-edit--original-content original)
        ;; Enable the minor mode
        (mark-graf-code-edit-mode 1)
        ;; Header line with hints
        (setq header-line-format
              (substitute-command-keys
               "Editing code block.  \\[mark-graf-code-edit-finish] to finish, \\[mark-graf-code-edit-abort] to abort"))
        ;; Clean up on external buffer kill
        (add-hook 'kill-buffer-hook #'mark-graf-code-edit--on-kill nil t))
      ;; Show the edit buffer
      (pop-to-buffer edit-buf))))

;;;###autoload
(defun mark-graf-code-edit-finish ()
  "Finish editing code block and return to source buffer.
Re-renders the code block region."
  (interactive)
  (unless mark-graf-code-edit--source-buffer
    (user-error "Not in a code edit buffer"))
  (let ((source-buf mark-graf-code-edit--source-buffer)
        (block-bounds mark-graf-code-edit--block-bounds)
        (edit-buf (current-buffer)))
    ;; Widen before switching
    (widen)
    ;; Switch to source and clean up
    (when (buffer-live-p source-buf)
      (switch-to-buffer source-buf)
      (setq mark-graf--code-edit-buffer nil)
      ;; Re-render the block region
      (when (and (boundp 'mark-graf--rendering-enabled)
                 mark-graf--rendering-enabled)
        (mark-graf-render--render-region (car block-bounds) (cdr block-bounds))))
    ;; Kill the edit buffer (remove hook first to avoid double-cleanup)
    (with-current-buffer edit-buf
      (remove-hook 'kill-buffer-hook #'mark-graf-code-edit--on-kill t))
    (kill-buffer edit-buf)))

;;;###autoload
(defun mark-graf-code-edit-abort ()
  "Abort editing code block, restoring original content."
  (interactive)
  (unless mark-graf-code-edit--source-buffer
    (user-error "Not in a code edit buffer"))
  (let ((source-buf mark-graf-code-edit--source-buffer)
        (content-bounds mark-graf-code-edit--content-bounds)
        (block-bounds mark-graf-code-edit--block-bounds)
        (original mark-graf-code-edit--original-content)
        (edit-buf (current-buffer)))
    ;; Widen so we can restore
    (widen)
    ;; Restore original content in the shared buffer text
    (when (buffer-live-p source-buf)
      (with-current-buffer source-buf
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (car content-bounds))
            (delete-region (car content-bounds) (cdr content-bounds))
            (insert original)))))
    ;; Switch to source and clean up
    (when (buffer-live-p source-buf)
      (switch-to-buffer source-buf)
      (setq mark-graf--code-edit-buffer nil)
      ;; Re-render the block region
      (when (and (boundp 'mark-graf--rendering-enabled)
                 mark-graf--rendering-enabled)
        (mark-graf-render--render-region (car block-bounds) (cdr block-bounds))))
    ;; Kill the edit buffer
    (with-current-buffer edit-buf
      (remove-hook 'kill-buffer-hook #'mark-graf-code-edit--on-kill t))
    (kill-buffer edit-buf)))

(defun mark-graf-code-edit--on-kill ()
  "Handle external kill of code edit buffer (e.g., \\`C-x k\\`).
Cleans up source buffer state and re-renders the code block."
  (when (and mark-graf-code-edit--source-buffer
             (buffer-live-p mark-graf-code-edit--source-buffer))
    (let ((source-buf mark-graf-code-edit--source-buffer)
          (block-bounds mark-graf-code-edit--block-bounds))
      (with-current-buffer source-buf
        (setq mark-graf--code-edit-buffer nil)
        (when (and (boundp 'mark-graf--rendering-enabled)
                   mark-graf--rendering-enabled)
          (mark-graf-render--render-region
           (car block-bounds) (cdr block-bounds)))))))

(provide 'mark-graf-commands)
;;; mark-graf-commands.el ends here

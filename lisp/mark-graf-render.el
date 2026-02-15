;;; mark-graf-render.el --- Rendering engine for mark-graf -*- lexical-binding: t; -*-

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

;; Rendering engine for mark-graf.
;; Manages overlays, text properties, and display properties.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'url-parse)
(require 'mark-graf-mermaid)

;; Variables defined in mark-graf-ts.el
(defvar mark-graf-ts--use-tree-sitter)

;; Variables defined in mark-graf.el
(defvar mark-graf--rendering-enabled)
(defvar mark-graf-display-images)
(defvar mark-graf-image-max-width)
(defvar mark-graf-image-max-height)
(defvar mark-graf-left-margin)

;; Variables defined later in this file
(defvar mark-graf-math-block-scale)

;; Functions defined in mark-graf-ts.el
(declare-function mark-graf-ts--elements-in-region "mark-graf-ts")
(declare-function mark-graf-ts--fallback-parse-region "mark-graf-ts")
(declare-function mark-graf-ts--inline-elements-in "mark-graf-ts")
(declare-function mark-graf-ts--children "mark-graf-ts")
(declare-function mark-graf-node-type "mark-graf-ts")
(declare-function mark-graf-node-start "mark-graf-ts")
(declare-function mark-graf-node-end "mark-graf-ts")
(declare-function mark-graf-node-level "mark-graf-ts")
(declare-function mark-graf-node-language "mark-graf-ts")
(declare-function mark-graf-node-properties "mark-graf-ts")

;;; Internal Variables

(defvar-local mark-graf-render--overlays nil
  "List of active overlays in the buffer.")

(defvar-local mark-graf-render--overlay-pool nil
  "Pool of reusable overlays.")

(defvar-local mark-graf-render--rendered-regions nil
  "Hash table of rendered region markers.")

(defvar-local mark-graf-render--rendering-p nil
  "Non-nil when rendering is in progress.")

;;; Display Character Sets

(defvar mark-graf-render--bullet-chars '(?● ?○ ?■ ?□)
  "Characters for unordered list bullets at each nesting level.")

(defvar mark-graf-render--checkbox-chars
  '((unchecked . ?☐)
    (checked . ?☑)
    (partial . ?☒))
  "Characters for task list checkboxes.")

(defvar mark-graf-render--hr-char ?─
  "Character used for horizontal rules.")

(defvar mark-graf-render--table-chars
  '((top-left . ?┌)
    (top-right . ?┐)
    (bottom-left . ?└)
    (bottom-right . ?┘)
    (horizontal . ?─)
    (vertical . ?│)
    (cross . ?┼)
    (t-down . ?┬)
    (t-up . ?┴)
    (t-right . ?├)
    (t-left . ?┤))
  "Characters for table borders.")

(defvar mark-graf-render--blockquote-char ?▌
  "Character for blockquote left border.")

;;; Initialization

(defun mark-graf-render--init ()
  "Initialize the rendering engine for current buffer."
  (setq mark-graf-render--overlays '())
  (setq mark-graf-render--overlay-pool '())
  (setq mark-graf-render--rendered-regions (make-hash-table :test 'equal))
  (mark-graf-render--setup-display-chars))

(defun mark-graf-render--setup-display-chars ()
  "Set up display characters based on environment."
  (cond
   ;; GUI Emacs - use Unicode
   ((display-graphic-p)
    (setq mark-graf-render--bullet-chars '(?● ?○ ?■ ?□))
    (setq mark-graf-render--checkbox-chars '((unchecked . ?☐) (checked . ?☑)))
    (setq mark-graf-render--hr-char ?─)
    (setq mark-graf-render--blockquote-char ?▌))
   ;; Terminal with Unicode
   ((char-displayable-p ?●)
    (setq mark-graf-render--bullet-chars '(?● ?○ ?◆ ?◇))
    (setq mark-graf-render--checkbox-chars '((unchecked . ?☐) (checked . ?☑)))
    (setq mark-graf-render--hr-char ?─)
    (setq mark-graf-render--blockquote-char ?│))
   ;; Basic ASCII terminal
   (t
    (setq mark-graf-render--bullet-chars '(?* ?- ?+ ?.))
    (setq mark-graf-render--checkbox-chars '((unchecked . ?\[) (checked . ?x)))
    (setq mark-graf-render--hr-char ?-)
    (setq mark-graf-render--blockquote-char ?|))))

;;; Overlay Management

(defun mark-graf-render--get-overlay (start end)
  "Get an overlay for region START to END, reusing from pool if possible."
  (let ((ov (or (pop mark-graf-render--overlay-pool)
                (make-overlay start end nil t nil))))
    (move-overlay ov start end)
    ;; Clear any existing properties when reusing from pool
    (overlay-put ov 'display nil)
    (overlay-put ov 'face nil)
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'after-string nil)
    (overlay-put ov 'wrap-prefix nil)
    (overlay-put ov 'line-prefix nil)
    (overlay-put ov 'priority nil)
    ;; Set standard properties
    (overlay-put ov 'mark-graf t)
    (overlay-put ov 'evaporate t)
    (push ov mark-graf-render--overlays)
    ov))

(defun mark-graf-render--release-overlay (ov)
  "Release overlay OV back to the pool."
  (when (overlay-buffer ov)
    (overlay-put ov 'display nil)
    (overlay-put ov 'face nil)
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'after-string nil)
    (overlay-put ov 'help-echo nil)
    (delete-overlay ov)
    (setq mark-graf-render--overlays (delq ov mark-graf-render--overlays))
    (push ov mark-graf-render--overlay-pool)))

(defun mark-graf-render--clear-region (start end)
  "Clear all mark-graf overlays in region from START to END."
  (dolist (ov (overlays-in start end))
    (when (overlay-get ov 'mark-graf)
      (mark-graf-render--release-overlay ov))))

(defun mark-graf-render--clear-all ()
  "Clear all mark-graf overlays in the buffer."
  (dolist (ov mark-graf-render--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq mark-graf-render--overlays nil)
  (setq mark-graf-render--overlay-pool nil))

;;; Core Rendering Functions

(defun mark-graf-render--element-in-table-p (elem-start table-regions)
  "Return non-nil if ELEM-START is inside one of TABLE-REGIONS."
  (cl-some (lambda (region)
             (and (>= elem-start (car region))
                  (<= elem-start (cdr region))))
           table-regions))

(defun mark-graf-render--render-region (start end)
  "Render markdown elements in region from START to END."
  (when (and (not mark-graf-render--rendering-p)
             mark-graf--rendering-enabled)
    (let ((mark-graf-render--rendering-p t)
          (inhibit-read-only t))
      (with-silent-modifications
        (mark-graf-render--clear-region start end)
        (let ((elements (if mark-graf-ts--use-tree-sitter
                           (mark-graf-ts--elements-in-region start end)
                         (mark-graf-ts--fallback-parse-region start end))))
          (dolist (elem elements)
            (mark-graf-render--render-element elem)))))))

(defun mark-graf-render--unrender-region (start end)
  "Remove rendering from region START to END."
  (with-silent-modifications
    (mark-graf-render--clear-region start end)))

(defun mark-graf-render--render-line (line-num)
  "Render markdown elements on line LINE-NUM.
If the line is part of a table, expands to render the entire table,
because tree-sitter cannot find the pipe_table node from a single-line range."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-num))
    (let ((bol (line-beginning-position))
          (eol (line-end-position)))
      ;; If the line looks like a table row, render the full table
      (if (save-match-data (looking-at "^[ \t]*|.+|[ \t]*$"))
          (let ((table-start bol)
                (table-end eol))
            ;; Scan backward for table start
            (save-excursion
              (while (and (not (bobp))
                          (progn (forward-line -1)
                                 (save-match-data
                                   (looking-at "^[ \t]*|.+|[ \t]*$"))))
                (setq table-start (line-beginning-position))))
            ;; Scan forward for table end
            (save-excursion
              (while (and (progn (forward-line 1) (not (eobp)))
                          (save-match-data
                            (looking-at "^[ \t]*|.+|[ \t]*$")))
                (setq table-end (line-end-position))))
            (mark-graf-render--render-region table-start table-end))
        (mark-graf-render--render-region bol eol)))))

(defun mark-graf-render--unrender-line (line-num)
  "Remove rendering from line LINE-NUM, revealing raw markdown for editing.
For table rows the overlay covering the source line plus its newline
is removed so the user can see and edit the pipe-delimited text.
A lightweight overlay is placed on the line to override buffer-local
`line-prefix' so the raw text aligns with the rendered table rows."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-num))
    (let ((bol (line-beginning-position))
          (eol (line-end-position)))
      ;; For table rows the per-row overlay spans bol..(1+ eol);
      ;; clear that full range so the display property is removed.
      (mark-graf-render--unrender-region
       bol (min (1+ eol) (point-max)))
      ;; Place a lightweight overlay to suppress the buffer-local
      ;; line-prefix on this line, so the raw pipe text starts at
      ;; column 0 like the rendered table rows around it.
      (let ((ov (make-overlay bol eol nil t nil)))
        (overlay-put ov 'mark-graf t)
        (overlay-put ov 'mark-graf-type 'unrender-prefix)
        (overlay-put ov 'line-prefix "")
        (overlay-put ov 'wrap-prefix "")
        (overlay-put ov 'priority 200)
        (push ov mark-graf-render--overlays)))))

(defun mark-graf-render--ensure-rendered (start end)
  "Ensure region from START to END is rendered."
  (let ((key (cons start end)))
    (unless (gethash key mark-graf-render--rendered-regions)
      (mark-graf-render--render-region start end)
      (puthash key t mark-graf-render--rendered-regions))))

;;; Element Rendering Dispatch

(defun mark-graf-render--render-element (elem)
  "Render a single element ELEM."
  (when elem
    (pcase (mark-graf-node-type elem)
      ;; Headings
      ('heading (mark-graf-render--heading elem))
      ;; Inline formatting
      ('strong (mark-graf-render--strong elem))
      ('emphasis (mark-graf-render--emphasis elem))
      ('strikethrough (mark-graf-render--strikethrough elem))
      ('code-span (mark-graf-render--code-span elem))
      ;; Links and images
      ('link (mark-graf-render--link elem))
      ('link-ref (mark-graf-render--link elem))
      ('image (mark-graf-render--image elem))
      ('autolink (mark-graf-render--autolink elem))
      ;; Block elements
      ('code-block (mark-graf-render--code-block elem))
      ('blockquote (mark-graf-render--blockquote elem))
      ('list (mark-graf-render--list elem))
      ('list-item (mark-graf-render--list-item elem))
      ('hr (mark-graf-render--hr elem))
      ('table (mark-graf-render--table elem))
      ('table-row (mark-graf-render--table-row-standalone elem))
      ('table-separator (mark-graf-render--table-separator elem))
      ;; Extended elements
      ('footnote-ref (mark-graf-render--footnote-ref elem))
      ('math (mark-graf-render--math elem))
      ('math-block (mark-graf-render--math-block elem))
      ;; Paragraphs: parse and render inline elements
      ('paragraph (mark-graf-render--paragraph elem))
      ;; Default: no special rendering
      (_ nil))))

;;; Paragraph Rendering

(defun mark-graf-render--paragraph (elem)
  "Render paragraph ELEM by parsing and rendering its inline elements."
  (let* ((start (mark-graf-node-start elem))
         (end (mark-graf-node-end elem))
         (inlines (mark-graf-ts--inline-elements-in start end)))
    (dolist (inline-elem inlines)
      (ignore-errors
        (mark-graf-render--render-element inline-elem)))))

;;; Inline Element Rendering

(defun mark-graf-render--heading (elem)
  "Render heading element ELEM."
  (let* ((start (mark-graf-node-start elem))
         (end (mark-graf-node-end elem))
         (level (or (mark-graf-node-level elem) 1))
         (face (intern (format "mark-graf-heading-%d" (min level 6)))))
    ;; Find the marker (### )
    (save-excursion
      (goto-char start)
      (when (looking-at "^\\(#\\{1,6\\}\\)[ \t]*")
        (let ((marker-end (match-end 0))
              (content-start (match-end 0)))
          ;; Hide the marker
          (let ((ov (mark-graf-render--get-overlay start marker-end)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'heading-marker))
          ;; Apply face to content
          (let ((ov (mark-graf-render--get-overlay content-start end)))
            (overlay-put ov 'face face)
            (overlay-put ov 'mark-graf-type 'heading-content)))))))

(defun mark-graf-render--strong (elem)
  "Render strong/bold element ELEM."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "\\(\\*\\*\\|__\\)\\([^\n\r]*?\\)\\(\\*\\*\\|__\\)")
        (let ((delim1-end (match-end 1))
              (content-start (match-beginning 2))
              (content-end (match-end 2))
              (delim2-start (match-beginning 3)))
          ;; Hide opening delimiter
          (let ((ov (mark-graf-render--get-overlay start delim1-end)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'strong-delim))
          ;; Apply bold face to content - high priority to override backgrounds
          (let ((ov (mark-graf-render--get-overlay content-start content-end)))
            (overlay-put ov 'face 'mark-graf-bold)
            (overlay-put ov 'mark-graf-type 'strong-content)
            (overlay-put ov 'priority 100))
          ;; Hide closing delimiter
          (let ((ov (mark-graf-render--get-overlay delim2-start end)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'strong-delim)))))))

(defun mark-graf-render--emphasis (elem)
  "Render emphasis/italic element ELEM."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "\\([*_]\\)\\([^\n\r]*?\\)\\([*_]\\)")
        (let ((delim1-end (match-end 1))
              (content-start (match-beginning 2))
              (content-end (match-end 2))
              (delim2-start (match-beginning 3)))
          ;; Hide opening delimiter
          (let ((ov (mark-graf-render--get-overlay start delim1-end)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'emphasis-delim))
          ;; Apply italic face to content - high priority
          (let ((ov (mark-graf-render--get-overlay content-start content-end)))
            (overlay-put ov 'face 'mark-graf-italic)
            (overlay-put ov 'mark-graf-type 'emphasis-content)
            (overlay-put ov 'priority 100))
          ;; Hide closing delimiter
          (let ((ov (mark-graf-render--get-overlay delim2-start end)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'emphasis-delim)))))))

(defun mark-graf-render--strikethrough (elem)
  "Render strikethrough element ELEM."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "~~\\([^\n\r]*?\\)~~")
        (let ((content-start (match-beginning 1))
              (content-end (match-end 1)))
          ;; Hide opening ~~
          (let ((ov (mark-graf-render--get-overlay start (+ start 2))))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'strike-delim))
          ;; Apply strikethrough face
          (let ((ov (mark-graf-render--get-overlay content-start content-end)))
            (overlay-put ov 'face 'mark-graf-strikethrough)
            (overlay-put ov 'mark-graf-type 'strike-content))
          ;; Hide closing ~~
          (let ((ov (mark-graf-render--get-overlay (- end 2) end)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'strike-delim)))))))

(defun mark-graf-render--code-span (elem)
  "Render inline code span element ELEM."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "`+\\([^`\n\r]+\\)`+")
        (let* ((backtick-count (- (match-end 0) (match-beginning 0)
                                  (- (match-end 1) (match-beginning 1))))
               (delim-len (/ backtick-count 2))
               (content-start (+ start delim-len))
               (content-end (- end delim-len)))
          ;; Hide opening backticks
          (let ((ov (mark-graf-render--get-overlay start content-start)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'code-delim))
          ;; Apply code face to content - high priority
          (let ((ov (mark-graf-render--get-overlay content-start content-end)))
            (overlay-put ov 'face 'mark-graf-inline-code)
            (overlay-put ov 'mark-graf-type 'code-content)
            (overlay-put ov 'priority 90))
          ;; Hide closing backticks
          (let ((ov (mark-graf-render--get-overlay content-end end)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'code-delim)))))))

;;; Link and Image Rendering

(defvar mark-graf-render--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'mark-graf-render--follow-link-at-mouse)
    (define-key map [mouse-2] #'mark-graf-render--follow-link-at-mouse)
    (define-key map (kbd "RET") #'mark-graf-render--follow-link-at-point)
    map)
  "Keymap for link overlays.")

(defun mark-graf-render--link (elem)
  "Render link element ELEM."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "\\[\\([^]]+\\)\\](\\([^)]+\\))")
        (let ((_text (match-string 1))
              (url (match-string 2))
              (text-start (match-beginning 1))
              (text-end (match-end 1))
              (_url-start (match-beginning 2))
              (_url-end (match-end 2)))
          ;; Hide opening [
          (let ((ov (mark-graf-render--get-overlay start (1+ start))))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'link-delim))
          ;; Style link text
          (let ((ov (mark-graf-render--get-overlay text-start text-end)))
            (overlay-put ov 'face 'mark-graf-link)
            (overlay-put ov 'mouse-face 'highlight)
            (overlay-put ov 'help-echo url)
            (overlay-put ov 'keymap mark-graf-render--link-keymap)
            (overlay-put ov 'follow-link t)
            (overlay-put ov 'mark-graf-url url)
            (overlay-put ov 'mark-graf-type 'link-text))
          ;; Hide ]( and URL and )
          (let ((ov (mark-graf-render--get-overlay text-end end)))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'link-delim)))))))

(defun mark-graf-render--follow-link (url)
  "Follow link URL - handle internal anchors vs external URLs."
  (cond
   ;; Internal anchor link (starts with #)
   ((string-prefix-p "#" url)
    (let ((anchor (substring url 1)))
      (mark-graf-render--goto-anchor anchor)))
   ;; Relative file link
   ((and (not (string-match-p "^[a-z]+://" url))
         (string-match-p "\\.md\\(?:#\\|$\\)" url))
    (let* ((parts (split-string url "#"))
           (file (car parts))
           (anchor (cadr parts)))
      (find-file (expand-file-name file))
      (when anchor
        (mark-graf-render--goto-anchor anchor))))
   ;; External URL
   (t (browse-url url))))

(defun mark-graf-render--goto-anchor (anchor)
  "Navigate to ANCHOR (heading ID) in current buffer."
  (goto-char (point-min))
  ;; Search for heading that matches anchor
  (let ((target-id (downcase (replace-regexp-in-string "[^a-z0-9-]" "" anchor))))
    (if (re-search-forward "^#\\{1,6\\} +\\(.+\\)$" nil t)
        (let ((found nil))
          (goto-char (point-min))
          (while (and (not found)
                      (re-search-forward "^#\\{1,6\\} +\\(.+\\)$" nil t))
            (let* ((heading-text (match-string 1))
                   (heading-id (downcase
                               (replace-regexp-in-string
                                "[^a-z0-9-]" ""
                                (replace-regexp-in-string " +" "-" heading-text)))))
              (when (string= heading-id target-id)
                (setq found t)
                (beginning-of-line)
                (recenter 0))))  ; Put heading at top of window
          (unless found
            (message "Anchor '%s' not found" anchor)))
      (message "No headings found"))))

(defun mark-graf-render--follow-link-at-mouse (event)
  "Follow link at mouse EVENT position."
  (interactive "e")
  (let* ((pos (posn-point (event-end event)))
         (url (get-char-property pos 'mark-graf-url)))
    (when url
      (mark-graf-render--follow-link url))))

(defun mark-graf-render--follow-link-at-point ()
  "Follow link at point."
  (interactive)
  (let ((url (get-char-property (point) 'mark-graf-url)))
    (when url
      (mark-graf-render--follow-link url))))

(defvar mark-graf-render--image-cache-dir nil
  "Directory for caching downloaded remote images.")

(defun mark-graf-render--image-cache-dir ()
  "Return the image cache directory, creating it if needed."
  (unless mark-graf-render--image-cache-dir
    (setq mark-graf-render--image-cache-dir
          (expand-file-name "mark-graf/images"
                            (or (getenv "XDG_CACHE_HOME")
                                (expand-file-name ".cache" "~")))))
  (unless (file-directory-p mark-graf-render--image-cache-dir)
    (make-directory mark-graf-render--image-cache-dir t))
  mark-graf-render--image-cache-dir)

(defun mark-graf-render--url-p (path)
  "Return non-nil if PATH is a remote URL."
  (or (string-prefix-p "http://" path)
      (string-prefix-p "https://" path)))

(defun mark-graf-render--image-cache-path (url)
  "Return local cache file path for remote image URL."
  (let* ((hash (md5 url))
         ;; Preserve extension for image type detection
         (ext (or (file-name-extension (url-filename (url-generic-parse-url url)))
                  "png")))
    (expand-file-name (concat hash "." ext)
                      (mark-graf-render--image-cache-dir))))

(defun mark-graf-render--image (elem)
  "Render image element ELEM."
  (when mark-graf-display-images
    (let ((start (mark-graf-node-start elem))
          (end (mark-graf-node-end elem)))
      (save-excursion
        (goto-char start)
        (when (looking-at "!\\[\\([^]]*\\)\\](\\([^)]+\\))")
          (let* ((alt-text (match-string 1))
                 (image-path (match-string 2))
                 (full-path (mark-graf-render--resolve-image-path image-path)))
            (cond
             ;; Local file exists - display directly
             ((and full-path
                   (not (mark-graf-render--url-p image-path))
                   (file-exists-p full-path))
              (mark-graf-render--display-image start end full-path alt-text image-path))
             ;; Remote URL - check cache or fetch async
             ((and full-path (mark-graf-render--url-p image-path))
              (let ((cache-file (mark-graf-render--image-cache-path image-path)))
                (if (file-exists-p cache-file)
                    ;; Already cached
                    (mark-graf-render--display-image start end cache-file alt-text image-path)
                  ;; Show placeholder and fetch async
                  (mark-graf-render--image-placeholder start end alt-text image-path)
                  (mark-graf-render--fetch-image-async
                   image-path cache-file
                   (current-buffer) start end alt-text))))
             ;; Not found
             (t
              (mark-graf-render--image-placeholder start end alt-text image-path)))))))))

(defun mark-graf-render--display-image (start end file-path alt-text orig-path)
  "Display image from FILE-PATH as overlay from START to END."
  (let* ((image (create-image file-path nil nil
                              :max-width mark-graf-image-max-width
                              :max-height mark-graf-image-max-height))
         (ov (mark-graf-render--get-overlay start end)))
    (overlay-put ov 'display image)
    (overlay-put ov 'help-echo (format "%s\n%s" alt-text orig-path))
    (overlay-put ov 'mark-graf-type 'image)))

(defun mark-graf-render--image-placeholder (start end alt-text image-path)
  "Show placeholder overlay from START to END for image."
  (let ((ov (mark-graf-render--get-overlay start end)))
    (overlay-put ov 'display
                 (propertize (format "[Image: %s]" (or alt-text image-path))
                            'face 'mark-graf-image-alt))
    (overlay-put ov 'mark-graf-type 'image-placeholder)))

(defun mark-graf-render--fetch-image-async (url cache-file buffer start end alt-text)
  "Fetch image from URL asynchronously, cache to CACHE-FILE, then display."
  (require 'url)
  (url-retrieve
   url
   (lambda (status)
     (unwind-protect
         (if (plist-get status :error)
             (message "mark-graf: Failed to fetch image: %s" url)
           ;; Skip HTTP headers
           (goto-char (point-min))
           (when (re-search-forward "\r?\n\r?\n" nil t)
             ;; Write binary data directly to cache file
             (let ((body-start (point))
                   (coding-system-for-write 'binary))
               (set-buffer-multibyte nil)
               (write-region body-start (point-max) cache-file nil 'silent))
             ;; Update the overlay in the original buffer
             (when (and (file-exists-p cache-file)
                        (> (file-attribute-size (file-attributes cache-file)) 0)
                        (buffer-live-p buffer))
               (with-current-buffer buffer
                 ;; Find and replace the placeholder overlay
                 (dolist (ov (overlays-in start end))
                   (when (eq (overlay-get ov 'mark-graf-type) 'image-placeholder)
                     (let ((image (create-image cache-file nil nil
                                                :max-width mark-graf-image-max-width
                                                :max-height mark-graf-image-max-height)))
                       (overlay-put ov 'display image)
                       (overlay-put ov 'help-echo (format "%s\n%s" alt-text url))
                       (overlay-put ov 'mark-graf-type 'image))))))))
       (kill-buffer)))
   nil t t))

(defun mark-graf-render--resolve-image-path (path)
  "Resolve image PATH relative to current buffer's directory."
  (if (or (string-prefix-p "http://" path)
          (string-prefix-p "https://" path)
          (file-name-absolute-p path))
      path
    (when buffer-file-name
      (expand-file-name path (file-name-directory buffer-file-name)))))

(defun mark-graf-render--autolink (elem)
  "Render autolink element ELEM."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "<\\([^>]+\\)>")
        (let ((url (match-string 1)))
          ;; Hide < >
          (let ((ov (mark-graf-render--get-overlay start (1+ start))))
            (overlay-put ov 'display ""))
          (let ((ov (mark-graf-render--get-overlay (1- end) end)))
            (overlay-put ov 'display ""))
          ;; Style the URL
          (let ((ov (mark-graf-render--get-overlay (1+ start) (1- end))))
            (overlay-put ov 'face 'mark-graf-link)
            (overlay-put ov 'mouse-face 'highlight)
            (overlay-put ov 'help-echo url)
            (overlay-put ov 'mark-graf-url url)
            (overlay-put ov 'follow-link t)
            (overlay-put ov 'keymap mark-graf-render--link-keymap)))))))

;;; Block Element Rendering

(defun mark-graf-render--code-block (elem)
  "Render fenced code block element ELEM.
Uses :extend t on face to extend background to window edge.
Dispatches to mermaid renderer for mermaid code blocks."
  (let* ((start (mark-graf-node-start elem))
         (end (mark-graf-node-end elem))
         (language (or (plist-get (mark-graf-node-properties elem) :language)
                       (mark-graf-node-language elem)))
         ;; Also detect language from buffer text if not in properties
         (detected-lang (or language
                            (save-excursion
                              (goto-char start)
                              (when (looking-at "[ \t]*\\(?:```\\|~~~\\)\\([a-zA-Z0-9_+-]*\\)")
                                (match-string-no-properties 1))))))
    (if (and (stringp detected-lang)
             (string-equal (downcase detected-lang) "mermaid"))
        ;; Mermaid diagram
        (mark-graf-render--mermaid elem)
      ;; Regular code block
      (save-excursion
        (goto-char start)
        ;; Match opening fence - allow up to 3 spaces indent per CommonMark spec
        ;; Note: Use \r? to handle Windows CRLF line endings
        (when (looking-at "\\([ \t]*\\)\\(```\\|~~~\\)\\([a-zA-Z0-9_+-]*\\)?[ \t]*\r?$")
        (let* ((fence-line-end (line-end-position))
               ;; Get language - strip text properties that might interfere with display
               (raw-lang (or language (match-string 3) ""))
               (_lang (if (stringp raw-lang)
                          (substring-no-properties raw-lang)
                        ""))
               (content-start (1+ fence-line-end))  ; Start of next line
               (content-end (save-excursion
                              (goto-char end)
                              ;; Search backward from end for closing fence
                              ;; Use fence-line-end as limit to not find opening fence
                              (if (re-search-backward "^[ \t]*\\(```\\|~~~\\)[ \t]*\r?$" fence-line-end t)
                                  (match-beginning 0)
                                end)))
               (closing-fence-start content-end))

          ;; Hide opening fence line but preserve the newline
          ;; This prevents the previous line from merging with code content
          (let ((ov (mark-graf-render--get-overlay start fence-line-end)))
            (overlay-put ov 'mark-graf-type 'code-fence)
            (overlay-put ov 'priority 100)
            (overlay-put ov 'display ""))

          ;; Apply background per-line, starting at bol so the face
          ;; extends under the line-prefix margin area
          (when (< content-start content-end)
            (save-excursion
              (goto-char content-start)
              (while (< (point) content-end)
                (let* ((bol (line-beginning-position))
                       (eol (line-end-position))
                       (line-end (min (1+ eol) content-end)))
                  (let ((ov (mark-graf-render--get-overlay bol line-end)))
                    (overlay-put ov 'face 'mark-graf-code-block)
                    (overlay-put ov 'mark-graf-type 'code-block-content)
                    (overlay-put ov 'priority 10)))
                (forward-line 1))))

          ;; Hide closing fence
          (when (< closing-fence-start end)
            (let ((ov (mark-graf-render--get-overlay closing-fence-start end)))
              (overlay-put ov 'display "")
              (overlay-put ov 'mark-graf-type 'code-fence)
              (overlay-put ov 'priority 100)))))))))

(defun mark-graf-render--highlight-code (start end language)
  "Apply syntax highlighting to code from START to END for LANGUAGE."
  (let* ((mode (mark-graf-render--language-to-mode language))
         (text (buffer-substring-no-properties start end)))
    (when mode
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks
          (condition-case nil
              (funcall mode)
            (error nil)))
        (font-lock-ensure)
        ;; Copy fontification back
        (let ((pos 1)
              (source-start start))
          (while (< pos (point-max))
            (let* ((next-change (or (next-property-change pos) (point-max)))
                   (face (get-text-property pos 'face)))
              (when face
                (let ((ov (mark-graf-render--get-overlay
                           (+ source-start (1- pos))
                           (+ source-start (1- next-change)))))
                  (overlay-put ov 'face face)
                  (overlay-put ov 'mark-graf-type 'code-highlight)))
              (setq pos next-change))))))))

(defconst mark-graf-render--language-mode-alist
  '(("elisp" . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("python" . python-mode)
    ("py" . python-mode)
    ("javascript" . js-mode)
    ("js" . js-mode)
    ("typescript" . typescript-ts-mode)
    ("ts" . typescript-ts-mode)
    ("rust" . rust-ts-mode)
    ("go" . go-ts-mode)
    ("golang" . go-ts-mode)
    ("c" . c-mode)
    ("cpp" . c++-mode)
    ("c++" . c++-mode)
    ("java" . java-mode)
    ("ruby" . ruby-mode)
    ("rb" . ruby-mode)
    ("shell" . sh-mode)
    ("bash" . sh-mode)
    ("sh" . sh-mode)
    ("zsh" . sh-mode)
    ("json" . js-mode)
    ("yaml" . yaml-mode)
    ("yml" . yaml-mode)
    ("html" . html-mode)
    ("css" . css-mode)
    ("sql" . sql-mode)
    ("xml" . xml-mode)
    ("lisp" . lisp-mode)
    ("clojure" . clojure-mode)
    ("clj" . clojure-mode)
    ("haskell" . haskell-mode)
    ("hs" . haskell-mode)
    ("ocaml" . tuareg-mode)
    ("ml" . tuareg-mode)
    ("lua" . lua-mode)
    ("perl" . perl-mode)
    ("php" . php-mode)
    ("r" . ess-r-mode)
    ("scala" . scala-mode)
    ("swift" . swift-mode)
    ("kotlin" . kotlin-mode)
    ("objc" . objc-mode)
    ("objective-c" . objc-mode)
    ("diff" . diff-mode)
    ("makefile" . makefile-mode)
    ("make" . makefile-mode)
    ("dockerfile" . dockerfile-mode)
    ("docker" . dockerfile-mode)
    ("toml" . toml-mode)
    ("ini" . conf-mode)
    ("conf" . conf-mode)
    ("tex" . latex-mode)
    ("latex" . latex-mode))
  "Alist mapping language identifiers to Emacs major modes.")

(defun mark-graf-render--language-to-mode (language)
  "Get Emacs major mode for LANGUAGE identifier."
  (let* ((lang-lower (downcase language))
         (mode (cdr (assoc lang-lower mark-graf-render--language-mode-alist))))
    (when (and mode (fboundp mode))
      mode)))

(defun mark-graf-render--blockquote (elem)
  "Render blockquote element ELEM.
Uses simple styling that works well with visual-line-mode wrapping.
Also handles list items inside blockquotes."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem))
        ;; Get the base indentation from the buffer's line-prefix
        (base-indent (or (and (boundp 'mark-graf-left-margin)
                              (make-string mark-graf-left-margin ?\s))
                         "")))
    ;; Process each line
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (when (looking-at "^\\(>+\\)[ \t]?")
          (let* ((marker-start (match-beginning 1))
                 (marker-end (match-end 0))
                 (level (length (match-string 1)))
                 (eol (line-end-position))
                 ;; Build the prefix string for the blockquote bar
                 (bar-str (propertize (concat (make-string level mark-graf-render--blockquote-char) " ")
                                      'face 'mark-graf-blockquote-marker))
                 ;; Full wrap prefix includes base indentation
                 (full-bar-str (concat base-indent bar-str)))
            ;; Replace > markers with styled block character
            (let ((ov (mark-graf-render--get-overlay marker-start marker-end)))
              (overlay-put ov 'display bar-str)
              (overlay-put ov 'mark-graf-type 'quote-marker))
            ;; Check if this line has a list item inside the blockquote
            (when (< marker-end eol)
              (goto-char marker-end)
              (cond
               ;; Unordered list item: > - item or > * item
               ((looking-at "\\([-*+]\\)[ \t]+")
                (let* ((list-marker-start (match-beginning 1))
                       (list-marker-end (match-end 1))
                       (content-start (match-end 0))
                       (bullet (nth 0 mark-graf-render--bullet-chars))
                       ;; Wrap prefix: base indent + bar + space for bullet alignment
                       (wrap-str (concat full-bar-str "  ")))
                  ;; Replace - with bullet
                  (let ((ov (mark-graf-render--get-overlay list-marker-start list-marker-end)))
                    (overlay-put ov 'display (propertize (string bullet)
                                                         'face 'mark-graf-list-bullet))
                    (overlay-put ov 'mark-graf-type 'list-marker))
                  ;; Apply face to content with wrap-prefix for line continuation
                  (let ((ov (mark-graf-render--get-overlay content-start eol)))
                    (overlay-put ov 'face 'mark-graf-blockquote)
                    (overlay-put ov 'wrap-prefix wrap-str)
                    (overlay-put ov 'mark-graf-type 'blockquote-list-content))))
               ;; Ordered list item: > 1. item
               ((looking-at "\\([0-9]+\\)[.)][ \t]+")
                (let* ((num-start (match-beginning 1))
                       (num-end (match-end 0))
                       (content-start (match-end 0))
                       ;; Wrap prefix: base indent + bar + space for number alignment
                       (wrap-str (concat full-bar-str "   ")))
                  ;; Style the number
                  (let ((ov (mark-graf-render--get-overlay num-start num-end)))
                    (overlay-put ov 'face 'mark-graf-list-number)
                    (overlay-put ov 'mark-graf-type 'list-marker))
                  ;; Apply face to content with wrap-prefix
                  (let ((ov (mark-graf-render--get-overlay content-start eol)))
                    (overlay-put ov 'face 'mark-graf-blockquote)
                    (overlay-put ov 'wrap-prefix wrap-str)
                    (overlay-put ov 'mark-graf-type 'blockquote-list-content))))
               ;; Regular blockquote content (no list)
               (t
                (let ((ov (mark-graf-render--get-overlay marker-end eol)))
                  (overlay-put ov 'face 'mark-graf-blockquote)
                  ;; Wrap prefix shows bar on continuation lines
                  (overlay-put ov 'wrap-prefix full-bar-str)
                  (overlay-put ov 'mark-graf-type 'blockquote-content)))))))
        (forward-line 1)))))

(defun mark-graf-render--list (elem)
  "Render list element ELEM."
  ;; Lists are containers, render children
  (dolist (child (mark-graf-ts--children elem))
    (mark-graf-render--render-element child)))

(defun mark-graf-render--list-item (elem)
  "Render list item element ELEM.
Includes wrap-prefix for proper line continuation."
  (let ((start (mark-graf-node-start elem))
        (_end (mark-graf-node-end elem))
        ;; Get the base indentation from the buffer's line-prefix
        (base-indent (or (and (boundp 'mark-graf-left-margin)
                              (make-string mark-graf-left-margin ?\s))
                         "")))
    (save-excursion
      (goto-char start)
      (cond
       ;; Task list item
       ((looking-at "^\\([ \t]*\\)\\([-*+]\\)[ \t]+\\(\\[[ xX]\\]\\)[ \t]+")
        (let* ((indent (match-string 1))
               (marker-start (match-beginning 2))
               (marker-end (match-end 2))
               (checkbox-start (match-beginning 3))
               (checkbox-end (match-end 3))
               (content-start (match-end 0))
               (checkbox-text (match-string 3))
               (is-checked (string-match-p "[xX]" checkbox-text))
               (_level (/ (length indent) 2))
               (eol (line-end-position))
               ;; Wrap prefix: base indent + list indent + space for checkbox alignment
               (wrap-str (concat base-indent indent "   ")))
          ;; Hide list marker
          (let ((ov (mark-graf-render--get-overlay marker-start (1+ marker-end))))
            (overlay-put ov 'display "")
            (overlay-put ov 'mark-graf-type 'list-marker))
          ;; Render checkbox
          (let ((ov (mark-graf-render--get-overlay checkbox-start checkbox-end)))
            (overlay-put ov 'display
                         (propertize
                          (string (cdr (assq (if is-checked 'checked 'unchecked)
                                            mark-graf-render--checkbox-chars)))
                          'face (if is-checked
                                    'mark-graf-task-checked
                                  'mark-graf-task-unchecked)))
            (overlay-put ov 'mark-graf-type 'checkbox)
            (overlay-put ov 'mark-graf-checkbox-state (if is-checked 'checked 'unchecked)))
          ;; Add wrap-prefix to content
          (when (< content-start eol)
            (let ((ov (mark-graf-render--get-overlay content-start eol)))
              (overlay-put ov 'wrap-prefix wrap-str)
              (overlay-put ov 'mark-graf-type 'list-content)))))
       ;; Unordered list item
       ((looking-at "^\\([ \t]*\\)\\([-*+]\\)[ \t]+")
        (let* ((indent (match-string 1))
               (marker-start (match-beginning 2))
               (marker-end (match-end 2))
               (content-start (match-end 0))
               (level (/ (length indent) 2))
               (bullet (nth (mod level (length mark-graf-render--bullet-chars))
                           mark-graf-render--bullet-chars))
               (eol (line-end-position))
               ;; Wrap prefix: base indent + list indent + space for bullet alignment
               (wrap-str (concat base-indent indent "  ")))
          ;; Replace marker with bullet
          (let ((ov (mark-graf-render--get-overlay marker-start marker-end)))
            (overlay-put ov 'display (propertize (string bullet)
                                                'face 'mark-graf-list-bullet))
            (overlay-put ov 'mark-graf-type 'list-marker))
          ;; Add wrap-prefix to content
          (when (< content-start eol)
            (let ((ov (mark-graf-render--get-overlay content-start eol)))
              (overlay-put ov 'wrap-prefix wrap-str)
              (overlay-put ov 'mark-graf-type 'list-content)))))
       ;; Ordered list item
       ((looking-at "^\\([ \t]*\\)\\([0-9]+\\)\\([.):]\\)[ \t]+")
        (let* ((indent (match-string 1))
               (number (match-string 2))
               (marker-start (match-beginning 2))
               (marker-end (match-end 3))
               (content-start (match-end 0))
               (eol (line-end-position))
               ;; Wrap prefix: base indent + list indent + space for number alignment
               (wrap-str (concat base-indent indent (make-string (+ 2 (length number)) ?\s))))
          ;; Style number
          (let ((ov (mark-graf-render--get-overlay marker-start marker-end)))
            (overlay-put ov 'face 'mark-graf-list-number)
            (overlay-put ov 'mark-graf-type 'list-marker))
          ;; Add wrap-prefix to content
          (when (< content-start eol)
            (let ((ov (mark-graf-render--get-overlay content-start eol)))
              (overlay-put ov 'wrap-prefix wrap-str)
              (overlay-put ov 'mark-graf-type 'list-content)))))))))

(defun mark-graf-render--hr (elem)
  "Render horizontal rule element ELEM.
Uses a conservative fixed count to ensure the rule never wraps."
  (let* ((start (mark-graf-node-start elem))
         (end (mark-graf-node-end elem))
         ;; Use a fixed safe count.  Box-drawing chars like ─ can render
         ;; wider than Emacs reports, especially on Windows.  40 chars is
         ;; safely under any reasonable column width.
         (count 40))
    (let ((ov (mark-graf-render--get-overlay start end)))
      (overlay-put ov 'display
                   (propertize (make-string count mark-graf-render--hr-char)
                              'face 'mark-graf-hr))
      (overlay-put ov 'mark-graf-type 'hr))))

(defun mark-graf-render--process-cell-text (text base-face)
  "Process TEXT for inline formatting like **bold**, with BASE-FACE as default."
  (let ((result (propertize text 'face base-face)))
    ;; Process bold **text** -> text with bold added
    (while (string-match "\\*\\*\\([^*]+\\)\\*\\*" result)
      (let* ((bold-text (match-string 1 result))
             (styled (propertize bold-text 'face (list :inherit base-face :weight 'bold))))
        (setq result (replace-match styled t t result))))
    result))

(defun mark-graf-render--table-parse-row (line)
  "Parse LINE into list of cell contents (without | delimiters)."
  (when (string-match "^[ \t]*|\\(.*\\)|[ \t]*$" line)
    (let ((inner (match-string 1 line)))
      (split-string inner "|" nil))))

(defun mark-graf-render--strip-markdown (text)
  "Strip markdown formatting markers from TEXT."
  (let ((result text))
    (setq result (string-replace "**" "" result))
    (setq result (string-replace "__" "" result))
    (setq result (string-replace "`" "" result))
    result))

(defcustom mark-graf-table-max-width nil
  "Maximum total width for rendered tables.
If nil, automatically uses window width minus margins."
  :type '(choice (const :tag "Auto (window width)" nil)
                 (integer :tag "Fixed width"))
  :group 'mark-graf)

(defun mark-graf-render--table-display-width ()
  "Get the available display width for tables.
Uses `window-body-width' of the window displaying the current buffer,
falling back to 80 if the buffer is not yet displayed.
Subtracts `line-prefix' width because Emacs applies line-prefix to
each visual line within an overlay's display string, which would
otherwise cause overflow and garbled wrapping in visual-line-mode."
  (let* ((win (get-buffer-window (current-buffer)))
         (raw-width (if win
                       (max 40 (window-body-width win))
                     (or mark-graf-table-max-width 80)))
         (prefix-width (if (and (local-variable-p 'line-prefix)
                                line-prefix
                                (stringp line-prefix))
                           (string-width line-prefix)
                         0)))
    (max 40 (- raw-width prefix-width))))

(defun mark-graf-render--table-column-widths (rows)
  "Calculate column widths from ROWS, respecting max table width.
Uses window width if `mark-graf-table-max-width' is nil."
  (let ((widths nil)
        (num-cols 0)
        (max-width (or mark-graf-table-max-width
                       (mark-graf-render--table-display-width))))
    ;; First pass: get natural widths (using string-width for display accuracy)
    (dolist (row rows)
      (setq num-cols (max num-cols (length row)))
      (let ((col 0))
        (dolist (cell row)
          (let* ((trimmed (string-trim cell))
                 (is-sep (string-match-p "^[-:|]+$" trimmed))
                 (content (if is-sep "" (mark-graf-render--strip-markdown trimmed)))
                 (cell-width (string-width content)))
            (if (nth col widths)
                (when (> cell-width (nth col widths))
                  (setf (nth col widths) cell-width))
              (setq widths (append widths (list cell-width)))))
          (setq col (1+ col)))))
    ;; Ensure minimum width of 5 per column
    (setq widths (mapcar (lambda (w) (max 5 (or w 5))) widths))
    ;; Scale down if total exceeds available space
    ;; border-overhead: (num-cols + 1) border chars + 2 spaces per column
    (let* ((border-overhead (+ 1 num-cols (* 2 num-cols)))
           (available (max num-cols (- max-width border-overhead)))
           (total (apply #'+ widths)))
      (when (> total available)
        ;; Scale proportionally, minimum 5 chars per column
        (let ((scale (/ (float available) (float total))))
          (setq widths (mapcar (lambda (w) (max 5 (floor (* w scale)))) widths))))
      ;; Final safety: if total still exceeds, force equal distribution
      (let ((new-total (apply #'+ widths)))
        (when (> new-total available)
          (let ((per-col (max 5 (/ available num-cols))))
            (setq widths (mapcar (lambda (_) per-col) widths))))))
    widths))

(defun mark-graf-render--wrap-text (text width)
  "Wrap TEXT to WIDTH, returning list of lines.
Guarantees all returned lines are at most WIDTH characters.
Forces breaks on long words when no space is found."
  (let ((trimmed (string-trim text)))
    (if (or (zerop (length trimmed)) (<= width 0))
        (list "")
      (let ((lines nil)
            (remaining trimmed))
        ;; Keep breaking while remaining text is longer than width
        (while (> (length remaining) width)
          (let* ((chunk (substring remaining 0 width))
                 (last-space (string-match-p " [^ ]*$" chunk)))
            (if (and last-space (> last-space 0))
                ;; Break at last space within width
                (progn
                  (push (substring remaining 0 last-space) lines)
                  (setq remaining (string-trim-left (substring remaining last-space))))
              ;; No space in chunk - force break at exactly width
              (push chunk lines)
              (setq remaining (string-trim-left (substring remaining width))))))
        ;; Add final piece (guaranteed <= width)
        (when (> (length remaining) 0)
          (push remaining lines))
        (or (nreverse lines) (list ""))))))

(defun mark-graf-render--table-truncate-to-width (str width)
  "Truncate STR to fit in WIDTH display columns.
Uses `string-width' for accurate multi-byte character handling."
  (if (<= (string-width str) width)
      str
    ;; Binary search for the right truncation point
    (let ((lo 0) (hi (length str)))
      (while (< (1+ lo) hi)
        (let ((mid (/ (+ lo hi) 2)))
          (if (<= (string-width (substring str 0 mid)) (- width 1))
              (setq lo mid)
            (setq hi mid))))
      (concat (substring str 0 lo) "…"))))

(defun mark-graf-render--table-format-row (cells widths is-separator)
  "Format CELLS with WIDTHS. Returns single-line string, truncating if needed."
  (let ((num-cols (length widths)))
    (if is-separator
        ;; Separator row - single line
        (concat "├" (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths "┼") "┤")
      ;; Data row - truncate cells that exceed width
      (let ((parts nil))
        (dotimes (col num-cols)
          (let* ((width (or (nth col widths) 15))
                 (cell (or (nth col cells) ""))
                 (content (mark-graf-render--strip-markdown (string-trim cell)))
                 (cw (string-width content))
                 (truncated (if (> cw width)
                                (mark-graf-render--table-truncate-to-width content width)
                              content))
                 (pad-needed (max 0 (- width (string-width truncated))))
                 (padded (concat " " truncated (make-string pad-needed ?\s) " ")))
            (push padded parts)))
        (concat "│" (mapconcat #'identity (nreverse parts) "│") "│")))))

(defun mark-graf-render--table (elem)
  "Render table element ELEM using per-row overlays for reliable display.
Always finds the COMPLETE table by scanning beyond element boundaries."
  (let ((start (mark-graf-node-start elem))
        (rows nil)
        (row-info nil))
    (save-excursion
      ;; Find the true start of the table (scan backwards)
      (goto-char start)
      (beginning-of-line)
      (while (and (not (bobp))
                  (save-excursion
                    (forward-line -1)
                    (looking-at "^[ \t]*|.+|[ \t]*$")))
        (forward-line -1))
      ;; Now scan forward to collect ALL rows (no end limit)
      (let ((is-header t)
            (seen-separator nil)
            (keep-going t))
        (while (and keep-going (looking-at "^[ \t]*|.+|[ \t]*$"))
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (line (buffer-substring-no-properties bol eol))
                 (cells (mark-graf-render--table-parse-row line))
                 (is-sep (and cells (string-match-p "^[ \t]*|[-:|]+|" line))))
            (when cells
              (push cells rows)
              (push (list bol eol is-sep
                          (and is-header (not is-sep) (not seen-separator)))
                    row-info)
              (when is-sep (setq seen-separator t))
              (when (and (not is-sep) seen-separator)
                (setq is-header nil))))
          (if (= (forward-line 1) 1)
              (setq keep-going nil))))
      (setq rows (nreverse rows))
      (setq row-info (nreverse row-info))
      ;; Clear overlays in the full table region (properly tracked)
      (when row-info
        (let ((table-start (nth 0 (car row-info)))
              (table-end (nth 1 (car (last row-info)))))
          (mark-graf-render--clear-region table-start table-end)))
      ;; Create per-row overlays
      (when (and rows row-info)
        (let ((widths (mark-graf-render--table-column-widths rows)))
          (when widths
            (mark-graf-render--table-create-overlays
             rows row-info widths)))))))

(defun mark-graf-render--table-create-overlays (rows row-info widths)
  "Create per-row overlays for table ROWS with ROW-INFO and WIDTHS.
Each source line gets its own overlay, avoiding multi-line display issues."
  (let* ((max-w (mark-graf-render--table-display-width))
         (top-border (propertize
                      (concat "┌" (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths "┬") "┐")
                      'face 'mark-graf-table))
         (bottom-border (propertize
                         (concat "└" (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths "┴") "┘")
                         'face 'mark-graf-table))
         (row-separator (propertize
                         (concat "├" (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths "┼") "┤")
                         'face 'mark-graf-table))
         (num-rows (length row-info))
         (row-idx 0))
    (dolist (info row-info)
      (let* ((bol (nth 0 info))
             (eol (nth 1 info))
             (is-sep (nth 2 info))
             (is-hdr (nth 3 info))
             (is-first (= row-idx 0))
             (is-last (= row-idx (1- num-rows)))
             (next-is-sep (and (< (1+ row-idx) num-rows)
                               (nth 2 (nth (1+ row-idx) row-info))))
             (cells (nth row-idx rows))
             (formatted (mark-graf-render--table-format-row cells widths is-sep))
             (face (if is-hdr 'mark-graf-table-header 'mark-graf-table))
             (parts nil))
        ;; Top border before first row
        (when is-first
          (push (mark-graf-render--table-truncate-to-width top-border max-w) parts)
          (push "\n" parts))
        ;; Row content (single line, safety-truncated to display width)
        (let ((safe-row (mark-graf-render--table-truncate-to-width formatted max-w)))
          (push (propertize safe-row 'face face) parts))
        (push "\n" parts)
        ;; After-row: bottom border on last, separator between data rows
        (cond
         (is-last
          (push (mark-graf-render--table-truncate-to-width bottom-border max-w) parts)
          (push "\n" parts))
         ((and (not is-sep) (not next-is-sep))
          (push (mark-graf-render--table-truncate-to-width row-separator max-w) parts)
          (push "\n" parts)))
        ;; Create overlay covering this source line + its newline
        (let* ((ov-end (min (1+ eol) (point-max)))
               (display-str (apply #'concat (nreverse parts)))
               (ov (mark-graf-render--get-overlay bol ov-end)))
          (overlay-put ov 'display display-str)
          (overlay-put ov 'priority 200)
          (overlay-put ov 'mark-graf-type 'table)
          ;; Override buffer-local line-prefix/wrap-prefix with empty
          ;; strings.  Emacs applies these to every visual line inside
          ;; an overlay's display string; the buffer-local 4-space
          ;; indent would push the table past window-body-width and
          ;; visual-line-mode wraps each row, garbling the table.
          (overlay-put ov 'line-prefix "")
          (overlay-put ov 'wrap-prefix "")))
      (setq row-idx (1+ row-idx)))))

(defun mark-graf-render--table-row (row)
  "Render a single table ROW."
  (let ((start (plist-get row :start))
        (end (plist-get row :end))
        (is-header (plist-get row :is-header)))
    (save-excursion
      (goto-char start)
      ;; Transform | characters to box-drawing characters
      (while (re-search-forward "|" (1+ end) t)
        (let ((ov (mark-graf-render--get-overlay (1- (point)) (point))))
          (overlay-put ov 'display
                       (propertize (string (cdr (assq 'vertical mark-graf-render--table-chars)))
                                  'face 'mark-graf-table-border))
          (overlay-put ov 'mark-graf-type 'table-border)))
      ;; Apply header face if needed
      (when is-header
        (let ((ov (mark-graf-render--get-overlay start end)))
          (overlay-put ov 'face 'mark-graf-table-header)
          (overlay-put ov 'mark-graf-type 'table-header)
          (overlay-put ov 'priority -5))))))

(defun mark-graf-render--table-row-standalone (elem)
  "Render a standalone table row element ELEM from fallback parser."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      ;; Apply table background - include newline for :extend to work
      (let* ((eol (line-end-position))
             (end-with-nl (min (1+ eol) (point-max)))
             (ov (mark-graf-render--get-overlay start end-with-nl)))
        (overlay-put ov 'face 'mark-graf-table)
        (overlay-put ov 'mark-graf-type 'table-row)
        (overlay-put ov 'priority -10))
      ;; Transform | characters to box-drawing
      (while (re-search-forward "|" end t)
        (let ((ov (mark-graf-render--get-overlay (1- (point)) (point))))
          (overlay-put ov 'display
                       (propertize "│" 'face 'mark-graf-table-border))
          (overlay-put ov 'mark-graf-type 'table-border))))))

(defun mark-graf-render--table-separator (elem)
  "Render a table separator row - display as horizontal line."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      ;; Count cells by counting | characters
      (let* ((row-text (buffer-substring-no-properties start end))
             (cell-count (1- (length (split-string row-text "|" t))))
             (line-str (concat "├" (mapconcat (lambda (_) "────────") (number-sequence 1 cell-count) "┼") "┤")))
        (let ((ov (mark-graf-render--get-overlay start end)))
          (overlay-put ov 'display (propertize line-str 'face 'mark-graf-table-border))
          (overlay-put ov 'mark-graf-type 'table-separator))))))

;;; Extended Element Rendering

(defun mark-graf-render--footnote-ref (elem)
  "Render footnote reference element ELEM."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "\\[\\^\\([^]]+\\)\\]")
        (let ((ref (match-string 1)))
          ;; Display as superscript
          (let ((ov (mark-graf-render--get-overlay start end)))
            (overlay-put ov 'display
                         (propertize ref 'face 'mark-graf-footnote-ref
                                    'display '(raise 0.3)))
            (overlay-put ov 'help-echo (format "Footnote: %s" ref))
            (overlay-put ov 'mark-graf-type 'footnote-ref)))))))

;;; LaTeX-to-Unicode Conversion Tables

(defconst mark-graf-render--latex-symbols
  '(;; Greek lowercase
    ("\\alpha" . "α") ("\\beta" . "β") ("\\gamma" . "γ") ("\\delta" . "δ")
    ("\\epsilon" . "ε") ("\\varepsilon" . "ε") ("\\zeta" . "ζ") ("\\eta" . "η")
    ("\\theta" . "θ") ("\\vartheta" . "ϑ") ("\\iota" . "ι") ("\\kappa" . "κ")
    ("\\lambda" . "λ") ("\\mu" . "μ") ("\\nu" . "ν") ("\\xi" . "ξ")
    ("\\pi" . "π") ("\\varpi" . "ϖ") ("\\rho" . "ρ") ("\\varrho" . "ϱ")
    ("\\sigma" . "σ") ("\\varsigma" . "ς") ("\\tau" . "τ") ("\\upsilon" . "υ")
    ("\\phi" . "φ") ("\\varphi" . "φ") ("\\chi" . "χ") ("\\psi" . "ψ")
    ("\\omega" . "ω")
    ;; Greek uppercase
    ("\\Gamma" . "Γ") ("\\Delta" . "Δ") ("\\Theta" . "Θ") ("\\Lambda" . "Λ")
    ("\\Xi" . "Ξ") ("\\Pi" . "Π") ("\\Sigma" . "Σ") ("\\Upsilon" . "Υ")
    ("\\Phi" . "Φ") ("\\Psi" . "Ψ") ("\\Omega" . "Ω")
    ;; Operators
    ("\\int" . "∫") ("\\iint" . "∬") ("\\iiint" . "∭")
    ("\\oint" . "∮") ("\\sum" . "∑") ("\\prod" . "∏")
    ("\\partial" . "∂") ("\\nabla" . "∇") ("\\infty" . "∞")
    ("\\cdot" . "·") ("\\times" . "×") ("\\pm" . "±") ("\\mp" . "∓")
    ("\\div" . "÷") ("\\star" . "⋆") ("\\circ" . "∘") ("\\bullet" . "∙")
    ("\\oplus" . "⊕") ("\\otimes" . "⊗")
    ;; Relations
    ("\\leq" . "≤") ("\\le" . "≤") ("\\geq" . "≥") ("\\ge" . "≥")
    ("\\neq" . "≠") ("\\ne" . "≠") ("\\approx" . "≈") ("\\equiv" . "≡")
    ("\\sim" . "∼") ("\\simeq" . "≃") ("\\cong" . "≅") ("\\propto" . "∝")
    ("\\ll" . "≪") ("\\gg" . "≫") ("\\prec" . "≺") ("\\succ" . "≻")
    ;; Set theory
    ("\\in" . "∈") ("\\notin" . "∉") ("\\ni" . "∋")
    ("\\subset" . "⊂") ("\\supset" . "⊃")
    ("\\subseteq" . "⊆") ("\\supseteq" . "⊇")
    ("\\cup" . "∪") ("\\cap" . "∩")
    ("\\emptyset" . "∅") ("\\varnothing" . "∅")
    ;; Arrows
    ("\\to" . "→") ("\\rightarrow" . "→") ("\\leftarrow" . "←")
    ("\\leftrightarrow" . "↔")
    ("\\Rightarrow" . "⇒") ("\\Leftarrow" . "⇐")
    ("\\Leftrightarrow" . "⇔") ("\\iff" . "⇔")
    ("\\mapsto" . "↦") ("\\uparrow" . "↑") ("\\downarrow" . "↓")
    ;; Logic
    ("\\forall" . "∀") ("\\exists" . "∃") ("\\nexists" . "∄")
    ("\\neg" . "¬") ("\\lnot" . "¬")
    ("\\wedge" . "∧") ("\\land" . "∧")
    ("\\vee" . "∨") ("\\lor" . "∨")
    ("\\top" . "⊤") ("\\bot" . "⊥") ("\\vdash" . "⊢") ("\\models" . "⊨")
    ;; Dots
    ("\\ldots" . "…") ("\\cdots" . "⋯") ("\\vdots" . "⋮") ("\\ddots" . "⋱")
    ("\\dots" . "…")
    ;; Misc symbols
    ("\\hbar" . "ℏ") ("\\ell" . "ℓ") ("\\Re" . "ℜ") ("\\Im" . "ℑ")
    ("\\aleph" . "ℵ") ("\\wp" . "℘")
    ("\\angle" . "∠") ("\\triangle" . "△")
    ("\\diamond" . "⋄") ("\\langle" . "⟨") ("\\rangle" . "⟩")
    ;; Spacing commands (replace with appropriate space or nothing)
    ("\\quad" . " ") ("\\qquad" . "  ") ("\\," . " ") ("\\;" . " ")
    ("\\!" . "") ("\\:" . " ") ("\\ " . " "))
  "Alist mapping LaTeX commands to Unicode characters.")

(defconst mark-graf-render--latex-superscripts
  '((?0 . ?⁰) (?1 . ?¹) (?2 . ?²) (?3 . ?³) (?4 . ?⁴)
    (?5 . ?⁵) (?6 . ?⁶) (?7 . ?⁷) (?8 . ?⁸) (?9 . ?⁹)
    (?+ . ?⁺) (?- . ?⁻) (?= . ?⁼) (?\( . ?⁽) (?\) . ?⁾)
    (?n . ?ⁿ) (?i . ?ⁱ) (?x . ?ˣ) (?y . ?ʸ)
    (?a . ?ᵃ) (?b . ?ᵇ) (?c . ?ᶜ) (?d . ?ᵈ) (?e . ?ᵉ)
    (?f . ?ᶠ) (?g . ?ᵍ) (?h . ?ʰ) (?j . ?ʲ) (?k . ?ᵏ)
    (?l . ?ˡ) (?m . ?ᵐ) (?o . ?ᵒ) (?p . ?ᵖ) (?r . ?ʳ)
    (?s . ?ˢ) (?t . ?ᵗ) (?u . ?ᵘ) (?v . ?ᵛ) (?w . ?ʷ) (?z . ?ᶻ)
    (?A . ?ᴬ) (?B . ?ᴮ) (?D . ?ᴰ) (?E . ?ᴱ) (?G . ?ᴳ)
    (?H . ?ᴴ) (?I . ?ᴵ) (?J . ?ᴶ) (?K . ?ᴷ) (?L . ?ᴸ)
    (?M . ?ᴹ) (?N . ?ᴺ) (?O . ?ᴼ) (?P . ?ᴾ) (?R . ?ᴿ)
    (?T . ?ᵀ) (?U . ?ᵁ) (?V . ?ⱽ) (?W . ?ᵂ))
  "Alist mapping characters to their Unicode superscript equivalents.")

(defconst mark-graf-render--latex-subscripts
  '((?0 . ?₀) (?1 . ?₁) (?2 . ?₂) (?3 . ?₃) (?4 . ?₄)
    (?5 . ?₅) (?6 . ?₆) (?7 . ?₇) (?8 . ?₈) (?9 . ?₉)
    (?+ . ?₊) (?- . ?₋) (?= . ?₌) (?\( . ?₍) (?\) . ?₎)
    (?a . ?ₐ) (?e . ?ₑ) (?h . ?ₕ) (?i . ?ᵢ) (?j . ?ⱼ)
    (?k . ?ₖ) (?l . ?ₗ) (?m . ?ₘ) (?n . ?ₙ) (?o . ?ₒ)
    (?p . ?ₚ) (?r . ?ᵣ) (?s . ?ₛ) (?t . ?ₜ) (?u . ?ᵤ)
    (?v . ?ᵥ) (?x . ?ₓ))
  "Alist mapping characters to their Unicode subscript equivalents.")

(defun mark-graf-render--latex-convert-scripts (str map prefix)
  "Convert characters in STR using MAP (super/subscript alist).
PREFIX is the script marker (\"^\" or \"_\") used for unconvertible chars."
  (let ((result "")
        (i 0)
        (len (length str)))
    (while (< i len)
      (let* ((ch (aref str i))
             (mapped (cdr (assq ch map))))
        (if mapped
            (setq result (concat result (string mapped)))
          ;; No mapping: keep prefix + char for visibility
          (setq result (concat result prefix (string ch)))))
      (setq i (1+ i)))
    result))

(defun mark-graf-render--latex-to-unicode (latex-str)
  "Convert LATEX-STR to Unicode representation.
Handles symbols, superscripts, subscripts, sqrt, frac, and text commands."
  (let ((s latex-str)
        (case-fold-search nil))
    ;; 1. Replace \sqrt{...} → √(...) and \sqrt x → √x
    (let ((pos 0))
      (while (string-match "\\\\sqrt{\\([^}]*\\)}" s pos)
        (let ((repl (concat "√(" (match-string 1 s) ")")))
          (setq s (replace-match repl t t s))
          (setq pos (+ (match-beginning 0) (length repl))))))
    (let ((pos 0))
      (while (string-match "\\\\sqrt \\([a-zA-Z0-9]\\)" s pos)
        (let ((repl (concat "√" (match-string 1 s))))
          (setq s (replace-match repl t t s))
          (setq pos (+ (match-beginning 0) (length repl))))))

    ;; 2. Replace \frac{a}{b} → a⁄b
    (let ((pos 0))
      (while (string-match "\\\\frac{\\([^}]*\\)}{\\([^}]*\\)}" s pos)
        (let ((repl (concat (match-string 1 s) "⁄" (match-string 2 s))))
          (setq s (replace-match repl t t s))
          (setq pos (+ (match-beginning 0) (length repl))))))

    ;; 3. Replace \text{...}, \mathrm{...}, \operatorname{...} → just the text
    (let ((pos 0))
      (while (string-match "\\\\\\(?:text\\|mathrm\\|operatorname\\|mathit\\|mathbf\\|textbf\\){\\([^}]*\\)}" s pos)
        (let ((repl (match-string 1 s)))
          (setq s (replace-match repl t t s))
          (setq pos (+ (match-beginning 0) (length repl))))))

    ;; 4. Replace \left and \right (sizing commands) - remove them
    (setq s (replace-regexp-in-string "\\\\left\\b" "" s))
    (setq s (replace-regexp-in-string "\\\\right\\b" "" s))

    ;; 5. Replace all known \command symbols (longest match first)
    (let ((sorted-symbols (sort (copy-sequence mark-graf-render--latex-symbols)
                                (lambda (a b) (> (length (car a)) (length (car b)))))))
      (dolist (pair sorted-symbols)
        (let ((cmd (car pair))
              (uni (cdr pair)))
          ;; Match command followed by non-letter (word boundary) or end of string
          (let ((re (concat (regexp-quote cmd) "\\(?:[^a-zA-Z]\\|$\\)"))
                (pos 0))
            (while (string-match re s pos)
              (let ((end-pos (+ (match-beginning 0) (length cmd))))
                (setq s (concat (substring s 0 (match-beginning 0))
                                uni
                                (substring s end-pos)))
                (setq pos (+ (match-beginning 0) (length uni)))))))))

    ;; 6. Convert ^{...} and ^x to superscripts
    (let ((pos 0))
      (while (and (< pos (length s))
                  (string-match "\\^{\\([^}]*\\)}" s pos))
        (let* ((content (match-string 1 s))
               (repl (mark-graf-render--latex-convert-scripts
                      content mark-graf-render--latex-superscripts "")))
          (setq s (replace-match repl t t s))
          (setq pos (+ (match-beginning 0) (length repl))))))
    ;; Single char superscript: ^x (but not ^{ which was handled above)
    (let ((pos 0))
      (while (and (< pos (length s))
                  (string-match "\\^\\([^{ \t\n]\\)" s pos))
        (let* ((ch-str (match-string 1 s))
               (ch (aref ch-str 0))
               (mapped (cdr (assq ch mark-graf-render--latex-superscripts))))
          (if mapped
              (progn
                (setq s (replace-match (string mapped) t t s))
                (setq pos (+ (match-beginning 0) 1)))
            ;; No mapping available - skip past this ^ and char
            (setq pos (match-end 0))))))

    ;; 7. Convert _{...} and _x to subscripts
    (let ((pos 0))
      (while (and (< pos (length s))
                  (string-match "_{\\([^}]*\\)}" s pos))
        (let* ((content (match-string 1 s))
               (repl (mark-graf-render--latex-convert-scripts
                      content mark-graf-render--latex-subscripts "")))
          (setq s (replace-match repl t t s))
          (setq pos (+ (match-beginning 0) (length repl))))))
    ;; Single char subscript: _x
    (let ((pos 0))
      (while (and (< pos (length s))
                  (string-match "_\\([^{ \t\n]\\)" s pos))
        (let* ((ch-str (match-string 1 s))
               (ch (aref ch-str 0))
               (mapped (cdr (assq ch mark-graf-render--latex-subscripts))))
          (if mapped
              (progn
                (setq s (replace-match (string mapped) t t s))
                (setq pos (+ (match-beginning 0) 1)))
            ;; No mapping available - skip past
            (setq pos (match-end 0))))))

    ;; 8. Strip remaining bare { and }
    (setq s (replace-regexp-in-string "[{}]" "" s))

    ;; 9. Clean up extra whitespace
    (setq s (replace-regexp-in-string "  +" " " s))
    (setq s (string-trim s))

    s))

(defun mark-graf-render--math (elem)
  "Render inline math element ELEM.
Converts LaTeX to Unicode, hides $ delimiters, replaces content."
  (let* ((start (mark-graf-node-start elem))
         (end (mark-graf-node-end elem))
         (content-start (1+ start))
         (content-end (1- end)))
    (when (< content-start content-end)
      (let* ((latex (buffer-substring-no-properties content-start content-end))
             (unicode (mark-graf-render--latex-to-unicode latex))
             (display-str (propertize unicode 'face 'mark-graf-math)))
        ;; Hide opening $
        (let ((ov (mark-graf-render--get-overlay start content-start)))
          (overlay-put ov 'display "")
          (overlay-put ov 'mark-graf-type 'math-delim))
        ;; Replace content with Unicode conversion
        (let ((ov (mark-graf-render--get-overlay content-start content-end)))
          (overlay-put ov 'display display-str)
          (overlay-put ov 'mark-graf-type 'math-content)
          (overlay-put ov 'priority 110)
          (overlay-put ov 'help-echo (format "LaTeX: $%s$" latex)))
        ;; Hide closing $
        (let ((ov (mark-graf-render--get-overlay content-end end)))
          (overlay-put ov 'display "")
          (overlay-put ov 'mark-graf-type 'math-delim))))))

(defun mark-graf-render--math-block (elem)
  "Render display math block element ELEM.
Converts LaTeX to Unicode using 3-overlay approach:
hide opening $$ line, display converted content, hide closing $$ line."
  (let* ((start (mark-graf-node-start elem))
         (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      ;; Find opening $$ line end
      (when (looking-at "^\\$\\$[ \t]*\r?$")
        (let* ((open-line-end (min (1+ (line-end-position)) end))
               (content-start open-line-end)
               (close-start (save-excursion
                              (goto-char end)
                              (if (re-search-backward "^\\$\\$[ \t]*\r?$"
                                                      content-start t)
                                  (match-beginning 0)
                                end))))
          (when (< content-start close-start)
            (let* ((latex (string-trim
                           (buffer-substring-no-properties
                            content-start close-start)))
                   (unicode (mark-graf-render--latex-to-unicode latex))
                   (display-str (propertize unicode 'face 'mark-graf-math)))
              ;; Hide opening $$ line
              (let ((ov (mark-graf-render--get-overlay start open-line-end)))
                (overlay-put ov 'display "")
                (overlay-put ov 'mark-graf-type 'math-block-delim))
              ;; Replace content with Unicode conversion
              (let ((ov (mark-graf-render--get-overlay content-start close-start)))
                (overlay-put ov 'display display-str)
                (overlay-put ov 'mark-graf-type 'math-block-content)
                (overlay-put ov 'priority 110)
                (overlay-put ov 'help-echo (format "LaTeX: $$%s$$" latex))
                ;; Apply scale via face height on the overlay itself
                (when (and mark-graf-math-block-scale
                           (/= mark-graf-math-block-scale 1.0))
                  (overlay-put ov 'face
                               (list :height mark-graf-math-block-scale))))
              ;; Hide closing $$ line
              (let ((ov (mark-graf-render--get-overlay close-start end)))
                (overlay-put ov 'display "")
                (overlay-put ov 'mark-graf-type 'math-block-delim)))))))))

(defcustom mark-graf-cache-directory
  (expand-file-name "mark-graf" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory for caching rendered diagrams."
  :type 'directory
  :group 'mark-graf-media)

;;; Mermaid Diagram Rendering (SVG)

(defvar-local mark-graf-render--mermaid-cache nil
  "Hash table caching mermaid diagram SVG by content hash.")

(defun mark-graf-render--mermaid-cache ()
  "Return the mermaid SVG cache, creating if needed."
  (unless mark-graf-render--mermaid-cache
    (setq mark-graf-render--mermaid-cache (make-hash-table :test 'equal)))
  mark-graf-render--mermaid-cache)

(defun mark-graf-render--mermaid (elem)
  "Render Mermaid diagram element ELEM as inline SVG."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "^[ \t]*```mermaid[ \t]*\r?$")
        (let* ((content-start (1+ (line-end-position)))
               (content-end (save-excursion
                              (goto-char end)
                              (if (re-search-backward "^[ \t]*```[ \t]*\r?$" content-start t)
                                  (match-beginning 0)
                                end)))
               (diagram-code (buffer-substring-no-properties content-start content-end))
               (cache-key (md5 diagram-code))
               (svg-string (or (gethash cache-key (mark-graf-render--mermaid-cache))
                               (let ((svg (condition-case err
                                              (mark-graf-mermaid-render diagram-code)
                                            (error
                                             (message "mark-graf: mermaid render error: %s" (error-message-string err))
                                             nil))))
                                 (when svg
                                   (puthash cache-key svg (mark-graf-render--mermaid-cache)))
                                 svg))))
          (if svg-string
              (condition-case img-err
                  (progn
                    (message "mark-graf: rendering mermaid SVG (len=%d)" (length svg-string))
                    (mark-graf-render--display-mermaid-svg start end svg-string))
                (error
                 (message "mark-graf: mermaid SVG display error: %s" (error-message-string img-err))
                 (mark-graf-render--mermaid-source-fallback elem)))
            ;; Unsupported diagram type or error - show as styled code block
            (mark-graf-render--mermaid-source-fallback elem)))))))

(defun mark-graf-render--display-mermaid-svg (start end svg-string)
  "Display SVG-STRING as an inline image in region START to END.
Writes SVG to a temp file for reliable cross-platform image creation."
  (when (and svg-string
             (> (length svg-string) 0)
             (<= start (point-max))
             (<= end (point-max)))
    ;; Write SVG to temp file - more reliable than in-memory on Windows
    (let* ((tmp-file (make-temp-file "mark-graf-mermaid-" nil ".svg"))
           (image nil))
      (unwind-protect
          (progn
            (with-temp-file tmp-file
              (insert svg-string))
            (setq image (create-image tmp-file 'svg nil
                                      :scale 2.0
                                      :max-width (* 2 (or mark-graf-image-max-width 600))))
            (when image
              (mark-graf-render--clear-region start end)
              (let ((ov (mark-graf-render--get-overlay start end)))
                (overlay-put ov 'display image)
                (overlay-put ov 'help-echo "Mermaid diagram (SVG)")
                (overlay-put ov 'mark-graf-type 'mermaid-svg)
                (overlay-put ov 'priority 200))))
        ;; Don't delete the temp file immediately - Emacs needs it for display
        ;; Schedule deletion after a delay
        (run-with-timer 10 nil #'delete-file tmp-file)))))

(defun mark-graf-render--mermaid-source-fallback (elem)
  "Render Mermaid ELEM as styled code block for unsupported diagram types."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "^[ \t]*```mermaid[ \t]*\r?$")
        (let* ((fence-end (line-end-position))
               (closing-fence (save-excursion
                                (goto-char end)
                                (if (re-search-backward "^[ \t]*```[ \t]*\r?$" fence-end t)
                                    (match-beginning 0)
                                  end)))
               (content-start (1+ fence-end)))
          ;; Replace opening fence with mermaid label
          (let ((ov (mark-graf-render--get-overlay start fence-end)))
            (overlay-put ov 'display
                         (propertize " mermaid "
                                    'face 'mark-graf-code-block-language))
            (overlay-put ov 'mark-graf-type 'mermaid-fallback)
            (overlay-put ov 'priority 100))
          ;; Apply background to content lines
          (when (< content-start closing-fence)
            (save-excursion
              (goto-char content-start)
              (while (< (point) closing-fence)
                (let* ((bol (line-beginning-position))
                       (eol (line-end-position))
                       (line-end (min (1+ eol) closing-fence)))
                  (let ((ov (mark-graf-render--get-overlay bol line-end)))
                    (overlay-put ov 'face 'mark-graf-code-block)
                    (overlay-put ov 'mark-graf-type 'mermaid-fallback)
                    (overlay-put ov 'priority 10)))
                (forward-line 1))))
          ;; Hide closing fence
          (when (< closing-fence end)
            (let ((ov (mark-graf-render--get-overlay closing-fence end)))
              (overlay-put ov 'display "")
              (overlay-put ov 'mark-graf-type 'mermaid-fallback)
              (overlay-put ov 'priority 100))))))))

(defcustom mark-graf-math-block-scale 1.4
  "Scale factor for display math block text relative to normal font size.
A value of 1.4 means display math is shown at 140% of normal size.
Set to 1.0 for same size as body text."
  :type 'number
  :group 'mark-graf-math)

;;; Enhanced Math Rendering with SVG

(defcustom mark-graf-math-renderer 'text
  "Method for rendering LaTeX math.
`text' - Styled text display (default, no dependencies)
`svg'  - SVG rendering via tex2svg or similar
`preview' - Use preview-latex if available"
  :type '(choice (const :tag "Styled text" text)
                 (const :tag "SVG rendering" svg)
                 (const :tag "preview-latex" preview))
  :group 'mark-graf-math)

(defcustom mark-graf-tex2svg-executable "tex2svg"
  "Path to tex2svg executable (from MathJax-node)."
  :type 'string
  :group 'mark-graf-math)

(defun mark-graf-tex2svg-available-p ()
  "Check if tex2svg is available."
  (executable-find mark-graf-tex2svg-executable))

(defun mark-graf-render--math-enhanced (elem)
  "Render math ELEM with the configured renderer."
  (pcase mark-graf-math-renderer
    ('svg (if (mark-graf-tex2svg-available-p)
              (mark-graf-render--math-svg elem)
            (mark-graf-render--math elem)))
    ('preview (mark-graf-render--math-preview elem))
    (_ (mark-graf-render--math elem))))

(defun mark-graf-render--math-svg (elem)
  "Render math ELEM to SVG."
  (let ((start (mark-graf-node-start elem))
        (end (mark-graf-node-end elem)))
    (save-excursion
      (goto-char start)
      (when (looking-at "\\$\\([^$]+\\)\\$")
        (let* ((latex (match-string 1))
               (hash (md5 latex))
               (cache-path (expand-file-name
                           (format "math-%s.svg" hash)
                           mark-graf-cache-directory)))
          (if (file-exists-p cache-path)
              ;; Use cached SVG
              (let ((image (create-image cache-path 'svg nil :ascent 'center)))
                (let ((ov (mark-graf-render--get-overlay start end)))
                  (overlay-put ov 'display image)
                  (overlay-put ov 'help-echo (format "LaTeX: %s" latex))
                  (overlay-put ov 'mark-graf-type 'math-svg)))
            ;; Render and cache
            (mark-graf-render--math elem)))))))  ; Fallback to text for now

(defun mark-graf-render--math-preview (elem)
  "Render math ELEM using preview-latex if available."
  (if (featurep 'preview)
      ;; preview-latex available
      (let ((_start (mark-graf-node-start elem))
            (_end (mark-graf-node-end elem)))
        ;; Use preview-latex machinery
        (mark-graf-render--math elem))  ; Simplified - full integration would be complex
    ;; Fallback
    (mark-graf-render--math elem)))

(provide 'mark-graf-render)
;;; mark-graf-render.el ends here

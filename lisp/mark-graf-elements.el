;;; mark-graf-elements.el --- Element handlers for mark-graf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;; This file is part of mark-graf.

;;; Commentary:

;; Element handlers for mark-graf.
;; Provides insertion, manipulation, and detection for markdown elements.

;;; Code:

(require 'cl-lib)

;;; Element Detection

(defun mark-graf-element-at-point ()
  "Return the type of markdown element at point."
  (when-let ((elem (mark-graf-ts--element-at (point))))
    (mark-graf-node-type elem)))

(defun mark-graf-in-heading-p ()
  "Return non-nil if point is in a heading."
  (eq (mark-graf-element-at-point) 'heading))

(defun mark-graf-in-code-block-p ()
  "Return non-nil if point is in a code block."
  (memq (mark-graf-element-at-point) '(code-block code-block-indented)))

(defun mark-graf-in-list-p ()
  "Return non-nil if point is in a list or list item."
  (memq (mark-graf-element-at-point) '(list list-item)))

(defun mark-graf-in-table-p ()
  "Return non-nil if point is in a table."
  (memq (mark-graf-element-at-point) '(table table-header table-row table-cell)))

(defun mark-graf-in-blockquote-p ()
  "Return non-nil if point is in a blockquote."
  (eq (mark-graf-element-at-point) 'blockquote))

(defun mark-graf-in-link-p ()
  "Return non-nil if point is in a link."
  (memq (mark-graf-element-at-point) '(link link-ref link-ref-collapsed)))

;;; Heading Utilities

(defun mark-graf-heading-level-at-point ()
  "Return the heading level at point, or nil if not in a heading."
  (or
   ;; Try tree-sitter first
   (when-let ((elem (ignore-errors (mark-graf-ts--element-at (point)))))
     (when (eq (mark-graf-node-type elem) 'heading)
       (mark-graf-node-level elem)))
   ;; Fallback to regex
   (save-excursion
     (beginning-of-line)
     (when (looking-at "^\\(#\\{1,6\\}\\) ")
       (length (match-string 1))))))

(defun mark-graf-current-heading ()
  "Return the nearest heading above point."
  (save-excursion
    (when (re-search-backward "^#+[ \t]+" nil t)
      (mark-graf-ts--element-at (point)))))

(defun mark-graf-heading-bounds ()
  "Return (START . END) bounds of current heading, or nil."
  (when-let ((elem (mark-graf-ts--element-at (point))))
    (when (eq (mark-graf-node-type elem) 'heading)
      (cons (mark-graf-node-start elem)
            (mark-graf-node-end elem)))))

;;; List Utilities

(defun mark-graf-list-item-bounds ()
  "Return (START . END) bounds of current list item, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([ \t]*\\)\\([-*+]\\|[0-9]+[.)]\\)[ \t]+")
      (let ((start (point))
            (indent (length (match-string 1))))
        ;; Find end of list item (next item at same or lower indent, or blank line)
        (forward-line 1)
        (while (and (not (eobp))
                    (not (looking-at "^[ \t]*$"))
                    (or (looking-at "^[ \t]+[^-*+0-9]")  ; continuation
                        (and (looking-at "^\\([ \t]*\\)[-*+0-9]")
                             (> (length (match-string 1)) indent))))
          (forward-line 1))
        (cons start (point))))))

(defun mark-graf-list-level-at-point ()
  "Return the nesting level of list item at point (0-based)."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([ \t]*\\)[-*+0-9]")
      (/ (length (match-string 1)) 2))))

(defun mark-graf-list-marker-at-point ()
  "Return the list marker type at point (:unordered, :ordered, :task)."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "^[ \t]*[-*+][ \t]+\\[[ xX]\\]") :task)
     ((looking-at "^[ \t]*[-*+][ \t]+") :unordered)
     ((looking-at "^[ \t]*[0-9]+[.)][ \t]+") :ordered)
     (t nil))))

;;; Table Utilities

(defun mark-graf-table-bounds ()
  "Return (START . END) bounds of current table, or nil."
  (when (mark-graf-in-table-p)
    (save-excursion
      (let (start end)
        ;; Find start
        (while (and (not (bobp))
                    (looking-at "^|"))
          (setq start (point))
          (forward-line -1))
        (unless start (setq start (point)))
        ;; Find end
        (goto-char start)
        (while (and (not (eobp))
                    (looking-at "^|"))
          (forward-line 1))
        (setq end (point))
        (cons start end)))))

(defun mark-graf-table-cell-bounds ()
  "Return (START . END) bounds of current table cell, or nil."
  (when (mark-graf-in-table-p)
    (save-excursion
      (let ((pos (point)))
        (beginning-of-line)
        (let ((line-start (point))
              (cell-start nil)
              (cell-end nil))
          ;; Find cell boundaries
          (while (and (< (point) pos)
                      (re-search-forward "|" (line-end-position) t))
            (setq cell-start (point)))
          (when cell-start
            (if (re-search-forward "|" (line-end-position) t)
                (setq cell-end (1- (point)))
              (setq cell-end (line-end-position)))
            (cons cell-start cell-end)))))))

(defun mark-graf-table-column-at-point ()
  "Return 0-based column index of current table cell."
  (when (mark-graf-in-table-p)
    (save-excursion
      (let ((pos (point))
            (col 0))
        (beginning-of-line)
        (while (and (< (point) pos)
                    (search-forward "|" (line-end-position) t))
          (when (<= (point) pos)
            (setq col (1+ col))))
        (1- col)))))

;;; Code Block Utilities

(defun mark-graf-code-block-bounds ()
  "Return (START . END) bounds of current code block, or nil."
  (when (mark-graf-in-code-block-p)
    (save-excursion
      (let ((start nil) (end nil))
        ;; Find opening fence
        (when (re-search-backward "^```\\|^~~~" nil t)
          (setq start (point))
          ;; Find closing fence
          (forward-line 1)
          (when (re-search-forward "^```\\|^~~~" nil t)
            (setq end (line-end-position))
            (cons start end)))))))

(defun mark-graf-code-block-language ()
  "Return the language of the current code block, or nil."
  (when (mark-graf-in-code-block-p)
    (save-excursion
      (when (re-search-backward "^```\\([a-zA-Z0-9_+-]*\\)\\|^~~~\\([a-zA-Z0-9_+-]*\\)" nil t)
        (or (match-string 1) (match-string 2))))))

;;; Blockquote Utilities

(defun mark-graf-blockquote-level-at-point ()
  "Return the nesting level of blockquote at point (1-based)."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\(>+\\)")
        (length (match-string 1))
      0)))

;;; Element Insertion Helpers

(defun mark-graf--wrap-region-or-insert (open close)
  "Wrap region with OPEN and CLOSE, or insert both at point."
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char end)
          (insert close)
          (goto-char start)
          (insert open)))
    (insert open close)
    (backward-char (length close))))

(defun mark-graf--toggle-markup (open close)
  "Toggle markup OPEN/CLOSE around current word or region."
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties start end)))
        (if (and (string-prefix-p open text)
                 (string-suffix-p close text))
            ;; Remove markup
            (progn
              (delete-region start end)
              (insert (substring text (length open) (- (length text) (length close)))))
          ;; Add markup
          (delete-region start end)
          (insert open text close)))
    ;; No region - check if we're inside markup
    (let ((word-bounds (bounds-of-thing-at-point 'word)))
      (if word-bounds
          (let* ((start (car word-bounds))
                 (end (cdr word-bounds))
                 (check-start (max (point-min) (- start (length open))))
                 (check-end (min (point-max) (+ end (length close))))
                 (text (buffer-substring-no-properties check-start check-end)))
            (if (and (string-prefix-p open text)
                     (string-suffix-p close text))
                ;; Remove markup
                (progn
                  (delete-region check-start check-end)
                  (insert (substring text (length open) (- (length text) (length close)))))
              ;; Add markup around word
              (save-excursion
                (goto-char end)
                (insert close)
                (goto-char start)
                (insert open))))
        ;; No word at point - just insert
        (insert open close)
        (backward-char (length close))))))

(defun mark-graf--ensure-blank-line-before ()
  "Ensure there's a blank line before point."
  (unless (or (bobp)
              (save-excursion
                (forward-line -1)
                (looking-at "^[ \t]*$")))
    (insert "\n")))

(defun mark-graf--ensure-blank-line-after ()
  "Ensure there's a blank line after point."
  (unless (or (eobp)
              (save-excursion
                (forward-line 1)
                (looking-at "^[ \t]*$")))
    (save-excursion (insert "\n"))))

(defun mark-graf--at-line-start-p ()
  "Return non-nil if point is at the start of a line (ignoring whitespace)."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

;;; Text Object Functions (for integration with evil-mode etc.)

(defun mark-graf-bounds-of-element-at-point ()
  "Return bounds of markdown element at point as (START . END)."
  (when-let ((elem (mark-graf-ts--element-at (point))))
    (cons (mark-graf-node-start elem)
          (mark-graf-node-end elem))))

(defun mark-graf-bounds-of-block-at-point ()
  "Return bounds of markdown block at point as (START . END)."
  (when-let ((block (mark-graf-ts--containing-block (point))))
    (cons (mark-graf-node-start block)
          (mark-graf-node-end block))))

;; Register as thing-at-point types
(put 'mark-graf-element 'bounds-of-thing-at-point
     #'mark-graf-bounds-of-element-at-point)
(put 'mark-graf-block 'bounds-of-thing-at-point
     #'mark-graf-bounds-of-block-at-point)

(provide 'mark-graf-elements)
;;; mark-graf-elements.el ends here

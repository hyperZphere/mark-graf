;;; mark-graf-mermaid.el --- Pure Elisp Mermaid SVG renderer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mark-graf contributors

;; This file is part of mark-graf.

;;; Commentary:

;; Pure Elisp SVG generator for Mermaid diagrams.
;; Generates SVG XML strings displayed inline via Emacs 30's
;; (create-image svg-string 'svg t).  Theme-aware using Emacs face colors.
;; Supports flowchart, sequence, state, class, and ER diagrams.

;;; Code:

(require 'cl-lib)
(require 'color)

;;; ============================================================
;;; Theme Integration
;;; ============================================================

(defun mark-graf-mermaid--color-blend (c1 c2 alpha)
  "Blend color C1 with C2 by ALPHA (0.0 = all C1, 1.0 = all C2).
C1 and C2 are hex strings like \"#rrggbb\"."
  (let ((r1 (string-to-number (substring c1 1 3) 16))
        (g1 (string-to-number (substring c1 3 5) 16))
        (b1 (string-to-number (substring c1 5 7) 16))
        (r2 (string-to-number (substring c2 1 3) 16))
        (g2 (string-to-number (substring c2 3 5) 16))
        (b2 (string-to-number (substring c2 5 7) 16)))
    (format "#%02x%02x%02x"
            (round (+ (* r1 (- 1 alpha)) (* r2 alpha)))
            (round (+ (* g1 (- 1 alpha)) (* g2 alpha)))
            (round (+ (* b1 (- 1 alpha)) (* b2 alpha))))))

(defun mark-graf-mermaid--face-color (face attr)
  "Get color ATTR from FACE, returning hex string.
Falls back to default face if unspecified."
  (let* ((color (face-attribute face attr nil t))
         (vals (when (and color (not (eq color 'unspecified)))
                 (color-values color))))
    (if vals
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (x) (/ x 256)) vals))
      (if (eq attr :background) "#1e1e2e" "#cdd6f4"))))

(defun mark-graf-mermaid--theme-colors ()
  "Extract theme colors from current Emacs faces.
Returns plist with :bg :fg :surface :border :accent."
  (let* ((bg (mark-graf-mermaid--face-color 'default :background))
         (fg (mark-graf-mermaid--face-color 'default :foreground))
         (surface (mark-graf-mermaid--color-blend bg fg 0.08))
         (border (mark-graf-mermaid--color-blend bg fg 0.3))
         (accent (mark-graf-mermaid--face-color 'font-lock-keyword-face :foreground)))
    (list :bg bg :fg fg :surface surface :border border :accent accent)))

;;; ============================================================
;;; SVG Primitives
;;; ============================================================

(defun mark-graf-mermaid--svg-escape (text)
  "Escape TEXT for safe inclusion in SVG XML."
  (let ((s (if (stringp text) text (format "%s" text))))
    (setq s (replace-regexp-in-string "&" "&amp;" s))
    (setq s (replace-regexp-in-string "<" "&lt;" s))
    (setq s (replace-regexp-in-string ">" "&gt;" s))
    (setq s (replace-regexp-in-string "\"" "&quot;" s))
    s))

(defun mark-graf-mermaid--svg-rect (x y w h rx fill stroke &optional stroke-width)
  "SVG rectangle at X Y with dimensions W H, corner radius RX."
  (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"%d\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%s\"/>"
          x y w h rx fill stroke (or stroke-width "1.5")))

(defun mark-graf-mermaid--svg-text (x y text font-size fill &optional anchor weight)
  "SVG text at X Y with FONT-SIZE and FILL color.
ANCHOR is text-anchor (start, middle, end).  WEIGHT is font-weight."
  (format "<text x=\"%d\" y=\"%d\" font-family=\"sans-serif\" font-size=\"%d\" fill=\"%s\" text-anchor=\"%s\"%s>%s</text>"
          x y font-size fill
          (or anchor "middle")
          (if weight (format " font-weight=\"%s\"" weight) "")
          (mark-graf-mermaid--svg-escape text)))

(defun mark-graf-mermaid--svg-line (x1 y1 x2 y2 stroke &optional dash stroke-width)
  "SVG line from X1 Y1 to X2 Y2."
  (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"%s\"%s/>"
          x1 y1 x2 y2 stroke (or stroke-width "1.5")
          (if dash (format " stroke-dasharray=\"%s\"" dash) "")))

(defun mark-graf-mermaid--svg-path (d stroke fill &optional stroke-width dash)
  "SVG path with data D."
  (format "<path d=\"%s\" stroke=\"%s\" fill=\"%s\" stroke-width=\"%s\"%s/>"
          d stroke fill (or stroke-width "1.5")
          (if dash (format " stroke-dasharray=\"%s\"" dash) "")))

(defun mark-graf-mermaid--svg-circle (cx cy r fill stroke &optional stroke-width)
  "SVG circle at CX CY with radius R."
  (format "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%s\"/>"
          cx cy r fill stroke (or stroke-width "1.5")))

(defun mark-graf-mermaid--svg-diamond (cx cy w h fill stroke)
  "SVG diamond centered at CX CY with width W and height H."
  (let ((hw (/ w 2))
        (hh (/ h 2)))
    (format "<polygon points=\"%d,%d %d,%d %d,%d %d,%d\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1.5\"/>"
            cx (- cy hh)
            (+ cx hw) cy
            cx (+ cy hh)
            (- cx hw) cy
            fill stroke)))

(defun mark-graf-mermaid--svg-arrow-marker (id color &optional marker-type)
  "SVG <marker> definition for arrowhead with ID and COLOR.
MARKER-TYPE can be `arrow' (default), `open', `diamond', `circle'."
  (pcase (or marker-type 'arrow)
    ('arrow
     (format "<marker id=\"%s\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"8\" markerHeight=\"8\" orient=\"auto-start-reverse\"><path d=\"M 0 0 L 10 5 L 0 10 z\" fill=\"%s\"/></marker>"
             id color))
    ('open
     (format "<marker id=\"%s\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"8\" markerHeight=\"8\" orient=\"auto-start-reverse\"><path d=\"M 0 0 L 10 5 L 0 10\" fill=\"none\" stroke=\"%s\" stroke-width=\"1.5\"/></marker>"
             id color))
    ('diamond
     (format "<marker id=\"%s\" viewBox=\"0 0 12 12\" refX=\"6\" refY=\"6\" markerWidth=\"10\" markerHeight=\"10\" orient=\"auto-start-reverse\"><polygon points=\"6,0 12,6 6,12 0,6\" fill=\"%s\"/></marker>"
             id color))
    ('circle
     (format "<marker id=\"%s\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\" markerWidth=\"8\" markerHeight=\"8\" orient=\"auto-start-reverse\"><circle cx=\"5\" cy=\"5\" r=\"4\" fill=\"%s\"/></marker>"
             id color))))

(defun mark-graf-mermaid--svg-wrap (width height defs body)
  "Wrap BODY in SVG root element with WIDTH, HEIGHT, DEFS."
  (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">\n%s%s\n</svg>"
          width height width height
          (if defs (format "<defs>%s</defs>\n" defs) "")
          body))

(defun mark-graf-mermaid--text-width (text font-size)
  "Estimate pixel width of TEXT at FONT-SIZE.
Uses average character width of 0.6 * font-size for sans-serif."
  (round (* (length text) font-size 0.6)))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

(defun mark-graf-mermaid-render (diagram-code)
  "Render mermaid DIAGRAM-CODE to an SVG string.
Returns SVG XML string or nil if diagram type is unsupported."
  (let* ((lines (split-string diagram-code "\n" t "[ \t\r]+"))
         (first (car lines))
         (type (cond
                ((and first (string-match "^\\(?:graph\\|flowchart\\)" first)) 'flowchart)
                ((and first (string-match "^sequenceDiagram" first)) 'sequence)
                ((and first (string-match "^stateDiagram" first)) 'state)
                ((and first (string-match "^classDiagram" first)) 'class)
                ((and first (string-match "^erDiagram" first)) 'er)
                (t nil))))
    (when type
      (condition-case err
          (let ((colors (mark-graf-mermaid--theme-colors)))
            (cond
             ((eq type 'flowchart) (mark-graf-mermaid--render-flowchart lines colors))
             ((eq type 'sequence)  (mark-graf-mermaid--render-sequence lines colors))
             ((eq type 'state)     (mark-graf-mermaid--render-state lines colors))
             ((eq type 'class)     (mark-graf-mermaid--render-class lines colors))
             ((eq type 'er)        (mark-graf-mermaid--render-er lines colors))
             (t (message "mark-graf-mermaid: unknown type %S" type) nil)))
        (error
         (message "mark-graf-mermaid: %s render error: %s" type (error-message-string err))
         nil)))))

;;; ============================================================
;;; Flowchart Renderer
;;; ============================================================

(defun mark-graf-mermaid--parse-flowchart (lines)
  "Parse flowchart LINES into (:direction DIR :nodes NODES :edges EDGES).
NODES is a hash-table id->(list :id :label :shape).
EDGES is a list of (list :from :to :label :style)."
  (let ((direction "TD")
        (nodes (make-hash-table :test 'equal))
        (edges nil)
        (first (car lines)))
    ;; Parse direction from first line
    (when (string-match "^\\(?:graph\\|flowchart\\)\\s-+\\(TB\\|TD\\|BT\\|LR\\|RL\\)" first)
      (setq direction (match-string 1 first)))
    ;; Parse remaining lines
    (dolist (raw-line (cdr lines))
      (let ((line (string-trim raw-line)))
        (unless (or (string-empty-p line)
                    (string-match-p "^%%" line)
                    (string-match-p "^\\(style\\|classDef\\|class\\|click\\|subgraph\\|end\\)" line))
          (cond
           ;; Edge: A[label] -->|text| B[label]
           ((string-match
             (concat "\\([A-Za-z0-9_]+\\)"
                     "\\(?:\\[\\([^]]*\\)\\]\\|(\\([^)]*\\))\\|{\\([^}]*\\)}\\|\\[\\[\\([^]]*\\)\\]\\]\\)?"
                     "[ \t]*"
                     "\\(--+>\\|==+>\\|-\\.->\\|--+\\)"
                     "[ \t]*"
                     "\\(?:|\\([^|]*\\)|\\)?"
                     "[ \t]*"
                     "\\([A-Za-z0-9_]+\\)"
                     "\\(?:\\[\\([^]]*\\)\\]\\|(\\([^)]*\\))\\|{\\([^}]*\\)}\\|\\[\\[\\([^]]*\\)\\]\\]\\)?")
             line)
            (let* ((from-id (match-string 1 line))
                   (from-label (or (match-string 2 line) (match-string 3 line)
                                   (match-string 4 line) (match-string 5 line)))
                   (arrow (match-string 6 line))
                   (edge-label (match-string 7 line))
                   (to-id (match-string 8 line))
                   (to-label (or (match-string 9 line) (match-string 10 line)
                                 (match-string 11 line) (match-string 12 line)))
                   (from-shape (cond ((match-string 2 line) 'rect)
                                     ((match-string 3 line) 'round)
                                     ((match-string 4 line) 'diamond)
                                     (t nil)))
                   (to-shape (cond ((match-string 9 line) 'rect)
                                   ((match-string 10 line) 'round)
                                   ((match-string 11 line) 'diamond)
                                   (t nil))))
              ;; Register nodes
              (unless (gethash from-id nodes)
                (puthash from-id (list :id from-id :label (or from-label from-id)
                                       :shape (or from-shape 'rect))
                         nodes))
              (when (and from-label (not from-shape))
                ;; Update label if we have one now
                (let ((existing (gethash from-id nodes)))
                  (when (string= (plist-get existing :label) from-id)
                    (puthash from-id (plist-put (copy-sequence existing) :label from-label) nodes))))
              (unless (gethash to-id nodes)
                (puthash to-id (list :id to-id :label (or to-label to-id)
                                     :shape (or to-shape 'rect))
                         nodes))
              (when (and to-label (not to-shape))
                (let ((existing (gethash to-id nodes)))
                  (when (string= (plist-get existing :label) to-id)
                    (puthash to-id (plist-put (copy-sequence existing) :label to-label) nodes))))
              (push (list :from from-id :to to-id
                          :label (when edge-label (string-trim edge-label))
                          :style (cond
                                  ((string-match-p "==>" arrow) 'thick)
                                  ((string-match-p "-\\.->" arrow) 'dotted)
                                  (t 'normal)))
                    edges)))
           ;; Standalone node: A[text] or A(text) or A{text}
           ((string-match
             "^\\([A-Za-z0-9_]+\\)\\(?:\\[\\([^]]*\\)\\]\\|(\\([^)]*\\))\\|{\\([^}]*\\)}\\)"
             line)
            (let ((id (match-string 1 line))
                  (label (or (match-string 2 line) (match-string 3 line) (match-string 4 line)))
                  (shape (cond ((match-string 2 line) 'rect)
                               ((match-string 3 line) 'round)
                               ((match-string 4 line) 'diamond))))
              (puthash id (list :id id :label (or label id) :shape (or shape 'rect)) nodes)))))))
    (list :direction direction :nodes nodes :edges (nreverse edges))))

(defun mark-graf-mermaid--flowchart-topo-sort (nodes edges)
  "Topological sort of NODES given EDGES. Returns list of node IDs."
  (let* ((all-ids nil)
         (in-degree (make-hash-table :test 'equal))
         (adjacency (make-hash-table :test 'equal))
         (queue nil)
         (result nil))
    (maphash (lambda (k _v) (push k all-ids)) nodes)
    ;; Ensure edge endpoints exist
    (dolist (edge edges)
      (let ((from (plist-get edge :from))
            (to (plist-get edge :to)))
        (unless (member from all-ids) (push from all-ids))
        (unless (member to all-ids) (push to all-ids))
        (unless (gethash from nodes)
          (puthash from (list :id from :label from :shape 'rect) nodes))
        (unless (gethash to nodes)
          (puthash to (list :id to :label to :shape 'rect) nodes))))
    (setq all-ids (nreverse all-ids))
    (dolist (id all-ids) (puthash id 0 in-degree))
    (dolist (edge edges)
      (let ((from (plist-get edge :from))
            (to (plist-get edge :to)))
        (puthash from (cons to (gethash from adjacency nil)) adjacency)
        (puthash to (1+ (gethash to in-degree 0)) in-degree)))
    (dolist (id all-ids)
      (when (zerop (gethash id in-degree 0))
        (push id queue)))
    (setq queue (nreverse queue))
    (while queue
      (let ((node (pop queue)))
        (push node result)
        (dolist (neighbor (gethash node adjacency nil))
          (let ((new-deg (1- (gethash neighbor in-degree 1))))
            (puthash neighbor new-deg in-degree)
            (when (zerop new-deg)
              (setq queue (append queue (list neighbor))))))))
    (dolist (id all-ids)
      (unless (member id result) (push id result)))
    (nreverse result)))

(defun mark-graf-mermaid--flowchart-assign-layers (node-ids edges)
  "Assign layer numbers to NODE-IDS based on EDGES. Returns alist (id . layer).
Back-edges (cycles) are ignored to prevent infinite layer growth."
  (let ((layers (make-hash-table :test 'equal))
        ;; Build a set of forward edges only (from appears before to in topo order)
        (topo-rank (make-hash-table :test 'equal))
        (forward-edges nil))
    ;; Assign topological rank to each node
    (let ((rank 0))
      (dolist (id node-ids)
        (puthash id rank topo-rank)
        (setq rank (1+ rank))))
    ;; Filter to forward edges only
    (dolist (edge edges)
      (let ((from (plist-get edge :from))
            (to (plist-get edge :to)))
        (when (< (gethash from topo-rank 0) (gethash to topo-rank 0))
          (push edge forward-edges))))
    ;; Assign layers using only forward edges
    (dolist (id node-ids) (puthash id 0 layers))
    (dotimes (_ (length node-ids))
      (dolist (edge forward-edges)
        (let* ((from (plist-get edge :from))
               (to (plist-get edge :to))
               (fl (gethash from layers 0))
               (tl (gethash to layers 0)))
          (when (< tl (1+ fl))
            (puthash to (1+ fl) layers)))))
    (let (result)
      (dolist (id node-ids)
        (push (cons id (gethash id layers 0)) result))
      (nreverse result))))

(defun mark-graf-mermaid--render-flowchart (lines colors)
  "Render flowchart from LINES with theme COLORS. Returns SVG string."
  (let* ((parsed (mark-graf-mermaid--parse-flowchart lines))
         (direction (plist-get parsed :direction))
         (nodes (plist-get parsed :nodes))
         (edges (plist-get parsed :edges))
         (horizontal (member direction '("LR" "RL")))
         (reverse-dir (member direction '("BT" "RL")))
         (bg (plist-get colors :bg))
         (fg (plist-get colors :fg))
         (surface (plist-get colors :surface))
         (border (plist-get colors :border))
         ;; Layout parameters
         (font-size 14)
         (node-pad-x 20)
         (node-h 40)
         (layer-gap (if horizontal 120 80))
         (sibling-gap 40)
         ;; Compute
         (sorted-ids (mark-graf-mermaid--flowchart-topo-sort nodes edges))
         (effective-edges (if reverse-dir
                              (mapcar (lambda (e)
                                        (list :from (plist-get e :to)
                                              :to (plist-get e :from)
                                              :label (plist-get e :label)
                                              :style (plist-get e :style)))
                                      edges)
                            edges))
         (layer-alist (mark-graf-mermaid--flowchart-assign-layers sorted-ids effective-edges))
         (max-layer (if layer-alist (apply #'max (mapcar #'cdr layer-alist)) 0))
         (layer-groups (make-hash-table :test 'equal))
         ;; Node positions: id -> (:x :y :w :h)
         (node-pos (make-hash-table :test 'equal))
         (svg-parts nil)
         (defs-parts nil)
         (total-w 0)
         (total-h 0))
    ;; Group nodes by layer
    (dolist (pair layer-alist)
      (puthash (cdr pair)
               (append (gethash (cdr pair) layer-groups nil) (list (car pair)))
               layer-groups))
    ;; Compute node widths
    (let ((node-widths (make-hash-table :test 'equal)))
      (maphash (lambda (id info)
                 (let* ((label (plist-get info :label))
                        (tw (mark-graf-mermaid--text-width label font-size))
                        (shape (plist-get info :shape))
                        (w (pcase shape
                             ('diamond (+ tw (* node-pad-x 4)))
                             (_ (+ tw (* node-pad-x 2))))))
                   (puthash id (max w 70) node-widths)))
               nodes)
      ;; FIRST PASS: compute total-w and total-h before positioning
      (if horizontal
          (let ((x 20))
            (dotimes (layer (1+ max-layer))
              (let* ((layer-nodes (gethash layer layer-groups nil))
                     (max-w (apply #'max 70
                                   (mapcar (lambda (id) (gethash id node-widths 70)) layer-nodes)))
                     (count (length layer-nodes))
                     (layer-h (+ (* count node-h) (* (max 0 (1- count)) sibling-gap))))
                (setq total-h (max total-h (+ layer-h 40)))
                (setq x (+ x max-w layer-gap))))
            (setq total-w (+ x 20)))
        (progn
          (dotimes (layer (1+ max-layer))
            (let* ((layer-nodes (gethash layer layer-groups nil))
                   (layer-w 0))
              (dolist (id layer-nodes)
                (setq layer-w (+ layer-w (gethash id node-widths 70) sibling-gap)))
              (setq layer-w (- layer-w sibling-gap))
              (setq total-w (max total-w (+ layer-w 40)))))
          ;; total-w already computed from layer widths
          (setq total-h (+ 40 (* (1+ max-layer) (+ node-h layer-gap))))))
      ;; SECOND PASS: position nodes centered within total-w
      (if horizontal
          (let ((x 20))
            (dotimes (layer (1+ max-layer))
              (let* ((layer-nodes (gethash layer layer-groups nil))
                     (max-w (apply #'max 70
                                   (mapcar (lambda (id) (gethash id node-widths 70)) layer-nodes)))
                     (count (length layer-nodes))
                     (layer-h (+ (* count node-h) (* (max 0 (1- count)) sibling-gap)))
                     (y (/ (- total-h layer-h) 2)))
                (dolist (id layer-nodes)
                  (let ((w (gethash id node-widths 70)))
                    (puthash id (list :x (+ x (/ (- max-w w) 2)) :y y :w w :h node-h) node-pos)
                    (setq y (+ y node-h sibling-gap))))
                (setq x (+ x max-w layer-gap)))))
        (let ((y 20))
          (dotimes (layer (1+ max-layer))
            (let* ((layer-nodes (gethash layer layer-groups nil))
                   (layer-w 0))
              (dolist (id layer-nodes)
                (setq layer-w (+ layer-w (gethash id node-widths 70) sibling-gap)))
              (setq layer-w (- layer-w sibling-gap))
              (let ((x (/ (- total-w layer-w) 2)))
                (dolist (id layer-nodes)
                  (let ((w (gethash id node-widths 70)))
                    (puthash id (list :x x :y y :w w :h node-h) node-pos)
                    (setq x (+ x w sibling-gap)))))
              (setq y (+ y node-h layer-gap))))
          (setq total-h (+ y 20)))))
    ;; Check for back-edges that need right-side routing and widen SVG
    (let ((has-back-edge nil))
      (dolist (edge effective-edges)
        (let ((fp (gethash (plist-get edge :from) node-pos))
              (tp (gethash (plist-get edge :to) node-pos)))
          (when (and fp tp
                     (if horizontal
                         (>= (plist-get fp :x) (plist-get tp :x))
                       (>= (plist-get fp :y) (plist-get tp :y))))
            (setq has-back-edge t))))
      (when has-back-edge
        (setq total-w (+ total-w 60))))
    ;; Arrow marker
    (push (mark-graf-mermaid--svg-arrow-marker "arrowhead" fg) defs-parts)
    ;; Background
    (push (mark-graf-mermaid--svg-rect 0 0 total-w total-h 6 bg "none") svg-parts)
    ;; Draw edges first (behind nodes)
    (dolist (edge effective-edges)
      (let* ((from-id (plist-get edge :from))
             (to-id (plist-get edge :to))
             (from-pos (gethash from-id node-pos))
             (to-pos (gethash to-id node-pos))
             (label (plist-get edge :label))
             (style (plist-get edge :style)))
        (when (and from-pos to-pos)
          (let* ((dash (pcase style ('dotted "5,5") ('thick nil) (_ nil)))
                 (sw (pcase style ('thick "3") (_ "1.5")))
                 ;; Detect back-edge: target is above source (vertical) or left (horizontal)
                 (is-back-edge (if horizontal
                                   (>= (plist-get from-pos :x) (plist-get to-pos :x))
                                 (>= (plist-get from-pos :y) (plist-get to-pos :y))))
                 (sx 0) (sy 0) (ex 0) (ey 0))
            (if is-back-edge
                ;; Back-edge: route around the right side of nodes
                (let* ((from-right (+ (plist-get from-pos :x) (plist-get from-pos :w)))
                       (to-right (+ (plist-get to-pos :x) (plist-get to-pos :w)))
                       (from-cy (+ (plist-get from-pos :y) (/ (plist-get from-pos :h) 2)))
                       (to-cy (+ (plist-get to-pos :y) (/ (plist-get to-pos :h) 2)))
                       (route-x (+ (max from-right to-right) 30)))
                  (setq sx from-right sy from-cy ex to-right ey to-cy)
                  (push (format "<path d=\"M %d %d L %d %d L %d %d L %d %d\" stroke=\"%s\" fill=\"none\" stroke-width=\"%s\" marker-end=\"url(#arrowhead)\"%s/>"
                                sx sy route-x sy route-x ey ex ey
                                fg sw
                                (if dash (format " stroke-dasharray=\"%s\"" dash) ""))
                        svg-parts)
                  ;; Label on back-edge
                  (when label
                    (let ((lx (+ route-x 6))
                          (ly (/ (+ sy ey) 2)))
                      (push (mark-graf-mermaid--svg-text lx ly label (1- font-size) fg "start") svg-parts))))
              ;; Forward edge: normal routing
              (let* ((fx (+ (plist-get from-pos :x) (/ (plist-get from-pos :w) 2)))
                     (fy (+ (plist-get from-pos :y) (/ (plist-get from-pos :h) 2)))
                     (tx (+ (plist-get to-pos :x) (/ (plist-get to-pos :w) 2)))
                     (ty (+ (plist-get to-pos :y) (/ (plist-get to-pos :h) 2))))
                ;; Adjust connection points to node borders
                (if horizontal
                    (progn
                      (setq sx (+ (plist-get from-pos :x) (plist-get from-pos :w)))
                      (setq sy fy)
                      (setq ex (plist-get to-pos :x))
                      (setq ey ty))
                  (setq sx fx)
                  (setq sy (+ (plist-get from-pos :y) (plist-get from-pos :h)))
                  (setq ex tx)
                  (setq ey (plist-get to-pos :y)))
                ;; Draw path
                (if (and (not horizontal) (/= sx ex))
                    (let ((mid-y (/ (+ sy ey) 2)))
                      (push (format "<path d=\"M %d %d C %d %d %d %d %d %d\" stroke=\"%s\" fill=\"none\" stroke-width=\"%s\" marker-end=\"url(#arrowhead)\"%s/>"
                                    sx sy sx mid-y ex mid-y ex ey
                                    fg sw
                                    (if dash (format " stroke-dasharray=\"%s\"" dash) ""))
                            svg-parts))
                  (if (and horizontal (/= sy ey))
                      (let ((mid-x (/ (+ sx ex) 2)))
                        (push (format "<path d=\"M %d %d C %d %d %d %d %d %d\" stroke=\"%s\" fill=\"none\" stroke-width=\"%s\" marker-end=\"url(#arrowhead)\"%s/>"
                                      sx sy mid-x sy mid-x ey ex ey
                                      fg sw
                                      (if dash (format " stroke-dasharray=\"%s\"" dash) ""))
                              svg-parts))
                    (push (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"%s\" marker-end=\"url(#arrowhead)\"%s/>"
                                  sx sy ex ey fg sw
                                  (if dash (format " stroke-dasharray=\"%s\"" dash) ""))
                          svg-parts)))
                ;; Edge label
                (when label
                  (let ((lx (/ (+ sx ex) 2))
                        (ly (- (/ (+ sy ey) 2) 4)))
                    (push (mark-graf-mermaid--svg-rect (- lx (/ (+ (mark-graf-mermaid--text-width label font-size) 8) 2))
                                                        (- ly (1+ font-size))
                                                        (+ (mark-graf-mermaid--text-width label font-size) 8)
                                                        (+ font-size 6)
                                                        3 bg "none")
                          svg-parts)
                    (push (mark-graf-mermaid--svg-text lx ly label (1- font-size) fg) svg-parts)))))))))
    ;; Draw nodes
    (maphash
     (lambda (id pos)
       (let* ((info (gethash id nodes))
              (label (plist-get info :label))
              (shape (plist-get info :shape))
              (x (plist-get pos :x))
              (y (plist-get pos :y))
              (w (plist-get pos :w))
              (h (plist-get pos :h))
              (cx (+ x (/ w 2)))
              (cy (+ y (/ h 2)))
              (text-y (+ cy (/ font-size 3))))
         (pcase shape
           ('diamond
            (push (mark-graf-mermaid--svg-diamond cx cy w h surface border) svg-parts))
           ('round
            (push (mark-graf-mermaid--svg-rect x y w h (/ h 2) surface border) svg-parts))
           ('circle
            (let ((r (/ (max w h) 2)))
              (push (mark-graf-mermaid--svg-circle cx cy r surface border) svg-parts)))
           (_
            (push (mark-graf-mermaid--svg-rect x y w h 6 surface border) svg-parts)))
         (push (mark-graf-mermaid--svg-text cx text-y label font-size fg) svg-parts)))
     node-pos)
    ;; Assemble SVG
    (mark-graf-mermaid--svg-wrap
     total-w total-h
     (mapconcat #'identity (nreverse defs-parts) "\n")
     (mapconcat #'identity (nreverse svg-parts) "\n"))))

;;; ============================================================
;;; Sequence Diagram Renderer
;;; ============================================================

(defun mark-graf-mermaid--parse-sequence (lines)
  "Parse sequence diagram LINES.
Returns plist with :participants :messages :blocks.
PARTICIPANTS is list of names in order.
MESSAGES is list of plists with :from :to :text :type :index.
BLOCKS is list of plists with :type :label :start :end :sections."
  (let ((participants nil)
        (messages nil)
        (blocks nil)
        (block-stack nil)
        (msg-idx 0))
    (dolist (raw-line (cdr lines))
      (let ((line (string-trim raw-line)))
        (cond
         ;; participant declarations
         ((string-match "^\\(?:participant\\|actor\\)\\s-+\\(.+\\)" line)
          (let ((name (string-trim (match-string 1 line))))
            ;; Handle "participant A as Label"
            (when (string-match "^\\(\\S-+\\)\\s-+as\\s-+\\(.*\\)" name)
              (setq name (string-trim (match-string 2 name))))
            (unless (member name participants)
              (push name participants))))
         ;; Messages: A->>B: text, A-->>B: text, A-)B: text, etc.
         ((string-match
           "^\\(\\S-+?\\)\\s-*\\(->>\\|-->>\\|->\\|-->\\|-)\\)\\s-*\\(\\S-+?\\)\\s-*:\\s-*\\(.*\\)" line)
          (let ((from (match-string 1 line))
                (arrow (match-string 2 line))
                (to (match-string 3 line))
                (text (string-trim (match-string 4 line))))
            ;; Auto-register participants
            (unless (member from participants) (push from participants))
            (unless (member to participants) (push to participants))
            (push (list :from from :to to :text text
                        :type (cond
                               ((string= arrow "->>") 'solid)
                               ((string= arrow "-->>") 'dashed)
                               ((string= arrow "->") 'solid-open)
                               ((string= arrow "-->") 'dashed-open)
                               ((string= arrow "-)") 'async)
                               (t 'solid))
                        :index msg-idx)
                  messages)
            (setq msg-idx (1+ msg-idx))))
         ;; Note
         ((string-match "^Note\\s-+\\(?:over\\|left of\\|right of\\)\\s-+\\([^:]+\\):\\s-*\\(.*\\)" line)
          (let ((target (string-trim (match-string 1 line)))
                (text (string-trim (match-string 2 line))))
            (push (list :from target :to target :text text :type 'note :index msg-idx) messages)
            (setq msg-idx (1+ msg-idx))))
         ;; Alt/Else/Loop/End blocks
         ((string-match "^\\(alt\\|opt\\|loop\\|par\\|critical\\)\\s-+\\(.*\\)" line)
          (push (list :type (intern (match-string 1 line))
                      :label (string-trim (match-string 2 line))
                      :start msg-idx
                      :sections nil)
                block-stack))
         ((string-match "^else\\s-*\\(.*\\)" line)
          (when block-stack
            (let ((block (car block-stack)))
              (plist-put block :sections
                         (append (plist-get block :sections)
                                 (list (list :label (string-trim (or (match-string 1 line) ""))
                                             :index msg-idx)))))))
         ((string-match "^end$" line)
          (when block-stack
            (let ((block (pop block-stack)))
              (plist-put block :end msg-idx)
              (push block blocks)))))))
    (list :participants (nreverse participants)
          :messages (nreverse messages)
          :blocks (nreverse blocks))))

(defun mark-graf-mermaid--render-sequence (lines colors)
  "Render sequence diagram from LINES with COLORS. Returns SVG string."
  (let* ((parsed (mark-graf-mermaid--parse-sequence lines))
         (participants (plist-get parsed :participants))
         (messages (plist-get parsed :messages))
         (blocks (plist-get parsed :blocks))
         (bg (plist-get colors :bg))
         (fg (plist-get colors :fg))
         (surface (plist-get colors :surface))
         (border (plist-get colors :border))
         (accent (plist-get colors :accent))
         (font-size 14)
         (box-h 40)
         (box-pad 20)
         (col-gap 180)
         (msg-gap 44)
         (top-margin 20)
         ;; Participant positions
         (_num-p (length participants))
         (total-w 0)
         (msg-count (length messages))
         (total-h (+ top-margin box-h 20 (* (1+ msg-count) msg-gap) box-h 20))
         ;; Map participant name -> x center
         (px-map (make-hash-table :test 'equal))
         (svg-parts nil)
         (defs-parts nil))
    ;; Assign x positions (left-aligned)
    (let* ((margin 20)
           (first-bw (if participants
                         (max 80 (+ (mark-graf-mermaid--text-width (car participants) font-size)
                                    (* box-pad 2)))
                       80))
           (x (+ margin (/ first-bw 2))))
      (dolist (p participants)
        (puthash p x px-map)
        (setq x (+ x col-gap)))
      ;; Compute tight total-w from last participant's right edge
      (let* ((last-p (car (last participants)))
             (last-px (gethash last-p px-map))
             (last-bw (if last-p
                          (max 80 (+ (mark-graf-mermaid--text-width last-p font-size)
                                     (* box-pad 2)))
                        80)))
        (setq total-w (+ last-px (/ last-bw 2) margin))))
    ;; Markers
    (push (mark-graf-mermaid--svg-arrow-marker "seq-arrow" fg) defs-parts)
    (push (mark-graf-mermaid--svg-arrow-marker "seq-open" fg 'open) defs-parts)
    ;; Background
    (push (mark-graf-mermaid--svg-rect 0 0 total-w total-h 6 bg "none") svg-parts)
    ;; Draw blocks (alt/loop rectangles) behind everything
    (dolist (block blocks)
      (let* ((start-idx (plist-get block :start))
             (end-idx (plist-get block :end))
             (label (plist-get block :label))
             (block-type (plist-get block :type))
             (by1 (+ top-margin box-h 10 (* start-idx msg-gap)))
             (by2 (+ top-margin box-h 10 (* end-idx msg-gap) 20))
             (bx1 10)
             (bx2 (- total-w 10)))
        (push (mark-graf-mermaid--svg-rect bx1 by1 (- bx2 bx1) (- by2 by1) 4
                                            "none" border "1" ) svg-parts)
        ;; Type label
        (let* ((type-label (upcase (symbol-name block-type)))
               (tw (+ (mark-graf-mermaid--text-width type-label (1- font-size)) 12)))
          (push (mark-graf-mermaid--svg-rect bx1 by1 tw 20 4 surface border) svg-parts)
          (push (mark-graf-mermaid--svg-text (+ bx1 (/ tw 2)) (+ by1 14)
                                              type-label (1- font-size) accent nil "bold")
                svg-parts))
        ;; Condition label
        (when (and label (not (string-empty-p label)))
          (push (mark-graf-mermaid--svg-text (+ bx1 80) (+ by1 14)
                                              (concat "[" label "]") (1- font-size) fg "start")
                svg-parts))
        ;; Section dividers (else)
        (dolist (section (plist-get block :sections))
          (let* ((si (plist-get section :index))
                 (sy (+ top-margin box-h 10 (* si msg-gap) -5)))
            (push (mark-graf-mermaid--svg-line bx1 sy bx2 sy border "5,5") svg-parts)
            (let ((sl (plist-get section :label)))
              (when (and sl (not (string-empty-p sl)))
                (push (mark-graf-mermaid--svg-text (+ bx1 30) (- sy 4)
                                                    (concat "[" sl "]") (1- font-size) fg "start")
                      svg-parts)))))))
    ;; Draw participant boxes (top)
    (dolist (p participants)
      (let* ((px (gethash p px-map))
             (tw (mark-graf-mermaid--text-width p font-size))
             (bw (max 80 (+ tw (* box-pad 2))))
             (bx (- px (/ bw 2)))
             (by top-margin))
        (push (mark-graf-mermaid--svg-rect bx by bw box-h 6 surface border) svg-parts)
        (push (mark-graf-mermaid--svg-text px (+ by (/ box-h 2) (/ font-size 3))
                                            p font-size fg)
              svg-parts)))
    ;; Draw lifelines
    (let ((life-y1 (+ top-margin box-h))
          (life-y2 (- total-h box-h 20)))
      (dolist (p participants)
        (let ((px (gethash p px-map)))
          (push (mark-graf-mermaid--svg-line px life-y1 px life-y2 border "4,4") svg-parts))))
    ;; Draw messages
    (dolist (msg messages)
      (let* ((from (plist-get msg :from))
             (to (plist-get msg :to))
             (text (plist-get msg :text))
             (type (plist-get msg :type))
             (idx (plist-get msg :index))
             (my (+ top-margin box-h 20 (* (1+ idx) msg-gap)))
             (fx (or (gethash from px-map) 0))
             (tx (or (gethash to px-map) 0)))
        (cond
         ;; Note
         ((eq type 'note)
          (let* ((nw (+ (mark-graf-mermaid--text-width text font-size) 20))
                 (nh 28)
                 (nx (- fx (/ nw 2)))
                 (ny (- my (/ nh 2))))
            (push (mark-graf-mermaid--svg-rect nx ny nw nh 4
                                                (mark-graf-mermaid--color-blend bg accent 0.1)
                                                accent)
                  svg-parts)
            (push (mark-graf-mermaid--svg-text fx (+ my (/ font-size 3)) text (1- font-size) fg)
                  svg-parts)))
         ;; Self-message
         ((string= from to)
          (let ((sx fx)
                (loop-w 30))
            (push (mark-graf-mermaid--svg-path
                   (format "M %d %d L %d %d L %d %d L %d %d"
                           sx my (+ sx loop-w) my (+ sx loop-w) (+ my 20) sx (+ my 20))
                   fg "none" "1.5")
                  svg-parts)
            (push (mark-graf-mermaid--svg-text (+ sx loop-w 8) (+ my 10)
                                                text (1- font-size) fg "start")
                  svg-parts)))
         ;; Normal message
         (t
          (let* ((is-dashed (memq type '(dashed dashed-open)))
                 (is-open (memq type '(solid-open dashed-open async)))
                 (marker-id (if is-open "seq-open" "seq-arrow"))
                 (x1 fx) (x2 tx))
            ;; Arrow line
            (push (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1.5\" marker-end=\"url(#%s)\"%s/>"
                          x1 my x2 my fg marker-id
                          (if is-dashed " stroke-dasharray=\"5,5\"" ""))
                  svg-parts)
            ;; Label
            (when (and text (not (string-empty-p text)))
              (let ((lx (/ (+ x1 x2) 2))
                    (ly (- my 6)))
                (push (mark-graf-mermaid--svg-text lx ly text (1- font-size) fg) svg-parts))))))))
    ;; Draw participant boxes (bottom)
    (dolist (p participants)
      (let* ((px (gethash p px-map))
             (tw (mark-graf-mermaid--text-width p font-size))
             (bw (max 80 (+ tw (* box-pad 2))))
             (bx (- px (/ bw 2)))
             (by (- total-h box-h 20)))
        (push (mark-graf-mermaid--svg-rect bx by bw box-h 6 surface border) svg-parts)
        (push (mark-graf-mermaid--svg-text px (+ by (/ box-h 2) (/ font-size 3))
                                            p font-size fg)
              svg-parts)))
    ;; Assemble
    (mark-graf-mermaid--svg-wrap
     total-w total-h
     (mapconcat #'identity (nreverse defs-parts) "\n")
     (mapconcat #'identity (nreverse svg-parts) "\n"))))

;;; ============================================================
;;; State Diagram Renderer
;;; ============================================================

(defun mark-graf-mermaid--parse-state (lines)
  "Parse state diagram LINES.
Returns (:states STATES :transitions TRANSITIONS).
STATES is hash-table id->(list :id :label :type).
TRANSITIONS is list of (:from FROM :to TO :label LABEL)."
  (let ((states (make-hash-table :test 'equal))
        (transitions nil))
    (dolist (raw-line (cdr lines))
      (let ((line (string-trim raw-line)))
        (cond
         ;; State with description: state "label" as id
         ((string-match "^state\\s-+\"\\([^\"]+\\)\"\\s-+as\\s-+\\(\\S-+\\)" line)
          (puthash (match-string 2 line)
                   (list :id (match-string 2 line) :label (match-string 1 line) :type 'state)
                   states))
         ;; Transition: State1 --> State2 : label
         ((string-match "^\\(\\[\\*\\]\\|\\S-+\\)\\s-*-->\\s-*\\(\\[\\*\\]\\|\\S-+\\)\\(?:\\s-*:\\s-*\\(.*\\)\\)?" line)
          (let ((from (match-string 1 line))
                (to (match-string 2 line))
                (label (match-string 3 line)))
            ;; Register states
            (unless (gethash from states)
              (puthash from (list :id from :label from
                                  :type (if (string= from "[*]") 'start 'state))
                       states))
            (unless (gethash to states)
              (puthash to (list :id to :label to
                                :type (if (string= to "[*]") 'end 'state))
                       states))
            (push (list :from from :to to
                        :label (when label (string-trim label)))
                  transitions))))))
    (list :states states :transitions (nreverse transitions))))

(defun mark-graf-mermaid--render-state (lines colors)
  "Render state diagram from LINES with COLORS. Returns SVG string."
  (let* ((parsed (mark-graf-mermaid--parse-state lines))
         (states (plist-get parsed :states))
         (transitions (plist-get parsed :transitions))
         (bg (plist-get colors :bg))
         (fg (plist-get colors :fg))
         (surface (plist-get colors :surface))
         (border (plist-get colors :border))
         (font-size 14)
         (node-h 40)
         (node-pad-x 20)
         (layer-gap 80)
         (sibling-gap 40)
         ;; Topo-sort states using transitions as edges
         (sorted-ids (mark-graf-mermaid--flowchart-topo-sort states
                       (mapcar (lambda (t_)
                                 (list :from (plist-get t_ :from) :to (plist-get t_ :to)))
                               transitions)))
         (layer-alist (mark-graf-mermaid--flowchart-assign-layers sorted-ids
                        (mapcar (lambda (t_)
                                  (list :from (plist-get t_ :from) :to (plist-get t_ :to)))
                                transitions)))
         (max-layer (if layer-alist (apply #'max (mapcar #'cdr layer-alist)) 0))
         (layer-groups (make-hash-table :test 'equal))
         (node-pos (make-hash-table :test 'equal))
         (svg-parts nil)
         (defs-parts nil)
         (total-w 0)
         (total-h 0))
    ;; Group by layer
    (dolist (pair layer-alist)
      (puthash (cdr pair)
               (append (gethash (cdr pair) layer-groups nil) (list (car pair)))
               layer-groups))
    ;; Compute widths
    (let ((node-widths (make-hash-table :test 'equal)))
      (maphash (lambda (id info)
                 (let* ((type (plist-get info :type))
                        (label (plist-get info :label)))
                   (puthash id
                            (if (memq type '(start end)) 24
                              (max 70 (+ (mark-graf-mermaid--text-width label font-size)
                                         (* node-pad-x 2))))
                            node-widths)))
               states)
      ;; First pass: compute total-w
      (dotimes (layer (1+ max-layer))
        (let* ((layer-nodes (gethash layer layer-groups nil))
               (layer-w 0))
          (dolist (id layer-nodes)
            (setq layer-w (+ layer-w (gethash id node-widths 70) sibling-gap)))
          (setq layer-w (- layer-w sibling-gap))
          (setq total-w (max total-w (+ layer-w 40)))))
      ;; Second pass: position nodes centered
      (let ((y 20))
        (dotimes (layer (1+ max-layer))
          (let* ((layer-nodes (gethash layer layer-groups nil))
                 (layer-w 0))
            (dolist (id layer-nodes)
              (setq layer-w (+ layer-w (gethash id node-widths 70) sibling-gap)))
            (setq layer-w (- layer-w sibling-gap))
            (let ((x (/ (- total-w layer-w) 2)))
              (dolist (id layer-nodes)
                (let ((w (gethash id node-widths 70))
                      (h (let ((info (gethash id states)))
                           (if (memq (plist-get info :type) '(start end)) 24 node-h))))
                  (puthash id (list :x x :y y :w w :h h) node-pos)
                  (setq x (+ x w sibling-gap)))))
            (setq y (+ y node-h layer-gap))))
        (setq total-h (+ y 20))))
    ;; Defs
    (push (mark-graf-mermaid--svg-arrow-marker "state-arrow" fg) defs-parts)
    ;; Background
    (push (mark-graf-mermaid--svg-rect 0 0 total-w total-h 6 bg "none") svg-parts)
    ;; Draw transitions
    (dolist (tr transitions)
      (let* ((from-id (plist-get tr :from))
             (to-id (plist-get tr :to))
             (label (plist-get tr :label))
             (fp (gethash from-id node-pos))
             (tp (gethash to-id node-pos)))
        (when (and fp tp)
          (let* ((fx (+ (plist-get fp :x) (/ (plist-get fp :w) 2)))
                 (fy (+ (plist-get fp :y) (plist-get fp :h)))
                 (tx (+ (plist-get tp :x) (/ (plist-get tp :w) 2)))
                 (ty (plist-get tp :y)))
            (if (/= fx tx)
                (let ((mid-y (/ (+ fy ty) 2)))
                  (push (format "<path d=\"M %d %d C %d %d %d %d %d %d\" stroke=\"%s\" fill=\"none\" stroke-width=\"1.5\" marker-end=\"url(#state-arrow)\"/>"
                                fx fy fx mid-y tx mid-y tx ty fg)
                        svg-parts))
              (push (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1.5\" marker-end=\"url(#state-arrow)\"/>"
                            fx fy tx ty fg)
                    svg-parts))
            (when label
              (let ((lx (/ (+ fx tx) 2))
                    (ly (- (/ (+ fy ty) 2) 4)))
                (push (mark-graf-mermaid--svg-rect (- lx (/ (+ (mark-graf-mermaid--text-width label font-size) 8) 2))
                                                    (- ly (1+ font-size))
                                                    (+ (mark-graf-mermaid--text-width label font-size) 8)
                                                    (+ font-size 6)
                                                    3 bg "none")
                      svg-parts)
                (push (mark-graf-mermaid--svg-text lx ly label (1- font-size) fg) svg-parts)))))))
    ;; Draw states
    (maphash
     (lambda (id pos)
       (let* ((info (gethash id states))
              (type (plist-get info :type))
              (label (plist-get info :label))
              (x (plist-get pos :x))
              (y (plist-get pos :y))
              (w (plist-get pos :w))
              (h (plist-get pos :h))
              (cx (+ x (/ w 2)))
              (cy (+ y (/ h 2))))
         (pcase type
           ('start
            (push (mark-graf-mermaid--svg-circle cx cy 10 fg fg) svg-parts))
           ('end
            (push (mark-graf-mermaid--svg-circle cx cy 12 "none" fg) svg-parts)
            (push (mark-graf-mermaid--svg-circle cx cy 8 fg fg) svg-parts))
           (_
            (push (mark-graf-mermaid--svg-rect x y w h 8 surface border) svg-parts)
            (push (mark-graf-mermaid--svg-text cx (+ cy (/ font-size 3)) label font-size fg)
                  svg-parts)))))
     node-pos)
    ;; Assemble
    (mark-graf-mermaid--svg-wrap
     total-w total-h
     (mapconcat #'identity (nreverse defs-parts) "\n")
     (mapconcat #'identity (nreverse svg-parts) "\n"))))

;;; ============================================================
;;; Class Diagram Renderer
;;; ============================================================

(defun mark-graf-mermaid--parse-class (lines)
  "Parse class diagram LINES.
Returns (:classes CLASSES :relationships RELS).
CLASSES is hash-table name->(list :name :attrs :methods).
RELS is list of (:from FROM :to TO :type TYPE :label LABEL)."
  (let ((classes (make-hash-table :test 'equal))
        (relationships nil)
        (current-class nil))
    (dolist (raw-line (cdr lines))
      (let ((line (string-trim raw-line)))
        (cond
         ;; End of class body
         ((and current-class (string= line "}"))
          (setq current-class nil))
         ;; Inside class body - attribute or method
         (current-class
          (let ((cls (gethash current-class classes)))
            (when cls
              (if (string-match "(.*)" line)
                  (plist-put cls :methods
                             (append (plist-get cls :methods) (list line)))
                (plist-put cls :attrs
                           (append (plist-get cls :attrs) (list line)))))))
         ;; class Name { ... }
         ((string-match "^class\\s-+\\(\\S-+\\)\\s-*{?" line)
          (let ((name (match-string 1 line)))
            (unless (gethash name classes)
              (puthash name (list :name name :attrs nil :methods nil) classes))
            (when (string-match "{" line)
              (setq current-class name))))
         ;; class Name
         ((string-match "^class\\s-+\\(\\S-+\\)$" line)
          (let ((name (match-string 1 line)))
            (unless (gethash name classes)
              (puthash name (list :name name :attrs nil :methods nil) classes))))
         ;; Relationship: A <|-- B, A *-- B, A o-- B, A --> B, A -- B, A ..> B
         ((string-match
           "^\\(\\S-+\\)\\s-+\\(<|--\\|\\*--\\|o--\\|-->\\|--\\|\\.\\+>\\|\\.\\.>\\)\\s-+\\(\\S-+\\)\\(?:\\s-*:\\s-*\\(.*\\)\\)?"
           line)
          (let ((from (match-string 1 line))
                (rel (match-string 2 line))
                (to (match-string 3 line))
                (label (match-string 4 line)))
            (unless (gethash from classes)
              (puthash from (list :name from :attrs nil :methods nil) classes))
            (unless (gethash to classes)
              (puthash to (list :name to :attrs nil :methods nil) classes))
            (push (list :from from :to to
                        :type (cond
                               ((string= rel "<|--") 'inheritance)
                               ((string= rel "*--") 'composition)
                               ((string= rel "o--") 'aggregation)
                               ((string= rel "-->") 'dependency)
                               ((string-match-p "\\.\\." rel) 'realization)
                               (t 'association))
                        :label (when label (string-trim label)))
                  relationships)))
         ;; Colon syntax: ClassName : +method() or ClassName : -attr
         ((string-match "^\\(\\S-+\\)\\s-*:\\s-*\\(.*\\)" line)
          (let ((name (match-string 1 line))
                (member-str (string-trim (match-string 2 line))))
            (unless (gethash name classes)
              (puthash name (list :name name :attrs nil :methods nil) classes))
            (let ((cls (gethash name classes)))
              (if (string-match "(.*)" member-str)
                  (plist-put cls :methods
                             (append (plist-get cls :methods) (list member-str)))
                (plist-put cls :attrs
                           (append (plist-get cls :attrs) (list member-str))))))))))
    (list :classes classes :relationships (nreverse relationships))))

(defun mark-graf-mermaid--render-class (lines colors)
  "Render class diagram from LINES with COLORS. Returns SVG string."
  (let* ((parsed (mark-graf-mermaid--parse-class lines))
         (classes (plist-get parsed :classes))
         (relationships (plist-get parsed :relationships))
         (bg (plist-get colors :bg))
         (fg (plist-get colors :fg))
         (surface (plist-get colors :surface))
         (border (plist-get colors :border))
         (accent (plist-get colors :accent))
         (font-size 13)
         (line-h 20)
         (pad-x 14)
         (pad-y 10)
         (grid-gap-x 50)
         (grid-gap-y 50)
         ;; Collect class names
         (class-names nil)
         (class-pos (make-hash-table :test 'equal))
         (svg-parts nil)
         (defs-parts nil)
         (total-w 0)
         (total-h 0))
    (maphash (lambda (k _v) (push k class-names)) classes)
    (setq class-names (nreverse class-names))
    ;; Compute class box sizes
    (let ((class-sizes (make-hash-table :test 'equal)))
      (dolist (name class-names)
        (let* ((cls (gethash name classes))
               (attrs (plist-get cls :attrs))
               (methods (plist-get cls :methods))
               (name-w (mark-graf-mermaid--text-width name font-size))
               (max-w name-w)
               (section-lines 1)) ; name section
          ;; Measure attrs
          (dolist (a attrs)
            (setq max-w (max max-w (mark-graf-mermaid--text-width a font-size)))
            (setq section-lines (1+ section-lines)))
          ;; Measure methods
          (dolist (m methods)
            (setq max-w (max max-w (mark-graf-mermaid--text-width m font-size)))
            (setq section-lines (1+ section-lines)))
          ;; Add separators
          (when attrs (setq section-lines (1+ section-lines)))
          (when methods (setq section-lines (1+ section-lines)))
          (let ((w (+ max-w (* pad-x 2) 20))
                (h (+ (* section-lines line-h) (* pad-y 2))))
            (puthash name (list :w w :h h) class-sizes))))
      ;; Grid layout
      (let* ((cols (max 1 (ceiling (sqrt (length class-names)))))
             (x 20) (y 20)
             (col 0)
             (row-h 0))
        (dolist (name class-names)
          (let* ((size (gethash name class-sizes))
                 (w (plist-get size :w))
                 (h (plist-get size :h)))
            (puthash name (list :x x :y y :w w :h h) class-pos)
            (setq row-h (max row-h h))
            (setq total-w (max total-w (+ x w 20)))
            (setq col (1+ col))
            (setq x (+ x w grid-gap-x))
            (when (>= col cols)
              (setq col 0 x 20 y (+ y row-h grid-gap-y) row-h 0))))
        (setq total-h (+ y row-h 20))))
    ;; Markers
    (push (mark-graf-mermaid--svg-arrow-marker "class-arrow" fg) defs-parts)
    (push (mark-graf-mermaid--svg-arrow-marker "class-diamond" fg 'diamond) defs-parts)
    (push (mark-graf-mermaid--svg-arrow-marker "class-circle" fg 'circle) defs-parts)
    (push (format "<marker id=\"class-inherit\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"10\" markerHeight=\"10\" orient=\"auto-start-reverse\"><path d=\"M 0 0 L 10 5 L 0 10 z\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/></marker>"
                  bg fg)
          defs-parts)
    ;; Background
    (push (mark-graf-mermaid--svg-rect 0 0 total-w total-h 6 bg "none") svg-parts)
    ;; Draw relationships
    (dolist (rel relationships)
      (let* ((from-name (plist-get rel :from))
             (to-name (plist-get rel :to))
             (rel-type (plist-get rel :type))
             (label (plist-get rel :label))
             (fp (gethash from-name class-pos))
             (tp (gethash to-name class-pos)))
        (when (and fp tp)
          (let* ((fx (+ (plist-get fp :x) (/ (plist-get fp :w) 2)))
                 (fy (+ (plist-get fp :y) (plist-get fp :h)))
                 (tx (+ (plist-get tp :x) (/ (plist-get tp :w) 2)))
                 (ty (plist-get tp :y))
                 (marker (pcase rel-type
                           ('inheritance "class-inherit")
                           ('composition "class-diamond")
                           ('aggregation "class-circle")
                           (_ "class-arrow")))
                 (dash (if (memq rel-type '(dependency realization)) "5,5" nil)))
            ;; Connect from bottom of from-class to top of to-class
            ;; If they're on the same row, connect side-to-side instead
            (when (and (>= ty (plist-get fp :y))
                       (<= ty (+ (plist-get fp :y) (plist-get fp :h) 5)))
              (setq fx (+ (plist-get fp :x) (plist-get fp :w)))
              (setq fy (+ (plist-get fp :y) (/ (plist-get fp :h) 2)))
              (setq tx (plist-get tp :x))
              (setq ty (+ (plist-get tp :y) (/ (plist-get tp :h) 2))))
            (push (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1.5\" marker-end=\"url(#%s)\"%s/>"
                          fx fy tx ty fg marker
                          (if dash (format " stroke-dasharray=\"%s\"" dash) ""))
                  svg-parts)
            (when label
              (push (mark-graf-mermaid--svg-text (/ (+ fx tx) 2) (- (/ (+ fy ty) 2) 6)
                                                  label (1- font-size) fg)
                    svg-parts))))))
    ;; Draw class boxes
    (dolist (name class-names)
      (let* ((cls (gethash name classes))
             (pos (gethash name class-pos))
             (x (plist-get pos :x))
             (y (plist-get pos :y))
             (w (plist-get pos :w))
             (h (plist-get pos :h))
             (attrs (plist-get cls :attrs))
             (methods (plist-get cls :methods))
             (cy y))
        ;; Box background
        (push (mark-graf-mermaid--svg-rect x y w h 4 surface border) svg-parts)
        ;; Name section
        (setq cy (+ cy pad-y line-h))
        (push (mark-graf-mermaid--svg-text (+ x (/ w 2)) cy name font-size accent nil "bold") svg-parts)
        ;; Separator after name
        (when (or attrs methods)
          (setq cy (+ cy (/ pad-y 2)))
          (push (mark-graf-mermaid--svg-line x cy (+ x w) cy border) svg-parts)
          (setq cy (+ cy (/ pad-y 2))))
        ;; Attrs
        (dolist (a attrs)
          (setq cy (+ cy line-h))
          (push (mark-graf-mermaid--svg-text (+ x pad-x) cy a font-size fg "start") svg-parts))
        ;; Separator between attrs and methods
        (when (and attrs methods)
          (setq cy (+ cy (/ pad-y 2)))
          (push (mark-graf-mermaid--svg-line x cy (+ x w) cy border) svg-parts)
          (setq cy (+ cy (/ pad-y 2))))
        ;; Methods
        (dolist (m methods)
          (setq cy (+ cy line-h))
          (push (mark-graf-mermaid--svg-text (+ x pad-x) cy m font-size fg "start") svg-parts))))
    ;; Assemble
    (mark-graf-mermaid--svg-wrap
     total-w total-h
     (mapconcat #'identity (nreverse defs-parts) "\n")
     (mapconcat #'identity (nreverse svg-parts) "\n"))))

;;; ============================================================
;;; ER Diagram Renderer
;;; ============================================================

(defun mark-graf-mermaid--parse-er (lines)
  "Parse ER diagram LINES.
Returns (:entities ENTITIES :relationships RELS).
ENTITIES is hash-table name->(list :name :attrs).
RELS is list of (:from FROM :to TO :card-from CARD :card-to CARD :label LABEL)."
  (let ((entities (make-hash-table :test 'equal))
        (relationships nil)
        (current-entity nil))
    (dolist (raw-line (cdr lines))
      (let ((line (string-trim raw-line)))
        (cond
         ;; End of entity body
         ((and current-entity (string= line "}"))
          (setq current-entity nil))
         ;; Inside entity body
         (current-entity
          (when (string-match "^\\(\\S-+\\)\\s-+\\(\\S-+\\)\\(?:\\s-+\\(.*\\)\\)?" line)
            (let ((ent (gethash current-entity entities)))
              (when ent
                (plist-put ent :attrs
                           (append (plist-get ent :attrs)
                                   (list (list :type (match-string 1 line)
                                               :name (match-string 2 line)
                                               :constraint (match-string 3 line)))))))))
         ;; Entity with body
         ((string-match "^\\(\\S-+\\)\\s-*{" line)
          (let ((name (match-string 1 line)))
            (unless (gethash name entities)
              (puthash name (list :name name :attrs nil) entities))
            (setq current-entity name)))
         ;; Relationship: ENTITY ||--o{ OTHER : "label"
         ((string-match
           "^\\(\\S-+\\)\\s-+\\(||\\||o\\|}|\\|}o\\)--\\(||\\|o|\\|o{\\|{\\||{\\)\\s-+\\(\\S-+\\)\\s-*:\\s-*\\(.*\\)"
           line)
          (let ((from (match-string 1 line))
                (card-from (match-string 2 line))
                (card-to (match-string 3 line))
                (to (match-string 4 line))
                (label (string-trim (match-string 5 line) "\"" "\"")))
            (unless (gethash from entities)
              (puthash from (list :name from :attrs nil) entities))
            (unless (gethash to entities)
              (puthash to (list :name to :attrs nil) entities))
            (push (list :from from :to to
                        :card-from card-from :card-to card-to
                        :label label)
                  relationships))))))
    (list :entities entities :relationships (nreverse relationships))))

(defun mark-graf-mermaid--er-draw-notation (ex ey dx dy card color)
  "Draw crow's foot ER notation at entity edge (EX, EY).
DX, DY is the direction away from the entity along the relationship.
CARD is the cardinality string (e.g. \"||\" \"|o\" \"}|\" \"o{\").
COLOR is the stroke color.  Returns list of SVG element strings."
  (let* ((len (sqrt (+ (* (float dx) dx) (* (float dy) dy))))
         (ux (if (> len 0) (/ (float dx) len) 1.0))
         (uy (if (> len 0) (/ (float dy) len) 0.0))
         (px (- uy))   ; perpendicular unit vector
         (py ux)
         (tk 7)        ; tick half-length
         (parts nil)
         (has-one (string-match-p "|" card))
         (has-zero (string-match-p "o" card))
         (has-many (string-match-p "[{}]" card)))
    (cond
     ;; Exactly one (||): two perpendicular ticks
     ((and has-one (not has-many) (not has-zero))
      (dolist (d '(6 12))
        (let ((cx (+ ex (* ux d))) (cy (+ ey (* uy d))))
          (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"2\"/>"
                        (- cx (* px tk)) (- cy (* py tk))
                        (+ cx (* px tk)) (+ cy (* py tk)) color)
                parts))))
     ;; One or many (}| |{): tick + crow's foot
     ((and has-one has-many)
      (let ((cx (+ ex (* ux 6))) (cy (+ ey (* uy 6))))
        (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"2\"/>"
                      (- cx (* px tk)) (- cy (* py tk))
                      (+ cx (* px tk)) (+ cy (* py tk)) color)
              parts))
      (let* ((tip-x (+ ex (* ux 20))) (tip-y (+ ey (* uy 20)))
             (base-x (+ ex (* ux 10))) (base-y (+ ey (* uy 10))))
        (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"1.5\"/>"
                      tip-x tip-y (+ base-x (* px tk)) (+ base-y (* py tk)) color) parts)
        (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"1.5\"/>"
                      tip-x tip-y base-x base-y color) parts)
        (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"1.5\"/>"
                      tip-x tip-y (- base-x (* px tk)) (- base-y (* py tk)) color) parts)))
     ;; Zero or one (o| |o): tick + circle
     ((and has-zero has-one)
      (let ((cx (+ ex (* ux 6))) (cy (+ ey (* uy 6))))
        (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"2\"/>"
                      (- cx (* px tk)) (- cy (* py tk))
                      (+ cx (* px tk)) (+ cy (* py tk)) color)
              parts))
      (let ((ox (+ ex (* ux 17))) (oy (+ ey (* uy 17))))
        (push (format "<circle cx=\"%.0f\" cy=\"%.0f\" r=\"4\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1.5\"/>"
                      ox oy "none" color) parts)))
     ;; Zero or many (}o o{): circle + crow's foot
     ((and has-zero has-many)
      (let ((ox (+ ex (* ux 8))) (oy (+ ey (* uy 8))))
        (push (format "<circle cx=\"%.0f\" cy=\"%.0f\" r=\"4\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1.5\"/>"
                      ox oy "none" color) parts))
      (let* ((tip-x (+ ex (* ux 22))) (tip-y (+ ey (* uy 22)))
             (base-x (+ ex (* ux 14))) (base-y (+ ey (* uy 14))))
        (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"1.5\"/>"
                      tip-x tip-y (+ base-x (* px tk)) (+ base-y (* py tk)) color) parts)
        (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"1.5\"/>"
                      tip-x tip-y base-x base-y color) parts)
        (push (format "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"%s\" stroke-width=\"1.5\"/>"
                      tip-x tip-y (- base-x (* px tk)) (- base-y (* py tk)) color) parts))))
    parts))

(defun mark-graf-mermaid--render-er (lines colors)
  "Render ER diagram from LINES with COLORS. Returns SVG string."
  (let* ((parsed (mark-graf-mermaid--parse-er lines))
         (entities (plist-get parsed :entities))
         (relationships (plist-get parsed :relationships))
         (bg (plist-get colors :bg))
         (fg (plist-get colors :fg))
         (surface (plist-get colors :surface))
         (border (plist-get colors :border))
         (_accent (plist-get colors :accent))
         (font-size 11)
         (line-h 18)
         (pad-x 12)
         (pad-y 10)
         (grid-gap-x 120)
         (grid-gap-y 80)
         ;; Collect entity names
         (entity-names nil)
         (entity-pos (make-hash-table :test 'equal))
         (svg-parts nil)
         (defs-parts nil)
         (total-w 0)
         (total-h 0))
    (maphash (lambda (k _v) (push k entity-names)) entities)
    (setq entity-names (nreverse entity-names))
    ;; Compute entity box sizes
    (let ((entity-sizes (make-hash-table :test 'equal)))
      (dolist (name entity-names)
        (let* ((ent (gethash name entities))
               (attrs (plist-get ent :attrs))
               (name-w (mark-graf-mermaid--text-width name font-size))
               (max-w name-w)
               (num-lines (1+ (length attrs)))) ; name + attrs
          (dolist (a attrs)
            (let ((attr-text (concat (plist-get a :type) " " (plist-get a :name))))
              (setq max-w (max max-w (mark-graf-mermaid--text-width attr-text font-size)))))
          ;; Add separator line
          (when attrs (setq num-lines (1+ num-lines)))
          (puthash name (list :w (+ max-w (* pad-x 2) 14)
                              :h (+ (* num-lines line-h) (* pad-y 2)))
                   entity-sizes)))
      ;; Grid layout
      (let* ((cols (max 1 (ceiling (sqrt (length entity-names)))))
             (x 20) (y 20)
             (col 0)
             (row-h 0))
        (dolist (name entity-names)
          (let* ((size (gethash name entity-sizes))
                 (w (plist-get size :w))
                 (h (plist-get size :h)))
            (puthash name (list :x x :y y :w w :h h) entity-pos)
            (setq row-h (max row-h h))
            (setq total-w (max total-w (+ x w 20)))
            (setq col (1+ col))
            (setq x (+ x w grid-gap-x))
            (when (>= col cols)
              (setq col 0 x 20 y (+ y row-h grid-gap-y) row-h 0))))
        (setq total-h (+ y row-h 20))))
    ;; Background
    (push (mark-graf-mermaid--svg-rect 0 0 total-w total-h 6 bg "none") svg-parts)
    ;; Draw relationships
    (dolist (rel relationships)
      (let* ((from-name (plist-get rel :from))
             (to-name (plist-get rel :to))
             (label (plist-get rel :label))
             (card-from (plist-get rel :card-from))
             (card-to (plist-get rel :card-to))
             (fp (gethash from-name entity-pos))
             (tp (gethash to-name entity-pos)))
        (when (and fp tp)
          (let* ((fx (+ (plist-get fp :x) (plist-get fp :w)))
                 (fy (+ (plist-get fp :y) (/ (plist-get fp :h) 2)))
                 (tx (plist-get tp :x))
                 (ty (+ (plist-get tp :y) (/ (plist-get tp :h) 2))))
            ;; If same row but to is left of from, adjust
            (when (< tx fx)
              (setq fx (plist-get fp :x))
              (setq tx (+ (plist-get tp :x) (plist-get tp :w))))
            ;; If different rows: route toward target entity, not just center
            (when (> (abs (- fy ty)) 20)
              (let* ((fp-x (plist-get fp :x)) (fp-w (plist-get fp :w))
                     (tp-x (plist-get tp :x)) (tp-w (plist-get tp :w))
                     (tp-cx (+ tp-x (/ tp-w 2)))
                     (fp-cx (+ fp-x (/ fp-w 2)))
                     (margin 10))
                ;; From-entity: bottom edge, x offset toward target center
                (setq fx (max (+ fp-x margin) (min (- (+ fp-x fp-w) margin) tp-cx)))
                (setq fy (+ (plist-get fp :y) (plist-get fp :h)))
                ;; To-entity: top edge, x offset toward source center
                (setq tx (max (+ tp-x margin) (min (- (+ tp-x tp-w) margin) fp-cx)))
                (setq ty (plist-get tp :y))))
            ;; Main relationship line (shortened to leave room for notation)
            (let* ((ldx (- tx fx)) (ldy (- ty fy))
                   (llen (sqrt (+ (* (float ldx) ldx) (* (float ldy) ldy))))
                   (lux (if (> llen 0) (/ (float ldx) llen) 1.0))
                   (luy (if (> llen 0) (/ (float ldy) llen) 0.0))
                   (inset 22))
              (push (mark-graf-mermaid--svg-line
                     (round (+ fx (* lux inset))) (round (+ fy (* luy inset)))
                     (round (- tx (* lux inset))) (round (- ty (* luy inset)))
                     fg) svg-parts)
              ;; Crow's foot notation at from-entity
              (dolist (s (mark-graf-mermaid--er-draw-notation
                          fx fy ldx ldy card-from fg))
                (push s svg-parts))
              ;; Crow's foot notation at to-entity
              (dolist (s (mark-graf-mermaid--er-draw-notation
                          tx ty (- ldx) (- ldy) card-to fg))
                (push s svg-parts)))
            ;; Relationship label
            (when (and label (not (string-empty-p label)))
              (let ((lx (/ (+ fx tx) 2))
                    (ly (- (/ (+ fy ty) 2) 8)))
                (push (mark-graf-mermaid--svg-text lx ly label (1- font-size) fg) svg-parts)))))))
    ;; Draw entity boxes
    (dolist (name entity-names)
      (let* ((ent (gethash name entities))
             (pos (gethash name entity-pos))
             (x (plist-get pos :x))
             (y (plist-get pos :y))
             (w (plist-get pos :w))
             (h (plist-get pos :h))
             (attrs (plist-get ent :attrs)))
        ;; Box background
        (push (mark-graf-mermaid--svg-rect x y w h 4 surface border) svg-parts)
        (if (not attrs)
            ;; Simple entity: center name vertically in box
            (push (mark-graf-mermaid--svg-text
                   (+ x (/ w 2)) (+ y (/ h 2) (/ font-size 3))
                   name font-size fg) svg-parts)
          ;; Entity with attributes: name in header, attrs below separator
          (let* ((header-h (+ pad-y line-h (/ pad-y 2)))
                 (sep-y (+ y header-h))
                 (cy (+ sep-y (/ pad-y 2))))
            ;; Name centered in header region
            (push (mark-graf-mermaid--svg-text
                   (+ x (/ w 2)) (+ y (/ header-h 2) (/ font-size 3))
                   name font-size fg) svg-parts)
            ;; Separator line
            (push (mark-graf-mermaid--svg-line x sep-y (+ x w) sep-y border) svg-parts)
            ;; Attributes
            (dolist (a attrs)
              (setq cy (+ cy line-h))
              (let ((attr-text (concat (plist-get a :type) " " (plist-get a :name)))
                    (constraint (plist-get a :constraint)))
                (push (mark-graf-mermaid--svg-text (+ x pad-x) cy attr-text font-size fg "start") svg-parts)
                (when (and constraint (not (string-empty-p constraint)))
                  (push (mark-graf-mermaid--svg-text (- (+ x w) pad-x) cy constraint
                                                      (- font-size 2) border "end")
                        svg-parts))))))))
    ;; Assemble
    (mark-graf-mermaid--svg-wrap
     total-w total-h
     (if defs-parts (mapconcat #'identity (nreverse defs-parts) "\n") "")
     (mapconcat #'identity (nreverse svg-parts) "\n"))))

(provide 'mark-graf-mermaid)
;;; mark-graf-mermaid.el ends here

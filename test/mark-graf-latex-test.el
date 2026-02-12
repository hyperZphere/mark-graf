;;; mark-graf-latex-test.el --- Tests for LaTeX-to-Unicode conversion -*- lexical-binding: t; -*-

(require 'ert)
(require 'mark-graf-render)

(ert-deftest latex/simple-superscript ()
  "Test simple superscript conversion."
  (should (equal (mark-graf-render--latex-to-unicode "E = mc^2")
                 "E = mc\u00B2")))

(ert-deftest latex/greek-letters ()
  "Test Greek letter conversion."
  (should (equal (mark-graf-render--latex-to-unicode "\\alpha + \\beta")
                 "\u03B1 + \u03B2")))

(ert-deftest latex/subscripts ()
  "Test subscript conversion."
  (should (equal (mark-graf-render--latex-to-unicode "x_1 + x_2")
                 "x\u2081 + x\u2082")))

(ert-deftest latex/braced-subscript ()
  "Test braced subscript conversion."
  (should (equal (mark-graf-render--latex-to-unicode "x_{n+1}")
                 "x\u2099\u208A\u2081")))

(ert-deftest latex/frac ()
  "Test fraction conversion."
  (should (equal (mark-graf-render--latex-to-unicode "\\frac{a}{b}")
                 "a\u2044b")))

(ert-deftest latex/sqrt ()
  "Test square root conversion."
  ;; \pi -> π, then sqrt wraps it
  (should (equal (mark-graf-render--latex-to-unicode "\\sqrt{\\pi}")
                 "\u221A(\u03C0)")))

(ert-deftest latex/integral ()
  "Test integral with limits."
  (let ((result (mark-graf-render--latex-to-unicode
                 "\\int_{-\\infty}^{\\infty} e^{-x^2} dx")))
    ;; Should contain integral sign
    (should (string-match-p "\u222B" result))
    ;; Should contain infinity symbol
    (should (string-match-p "\u221E" result))
    ;; Should not loop infinitely (if we get here, it didn't)
    (should (stringp result))))

(ert-deftest latex/sum ()
  "Test summation."
  (let ((result (mark-graf-render--latex-to-unicode "\\sum_{i=1}^{n} i^2")))
    ;; Should contain sum sign
    (should (string-match-p "\u2211" result))
    ;; Should contain superscript 2
    (should (string-match-p "\u00B2" result))))

(ert-deftest latex/arrows ()
  "Test arrow conversion."
  (should (equal (mark-graf-render--latex-to-unicode "A \\Rightarrow B \\iff C")
                 "A \u21D2 B \u21D4 C")))

(ert-deftest latex/relations ()
  "Test relation conversion."
  (should (equal (mark-graf-render--latex-to-unicode "x \\leq y \\neq z")
                 "x \u2264 y \u2260 z")))

(ert-deftest latex/text-command ()
  "Test \\text{} stripping."
  (should (equal (mark-graf-render--latex-to-unicode "\\text{if } x > 0")
                 "if x > 0")))

(ert-deftest latex/empty-string ()
  "Test empty string."
  (should (equal (mark-graf-render--latex-to-unicode "") "")))

(ert-deftest latex/plain-text ()
  "Test plain text passthrough."
  (should (equal (mark-graf-render--latex-to-unicode "hello world")
                 "hello world")))

(ert-deftest latex/no-infinite-loop ()
  "Test that unconvertible superscripts don't cause infinite loops."
  (let ((result (mark-graf-render--latex-to-unicode "x^{\\infty}")))
    ;; Should complete without hanging
    (should (stringp result))
    ;; Should contain infinity
    (should (string-match-p "\u221E" result))))

(ert-deftest latex/mixed-expression ()
  "Test a complex mixed expression."
  (let ((result (mark-graf-render--latex-to-unicode
                 "\\forall x \\in \\mathbb{R}, \\exists y \\neq 0")))
    (should (string-match-p "\u2200" result))  ; forall
    (should (string-match-p "\u2208" result))  ; in
    (should (string-match-p "\u2260" result))  ; neq
    (should (stringp result))))

(ert-deftest latex/left-right-stripped ()
  "Test that \\left and \\right are removed."
  (should (equal (mark-graf-render--latex-to-unicode
                  "\\left( x \\right)")
                 "( x )")))

(ert-deftest latex/dots ()
  "Test ellipsis conversion."
  (should (equal (mark-graf-render--latex-to-unicode "a, \\ldots, z")
                 "a, \u2026, z")))

(provide 'mark-graf-latex-test)
;;; mark-graf-latex-test.el ends here

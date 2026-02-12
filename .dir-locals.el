;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (emacs-lisp-docstring-fill-column . 75)
  (checkdoc-minor-mode . t))
 (nil
  (projectile-project-compilation-cmd . "make compile")
  (projectile-project-test-cmd . "make test")))

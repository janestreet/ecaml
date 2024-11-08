;;; ecaml.el --- ELisp-side support for ECaml        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jane Street Capital

;; Keywords: extensions

;;; Code:

(require 'cl-lib)
(require 'find-func)

(defun ecaml--load-and-delete-history (file)
  ;; This shouldn't normally happen, but we may as well defend against it.
  ;;
  ;; However, we can't just `require' the file because the inline_tests_runner doesn't
  ;; actually `provide' a feature.
  (cl-assert (not (featurep 'ecaml_plugin)) nil "Loading the Ecaml plugin twice will segfault Emacs.")
  (load file nil 'nomessage)

  ;; Work around Emacs >=27 being too helpful.
  ;;
  ;; In solving https://debbugs.gnu.org/cgi/bugreport.cgi?bug=30164, Emacs upstream made
  ;; modules record load history, using the module binary artifact as the file name.
  ;; However, because this happens at the end of [load], it shadows all the load-history
  ;; entries we carefully set up!
  ;; See also https://debbugs.gnu.org/cgi/bugreport.cgi?bug=71522
  (let ((filename (find-library-name file)))
    (setq load-history
          (assoc-delete-all filename load-history))))

(defun ecaml-plugin-load ()
  (ecaml--load-and-delete-history
   (if (equal (getenv "TESTING_FRAMEWORK") "inline-test")
       "inline_tests_runner" "ecaml_plugin")))

(defmacro ecaml-profile (message &rest body)
  "Evaluate BODY, recording a profiling frame in the *profile* buffer.

If BODY takes longer to evaluate than
`ecaml-profile-hide-frame-if-less-than' or
`ecaml-profile-hide-top-level-if-less-than' (depending on
context), a profiling frame will be recorded in the appropriate
place in a new entry in *profile*.  If such a profiling frame is
rendered, MESSAGE is also included therein."
  (declare (indent 1))
  `(ecaml-profile-inner ,message (lambda () ,@body)))

(provide 'ecaml)
;;; ecaml.el ends here

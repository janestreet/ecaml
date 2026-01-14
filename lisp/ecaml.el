;;; ecaml.el --- ELisp-side support for ECaml        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jane Street Capital

;; Keywords: extensions

;;; Code:

(require 'cl-lib)
(require 'find-func)
(require 'jane-ecaml nil t)

(defmacro ecaml-profile (message &rest body)
  "Evaluate BODY, recording a profiling frame in the *profile* buffer.

If BODY takes longer to evaluate than
`ecaml-profile-hide-frame-if-less-than' or
`ecaml-profile-hide-top-level-if-less-than' (depending on
context), a profiling frame will be recorded in the appropriate
place in a new entry in *profile*.  If such a profiling frame is
rendered, MESSAGE is also included therein."
  (declare (indent 1))
  `(ecaml-profile--inner ,message (lambda () ,@body)))

(defvar ecaml-profiled-functions nil
  "A list of function symbols that have been profiled by Ecaml.")

(defun ecaml-profile-elisp-function (function)
  "Record a profiling frame in the *profile* buffer whenever FUNCTION is called."
  (interactive "aProfile function: ")
  (cl-assert (symbolp function))
  (unless (member function ecaml-profiled-functions)
    (advice-add function :around
                (lambda (orig-fn &rest args)
                  (apply
                   #'ecaml-profile--inner
                   function
                   orig-fn
                   args))
                '((name . ecaml-profile--advice)))
    (push function ecaml-profiled-functions)
    (message "You just added Ecaml profiling of %s" function)))

(defun ecaml-unprofile-elisp-function (function)
  "Stop recording profiling frames in the *profile* buffer for FUNCTION."
  (interactive (list (intern (completing-read "Unprofile function: " ecaml-profiled-functions))))
  (cl-assert (member function ecaml-profiled-functions) nil
             "%s is not being profiled" function)
  ;; Removing with the same name we used in `ecaml-profile-elisp-function'.
  (advice-remove function 'ecaml-profile--advice)
  (setq ecaml-profiled-functions (delq function ecaml-profiled-functions))
  (message "You just removed Ecaml profiling of %s" function))

(provide 'ecaml)
;;; ecaml.el ends here

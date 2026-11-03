;;; ecaml-early.el --- ELisp-side Ecaml support loaded before the plugin  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Jane Street Capital

;; Keywords: extensions

;;; Commentary:

;; Definitions which the Ecaml plugin calls into and which must be available as
;; soon as the plugin is loaded.  For example, the plugin calls
;; `ecaml-profile-tag-frame' as soon as it starts profiling, which happens while
;; the plugin itself is still loading.
;;
;; Unlike ecaml.el, this file does not depend on the plugin.

;;; Code:

(defvar ecaml-profile-tag-frame-function nil
  "A function of no arguments called when creating a profile frame.

The return value of this function is added to the profile frame, to
provide additional context (e.g., the current buffer).  It is called on
every profile frame, so it should be fast and should not call into
Ecaml.

This is called via `ecaml-profile-tag-frame'.")

(defvar ecaml-profile-tag-frame--last-context nil
  "The context most recently returned by `ecaml-profile-tag-frame'.")

(defun ecaml-profile-tag-frame (&optional new-profile)
  "Return extra context to add to the profile frame being created.

Calls `ecaml-profile-tag-frame-function' if it is set, and returns its
result.  A return value of nil means no extra context.

To avoid the cost of storing identical context on every frame, return
nil when the context is unchanged since the previous frame.  NEW-PROFILE
non-nil means the frame being created starts a new profile (it has no
enclosing frame), in which case the context is returned even if
unchanged, so that every profile has context on its outermost frame."
  (when ecaml-profile-tag-frame-function
    (let ((context (funcall ecaml-profile-tag-frame-function)))
      (if (and (not new-profile)
               (equal context ecaml-profile-tag-frame--last-context))
          nil
        (setq ecaml-profile-tag-frame--last-context context)
        context))))

(provide 'ecaml-early)
;;; ecaml-early.el ends here

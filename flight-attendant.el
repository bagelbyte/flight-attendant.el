;;; flight-attendant.el --- Major mode to provide Github Copilot support -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;;
;; Author: fkr <https://github.com/fkr-0>
;; Maintainer: fkr_0
;; Created: November 28, 2021
;; Modified: November 28, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/wintermute/flight-attendant
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Major mode to provide Github Copilot support
;;
;;; Code:
;;;

(require 'json)
(require 'flight-attendant-rpc)
(require 'flight-attendant-ui)
(require 'cl-lib)   ; for `cl-every', `cl-copy-list', `cl-delete-if-not'
(require 'subr-x)
(defvar fa-enabled-p nil
  "Is Fa enabled or not.")
(defconst fa-version "0.05.0"
  "The version of the Fa Lisp code.")


(defcustom language-ids
  '((python-mode . "python")
    (emacs-lisp-mode . "emacs-lisp")
    (sh-mode . "sh"))
  ;; (markdown . "markdown")
  ;; (typescript-mode . "typescript")
  ;; (org-mode . "org-mode")
  ;; (javascript-mode . "javascript")
  ;; (rjsx-mode . "javascript")
  ;; (gfm-mode . "markdown")
  ;; (rustic-mode . "rust"))
  "Languages for which Copilot should request completion. For an overview of possible modes refer to 'auto-mode-alist'."
  :type 'list
  :group 'fa)


(defgroup fa nil
  "Emacs copilot."
  :prefix "fa-"
  :group 'languages)

(defcustom fa-modules '()
  "TODO - additional modifications."
  :type 'list
  :group 'fa)

(defcustom fa-mode-hook nil
  "Hook run when `fa-mode' is enabled.
This can be used to enable minor modes when flight-attendant is used. E.g. to disable plugins messing with the visuals."
  :type 'hook
  :options '(subword-mode hl-line-mode)
  :group 'fa)

;;;###autoload
(defun flight-attendant (&optional _ignored)
  "Toggle flight attendant."
  (interactive)
  (if fa-enabled-p
      (fa-disable)
    (fa-enable)))

(defun fa-enable (&optional)
  "Enable Fa for all defined languages."
  (interactive)
  (and fa-enabled-p
       (message "Fa already enabled.")
       (when (not (assq major-mode language-ids))
         (warn (concat "The current major mode is not"
                       "listed in 'language-ids'."))))
  (unless fa-enabled-p
    (fa-modules-global-init)
    ;; (add-hook 'python-mode-hook 'fa-mode)
    ;; (add-hook 'pyvenv-post-activate-hooks 'fa-rpc--disconnect)
    ;; (add-hook 'pyvenv-post-deactivate-hooks 'fa-rpc--disconnect)
    ;; (add-hook 'inferior-python-mode-hook 'fa-shell--enable-output-filter)
    ;; (add-hook 'python-shell-first-prompt-hook 'fa-shell--send-setup-code t)
    ;; Add codecell boundaries highligting
    ;; (font-lock-add-keywords
    ;;  'python-mode
    ;;  `((,(replace-regexp-in-string "\\\\" "\\\\"
    ;;                                fa-shell-cell-boundary-regexp)
    ;;     0 'fa-codecell-boundary prepend)))
    ;; ;; Enable Fa-mode in the opened buffer
    (setq fa-enabled-p t)
    (dolist (buffer (buffer-list))
      (and (not (string-match "^ ?\\*" (buffer-name buffer)))
           (with-current-buffer buffer
             (when (assq major-mode language-ids)
               (fa-mode t)))))))

(defun fa-disable ()
  "Disable Fa."
  (interactive)
  (fa-modules-global-stop)
  ;; (define-key inferior-python-mode-map (kbd "C-c C-z") nil)
  ;; (remove-hook 'python-mode-hook 'fa-mode)
  ;; Remove codecell boundaries highligting
  (fa--remove-hooks)
  (remove-hook 'python-mode-hook 'fa-mode)
  (setq fa-enabled-p nil))

(defun fa--remove-hooks ()
  (remove-hook 'evil-insert-state-entry-hook 'fa--enter-insert)
  (remove-hook 'evil-insert-state-exit-hook 'fa-clear-overlay)
  (remove-hook 'post-self-insert-hook 'fa--on-insert))

(define-minor-mode fa-mode
  "Minor mode for requesting auto-completion suggestions provided by GitHub Copilot.
\\{fa-mode-map}"
  :lighter " Fa"
  (unless (assq major-mode language-ids)
    (warn "flight-attendant.el is not enabled for this major mode."))
  (unless fa-enabled-p
    (error "Please enable flight-attendant with 'flight-attendant' before using it"))
  ;; (when (boundp 'xref-backend-functions)
  ;;   (add-hook 'xref-backend-functions #'fa--xref-backend nil t)))
  ;; Set this for `fa-check' command
  ;; (setq-local python-check-command fa-syntax-check-command)
  (cond
   (fa-mode
    (message "Activating FA-Mode")
    (add-to-list 'mode-line-misc-info
                 `(fa-mode (" [COPILOT] ")))
    ;; enable
    ;; (fa-modules-buffer-init)
    (fa--add-hooks)
    (when (not gh-token)
      (fa-rpc-get-token)))
   ((not fa-mode)
    ;; disable
    (fa--remove-hooks)
    (delete 'mode-line-misc-info
            `(fa-mode (" [COPILOT] "))))))
;; (fa-modules-buffer-stop)))

(defun fa--add-hooks ()
  (add-hook 'evil-insert-state-entry-hook 'fa--enter-insert)
  (add-hook 'evil-insert-state-exit-hook 'fa-clear-overlay)
  (add-hook 'post-self-insert-hook 'fa--on-insert))

(defun fa--enter-insert ()
  (when fa-mode
    (fa-clear-overlay)
    (fa-rpc-request-completion
     (lambda (completion)
       (message (cdr (assq 'displayText (car (cdr (assq 'completions completion))))))
       (let ((display-text (cdr (assq 'displayText (car (cdr (assq 'completions completion))))))
             (pos-line (cdr (car (cdr (assq 'position (car (cdr (assq 'completions completion))))))))
             (pos-char (cdr (assq 'character (cdr (assq 'position (car (cdr (assq 'completions completion)))))))))
         (and
          (message display-text)
          (fa-display-overlay-str display-text pos-line pos-char)))))))

(defun fa--on-insert ()
  (when fa-mode
    (fa-clear-overlay)
    (fa-rpc-request-completion
     (lambda (completion)
       (let ((display-text (cdr (assq 'displayText (car (cdr (assq 'completions completion))))))
             (pos-line (cdr (car (cdr (assq 'position (car (cdr (assq 'completions completion))))))))
             (pos-char (cdr (assq 'character (cdr (assq 'position (car (cdr (assq 'completions completion)))))))))
         (and
          (message display-text)
          (fa-display-overlay-str display-text pos-line pos-char)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Sane Defaults

(defun fa-module-fix-company (command &rest _args)
  "TODO Make overlays work with company"
  (pcase command
    (`buffer-init
     ;; Set `forward-sexp-function' to nil in python-mode. See
     ;; http://debbugs.gnu.org/db/13/13642.html

     (when (and (variable-at-point 'company-mode-on) (company-mode-on)
                (variable-at-point 'company-box-mode) (not company-box-mode)
            (warn "Company without childframes will mess up the display."))))))

;;;;;;;;;;;
;;; Modules

(defvar fa-modules-initialized-p nil
  "Boolean, set to true if modules were run with `global-init'.")

(defun fa-modules-run (command &rest args)
  "Run COMMAND with ARGS for all modules in `fa-modules'."
  (dolist (module fa-modules)
    (apply module command args)))

(defun fa-modules-global-init ()
  "Run the global-init method of Fa modules.

Make sure this only happens once."
  (unless fa-modules-initialized-p
    (fa-modules-run 'global-init)
    (setq fa-modules-initialized-p t)))

(defun fa-modules-global-stop ()
  "Run the global-stop method of Fa modules.

Make sure this only happens once per global-init call."
  (when fa-modules-initialized-p
    (fa-modules-run 'global-stop)
    (setq fa-modules-initialized-p nil)))

(defun fa-modules-buffer-init ()
  "Run the buffer-init method of Fa modules.

Make sure global-init is called first."
  (fa-modules-global-init)
  (fa-modules-run 'buffer-init))

(defun fa-modules-buffer-stop ()
  "Run the buffer-stop method of Fa modules."
  (fa-modules-run 'buffer-stop))


(provide 'flight-attendant)
;;; flight-attendant.el ends here

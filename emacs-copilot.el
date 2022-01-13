;;; emacs-copilot.el --- Major mode to provide Github Copilot support -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;;
;; Author: cryptobadger <https://github.com/cryptobadger>
;; Maintainer: cryptobadger
;; Created: November 28, 2021
;; Modified: November 28, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/wintermute/emacs-copilot
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

(add-to-list 'load-path ".")
(message "emacs-copilot.el loaded")
(require 'json)
;; (add-load-path! ".")
(require 'emacs-copilot-rpc)
(require 'emacs-copilot-ui)
(require 'cl-lib)   ; for `cl-every', `cl-copy-list', `cl-delete-if-not'
(require 'subr-x)
(defvar ec-enabled-p nil
  "Is Ec enabled or not.")
(defconst ec-version "0.05.0"
  "The version of the Ec Lisp code.")

(defgroup ec nil
  "Emacs copilot."
  :prefix "ec-"
  :group 'languages)

(defcustom ec-mode-hook nil
  "Hook run when `ec-mode' is enabled.

This can be used to enable minor modes for Python development."
  :type 'hook
  :options '(subword-mode hl-line-mode)
  :group 'ec)

;;;###autoload
(defun ec-enable (&optional _ignored)
  "Enable Ec in all future Python buffers."
  (interactive)
  (and ec-enabled-p
       (message "Ec already enabled."))
  (unless ec-enabled-p
    (when (< emacs-major-version 24)
      (error "Ec requires Emacs 24 or newer"))
    (when _ignored
      (warn "The argument to `ec-enable' is deprecated, customize `ec-modules' instead"))
    (let ((filename (find-lisp-object-file-name 'python-mode
                                                'symbol-function)))
      (when (and filename
                 (string-match "/python-mode\\.el\\'"
                               filename))
        (error (concat "You are using python-mode.el. "
                       "Ec only works with python.el from "
                       "Emacs 24 and above"))))
    ;; (ec-modules-global-init)
    ;; (define-key inferior-python-mode-map (kbd "C-c C-z") 'ec-shell-switch-to-buffer)
    (add-hook 'python-mode-hook 'ec-mode)
    ;; (add-hook 'pyvenv-post-activate-hooks 'ec-rpc--disconnect)
    ;; (add-hook 'pyvenv-post-deactivate-hooks 'ec-rpc--disconnect)
    ;; (add-hook 'inferior-python-mode-hook 'ec-shell--enable-output-filter)
    ;; (add-hook 'python-shell-first-prompt-hook 'ec-shell--send-setup-code t)
    ;; Add codecell boundaries highligting
    ;; (font-lock-add-keywords
    ;;  'python-mode
    ;;  `((,(replace-regexp-in-string "\\\\" "\\\\"
    ;;                                ec-shell-cell-boundary-regexp)
    ;;     0 'ec-codecell-boundary prepend)))
    ;; ;; Enable Ec-mode in the opened python buffer
    (setq ec-enabled-p t)
    (dolist (buffer (buffer-list))
      (and (not (string-match "^ ?\\*" (buffer-name buffer)))
           (with-current-buffer buffer
             (when (string= major-mode 'python-mode)
               (python-mode)  ;; update codecell fontification
               (ec-mode t)))))))


(defun ec-disable ()
  "Disable Ec in all future Python buffers."
  (interactive)
  ;; (ec-modules-global-stop)
  ;; (define-key inferior-python-mode-map (kbd "C-c C-z") nil)
  ;; (remove-hook 'python-mode-hook 'ec-mode)
  ;; Remove codecell boundaries highligting
  (ec--remove-hooks)
  (remove-hook 'python-mode-hook 'ec-mode)
  (setq ec-enabled-p nil))

(defun ec--remove-hooks ()
  (remove-hook 'evil-insert-state-entry-hook 'ec--enter-insert)
  (remove-hook 'evil-insert-state-exit-hook 'ec-clear-overlay)
  (remove-hook 'post-self-insert-hook 'ec--on-insert))
(define-minor-mode ec-mode
  "Minor mode in Python buffers for the Emacs Lisp Python Environment.
\\{ec-mode-map}"
  :lighter " Ec"
  (unless (derived-mode-p 'python-mode)
    (error "Ec only works with `python-mode'"))
  (unless ec-enabled-p
    (error "Please enable Ec with `(ec-enable)` before using it"))
;; (when (boundp 'xref-backend-functions)
;;   (add-hook 'xref-backend-functions #'ec--xref-backend nil t)))
;; Set this for `ec-check' command
;; (setq-local python-check-command ec-syntax-check-command)
 (cond
  (ec-mode
   (message "Activating EC-Mode")
   (add-to-list 'mode-line-misc-info
                `(ec-mode (" [COPILOT] ")))
   ;; enable
   ;; (ec-modules-buffer-init)
   (ec--add-hooks)
   (when (not gh-token)
     (ec-rpc-get-token)))
  ((not ec-mode)
   ;; disable
   (ec--remove-hooks))))
  ;; (ec-modules-buffer-stop)))

(defun ec--add-hooks ()
  (add-hook 'evil-insert-state-entry-hook 'ec--enter-insert)
  (add-hook 'evil-insert-state-exit-hook 'ec-clear-overlay)
  (add-hook 'post-self-insert-hook 'ec--on-insert))

(defun ec--enter-insert ()
  (ec-clear-overlay)
  (message "inserting")
  (ec-rpc-request-completion
   (lambda (completion)
     (let ((display-text (cdr (assq 'displayText (car (cdr (assq 'completions completion))))))
           (pos-line (cdr (car (cdr (assq 'position (car (cdr (assq 'completions completion))))))))
           (pos-char (cdr (assq 'character (cdr (assq 'position (car (cdr (assq 'completions completion)))))))))
      (and
        (message display-text)
        (ec-display-overlay-str display-text pos-line pos-char))))))

(defun ec--on-insert ()
  (ec-clear-overlay)
  (message "oninsert")
  (ec-rpc-request-completion
   (lambda (completion)
     (let ((display-text (cdr (assq 'displayText (car (cdr (assq 'completions completion))))))
           (pos-line (cdr (car (cdr (assq 'position (car (cdr (assq 'completions completion))))))))
           (pos-char (cdr (assq 'character (cdr (assq 'position (car (cdr (assq 'completions completion)))))))))
      (and
        (message display-text)
        (ec-display-overlay-str display-text pos-line pos-char))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Sane Defaults

(defun ec-module-sane-defaults (command &rest _args)
  "Module for sane Emacs default for python."
  (pcase command
    (`buffer-init
     ;; Set `forward-sexp-function' to nil in python-mode. See
     ;; http://debbugs.gnu.org/db/13/13642.html
     (set (make-local-variable 'forward-sexp-function) nil)
     ;; PEP8 recommends two spaces in front of inline comments.
     (set (make-local-variable 'comment-inline-offset) 2))
    (`buffer-stop
     (kill-local-variable 'forward-sexp-function)
     (kill-local-variable 'comment-inline-offset))))


;;;;;;;;;;;
;;; Modules

(defvar ec-modules-initialized-p nil
  "Boolean, set to true if modules were run with `global-init'.")

(defun ec-modules-run (command &rest args)
  "Run COMMAND with ARGS for all modules in `ec-modules'."
  (dolist (module ec-modules)
    (apply module command args)))

(defun ec-modules-global-init ()
  "Run the global-init method of Ec modules.

Make sure this only happens once."
  (unless ec-modules-initialized-p
    (ec-modules-run 'global-init)
    (setq ec-modules-initialized-p t)))

(defun ec-modules-global-stop ()
  "Run the global-stop method of Ec modules.

Make sure this only happens once per global-init call."
  (when ec-modules-initialized-p
    (ec-modules-run 'global-stop)
    (setq ec-modules-initialized-p nil)))

(defun ec-modules-buffer-init ()
  "Run the buffer-init method of Ec modules.

Make sure global-init is called first."
  (ec-modules-global-init)
  (ec-modules-run 'buffer-init))

(defun ec-modules-buffer-stop ()
  "Run the buffer-stop method of Ec modules."
  (ec-modules-run 'buffer-stop))


(provide 'emacs-copilot)
;;; emacs-copilot.el ends here

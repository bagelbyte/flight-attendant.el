;;; emacs-copilot-ui.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;;
;; Author: cryptobadger <https://github.com/cryptobadger>
;; Maintainer: cryptobadger
;; Created: December 02, 2021
;; Modified: December 02, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cryptobadger/cryptobadger/TODO
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(message "emacs-copilot-ui.el loaded")
(defface ec-overlay-face
  '((t :inherit shadow))
  "The face used for code lens overlays."
  :group 'ec-overlay)

(defface ec-overlay-mouse-face
  '((t :box t))
  "The face used for code lens overlays."
  :group 'ec-overlay)

(defvar ec-overlay nil)
 
(defun ec-display-overlay-str (str line col)
  "Displays STR at LINE, COL."
  (overlay-recenter (point-max))
  (ec-clear-overlay)
  (save-excursion
    (widen)
    (goto-char 1)
    (let* ((l1 (+ line 1))
           ov
           start
           end
           (propstr (propertize str
                               'face 'ec-overlay-face
                               'mouse-face 'ec-overlay-mouse-face)))
                               ;; 'local-map map
      ;; (pcase ccls-code-lens-position
      ;;   ('end
      ;;    (forward-line (- l0 line))
      ;;    (if (and ov (= l0 line))
      ;;        (overlay-put ov 'display
      ;;                     (concat (overlay-get ov 'display)
      ;;                             str))
      ;;      (when ov
      ;;        (overlay-put ov 'display (concat (overlay-get ov 'display) "\n")))
      ;;      (let ((p (point-at-eol)))
      ;;        (setq ov (make-overlay p (1+ p) nil 'front-advance))
      ;;        (overlay-put ov 'ec-overlay-str t)
      ;;        (overlay-put ov 'display str)))
      ;;    (setq line l0 col c0))
      ;; ('inplace
       (forward-line line)
       (forward-char col)
       (setq line l1)
       (setq start (point))
       (insert propstr)
       (setq end (point))
       (setq ov (make-overlay start end nil t))
       (overlay-put ov 'ec-overlay-str t)
       (overlay-put ov 'face 'ec-overlay-face)
       (overlay-put ov 'insert-before-hooks (lambda (st en reg) (message "imthere")))
       (setq ec-overlay ov))))

       ;; (overlay-put ov 'after-string propstr))))

      ;; (When (and (eq ccls-code-lens-position 'end) ov)
      ;;   (overlay-put ov 'display (concat (overlay-get ov 'display) "\n"))))))

(defun ec-clear-overlay ()
  "Clear all overlays from this buffer."
  (interactive)
  (and ec-overlay
       (let ((ov-start (overlay-start ec-overlay))
             (ov-end (overlay-end ec-overlay)))
          (and ov-start ov-end
              (delete-region (overlay-start ec-overlay) (overlay-end ec-overlay)))))
  (remove-overlays (point-min) (point-max) 'ec-overlay-str t))

(provide 'emacs-copilot-ui)
;;; emacs-copilot-ui.el ends here

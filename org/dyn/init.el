;;; .emacs --- Emacs startup file.

;; Author:  Yoshinari Nomura
;; Created: 2010-01-14 13:59:02 JST
;; Revised Time-stamp: <2015-07-26 23:33:24 JST>

;;; Commentary:

;;; Code:

;;; For Debug

;; M-x toggle-debug-on-error
;; (setq debug-on-error t)

;;; Disable Bi-Directional Display

(setq-default
 bidi-display-reordering nil
 bidi-paragraph-direction 'left-to-right)

;;; Add Function(s) for Compatibility

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

;;; Basic Keybinds

;; C-h is BS, C-d is DEL
(load-library "term/bobcat")
(terminal-init-bobcat)
(setq normal-erase-is-backspace nil)

;; C-zh to help
(global-unset-key "\C-z")
(global-set-key "\C-zh" 'help)
(global-set-key "\C-z\C-z" 'suspend-emacs)

;; SPC always acts as complation

;; Space works as completion-key in minibuffer
(define-key minibuffer-local-filename-completion-map
  " " 'minibuffer-complete-word)

;;; Load Path and Directory Settings

(setenv "TEXINPUTS" (format "sty:%s/sys/lib/TeXsty:" (getenv "HOME")))

(defvar emacs-exec-path
  '("~/sys/lib/elisp/mew/current/bin"
    "~/sys/lib/elisp/migemo/bin"))

(defvar shell-exec-path
  (split-string
   (with-temp-buffer
     (call-process
      (getenv "SHELL") nil '(t nil) nil "-c" "echo -n $PATH")
     (buffer-substring-no-properties (point-min) (point-max)))
   ":"))

;; set exec-path from shell-exec-path and emacs-exec-path
(dolist (ent (reverse (append emacs-exec-path shell-exec-path)))
  (let ((dir (expand-file-name ent)))
    (when (and (file-exists-p dir) (not (member dir exec-path)))
      (setq exec-path (cons dir exec-path))
      (setenv "PATH" (concat dir ":" (getenv "PATH"))))))

(defun hostname ()
  "Short hostname."
  (car (split-string (system-name) "\\.")))

;;; Cask Package Manager

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;; Enable Commands

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Buffer-name Uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;; search

(setq isearch-case-fold-search t)

;;; global mode

(setq transient-mark-mode nil)

;;; Emacs Server

(require 'server)
(unless (server-running-p)
  (server-start))


;;; eldoc

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

;;; Load individual init files

;; Prepend all subdirectories of `default-directory' to `load-path'.

(let ((default-directory "~/sys/lib/elisp"))
  (if (file-directory-p default-directory)
      (setq load-path
            (append
             (let ((load-path (copy-sequence load-path))) ;; Shadow
               (normal-top-level-add-subdirs-to-load-path))
             load-path))))

;;; Load all files in `private-init-directory'.

(defvar private-init-directory
  "~/sys/rc/elisp-init/"
  "My private init directory.")

(defvar private-load-path-top "~/sys/lib/elisp/")

(defun private-load-path (relative-path)
  "Calculate absolute path of RELATIVE-PATH from `private-load-path-top'."
  (expand-file-name relative-path private-load-path-top))

(defun add-private-load-path (relative-path)
  "Add RELATIVE-PATH to `load-path' as a relative path from `private-load-path-top'."
  (setq load-path (cons (private-load-path relative-path) load-path)))

(mapc 'load (directory-files private-init-directory t "^[0-9].*.elc?$"))

;;; Load Custom file

(setq custom-file
      (expand-file-name
       "XX-emacs-custom.el" private-init-directory))
(load custom-file)

;;; Restore frame configuration

(if (fboundp 'load-frame-configuration)
    (load-frame-configuration))

;;; Done

(message "Done.")
(provide '.emacs)

;;; Local Variables:
;;; mode: emacs-lisp
;;; time-stamp-active: t
;;; End:
;;; .emacs ends here

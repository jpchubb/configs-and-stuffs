(add-to-list 'load-path "~/.emacs.d/") ;; user load path
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/")
;;;; Prettify emacs ;;;;

(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(setq inhibit-startup-message t
      column-number-mode 1
      echo-keystrokes 0.1
      show-trailing-whitespace 1)

;;;; extra keybindings ;;;;
(global-set-key (kbd "C-c 1") 'erc)
(global-set-key (kbd "C-c 2") 'scratch)
(global-set-key (kbd "C-c 0") 'switch-to-dot-emacs)

;;;; IDE useful stuffs ;;;;

;;;; Major Modes ;;;;
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode) )
;(require 'haskkell)
;;;; Minor Modes ;;;;
(require 'scratch-mode) ;; funky mode sensitive scratch buffers

(autoload 'tramp "Remote file manipulation in TRAMP." t)

(require 'ido) ;; interactively do shit

;;; Use "%" to jump to the matching parenthesis.
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert
the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t                    (self-insert-command (or arg 1))) ))
(global-set-key (kbd "C-%") `goto-match-paren)

;;;; formatting ;;;;

;; Linus' kernel formatting
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))
(setq make-backup-files nil)
(line-number-mode 1)

;;;; Custom functions ;;;;
(defun switch-to-dot-emacs (&rest junk)
  "Switch to .emacs file"
  (interactive)
  (if (equal (buffer-name) ".emacs")
      (eval-buffer)
    (find-file "~/.emacs")))
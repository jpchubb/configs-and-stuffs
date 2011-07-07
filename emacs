(add-to-list 'load-path "~/.emacs.d/") ;; user load path
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/")
(require 'org-entities)
;;(add-to-list
;;;; Prettify emacs ;;;;
(require 'zenburn)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(menu-bar-mode -1)
  ;(setq frame-title-format "%b" icon title format "%b")

  (fringe-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
(when window-system
  (zenburn)
  (set-frame-height (selected-frame) 38)
  (set-frame-width (selected-frame) 143)
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities))


(setq inhibit-startup-message t
      column-number-mode 1
      echo-keystrokes 0.1
      show-trailing-whitespace 1)

(set-face-attribute 'default nil :height 92)

;;pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode '(("(\\(lambda\\)\\>" (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ?λ))))))

(add-to-list 'org-entities '("neg" "\\neg" t "&neg;" "[negation]" "[negation]" "¬"))
(add-to-list 'org-entities '("vdash" "\\vdash" t "&vdash;" "[vdash]" "[vdash]" "⊢"))
(add-to-list 'org-entities '("iff" "\\iff" t "&iff;" "[if and only if]" "[if and only if]" ""))
(add-to-list 'org-entities '("top" "\\top" t "&top;" "[Top, true]" "[Top, true]" "⊤"))
(add-to-list 'org-entities '("bot" "\\bot" t "&bot;" "[Bot, false]" "[Bot, false]" "⊥"))
(add-to-list 'org-entities '("langle" "\\langle" t "&blah;" "[angle bracket left]" "[angle bracket left]" "⟨"))
(add-to-list 'org-entities '("rangle" "\\rangle" t "&blah;" "[angle bracket right]" "[angle bracket right]" "⟩"))


;;;; extra keybindings ;;;;
(global-set-key (kbd "C-c 1") 'dired)
(global-set-key (kbd "C-c 2") 'switch-to-screenrc)
(global-set-key (kbd "C-c 0") 'switch-to-dot-emacs)
(global-set-key (kbd "C-c 3") 'switch-to-xmonadhs)
(global-set-key (kbd "C-c 4") 'switch-to-gtd)
(global-set-key (kbd "C-c 9") 'ielm)

;;;; IDE useful stuffs ;;;;

;;;; Major Modes ;;;;
;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Haskell
(autoload 'haskell-mode "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file" "Major mode for editing haskell" t)
(setq haskell-font-lock-symbols t) ;; enable comments in haskell
(let ( (fn-list '(turn on haskell-dot-mode turn-on-haskell-indent)) ) 
  (mapc (lambda (fn) (add-hook 'haskell-mode-hook fn)) fn-list))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode) )
;(require 'haskell)

;;;; Minor Modes ;;;;
;(require 'scratch-mode) ;; funky mode sensitive scratch buffers

(autoload 'tramp "Remote file manipulation in TRAMP." t)

(require 'ido) ;; interactively do stuff

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

(defun switch-to-xmonadhs (&rest junk)
  "Switch to xmonad.hs file"
  (interactive)
    (find-file "~/.xmonad/xmonad.hs"))

(defun switch-to-screenrc (&rest junk)
  "Switch to .screenrc file"
  (interactive)
    (find-file "~/.screenrc"))

(defun switch-to-gtd (&rest junk)
  "Switch to gtd.org file"
  (interactive)
    (find-file "~/gtd.org"))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido"
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(server-start)
(desktop-load-default)
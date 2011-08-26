;; ==================== emacs.el
;;; Author:
;; Joshua Chubb <chubb.jp@gmail.com>
;;
;;; DATE: Friday, August 26 2011
;; An emacs configuration file. 
;;; Licence:
;; GNU GPL <www.gnu.org/copyleft/gpl.html>
;; ========================================

;;==============;;
;;; Load Paths ;;;
;;==============;;
(add-to-list 'load-path "~/.emacs.d/") ;; user load path
(add-to-list 'load-path "~/.emacs.d/ada")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/")

;;==================;;
;;; Prettify emacs ;;;
;;==================;;
(require 'zenburn)

(global-visual-line-mode)

;; Get rid of that deadspace
(menu-bar-mode -1)
(fringe-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode '(("(\\(lambda\\)\\>" (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ?λ))))))

(setq inhibit-startup-message t
      column-number-mode 1
      echo-keystrokes 0.1
      show-trailing-whitespace 1)

(set-face-attribute 'default nil :height 92)

;; I don't think I actually need this anymore I don't think I've started the server in X for a long time
(when window-system
  (zenburn)
  (set-frame-height (selected-frame) 38)
  (set-frame-width (selected-frame) 143)
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities))

;;=============================;;
;;; icomplete, ibuffer, ifile ;;;
;;=============================;;

;; icomplete
(autoload 'icomplete "Preview command input." t)
(eval-after-load "icomplete" (icomplete-mode t))
(setq icomplete-prospects-height 1
      icomplete-compute-delay 0)

;; ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Emacs Misc" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$") (name . "NEWS$")))
	       ("Configs" (or (filename . "emacs.el") (filename . "stumpwmrc.el") (filename . "gnus.el") (filename . "bashrc")))
	       ("Agenda" (or (filename . "todo.org") (filename . "Uni.org") (name . "^\\*Org Agenda\\*$")))
	       ("GIT" (or (mode . diff-mode) (mode . magit-mode) (name . "^\\*magit-process\\*$") (name . "^\\*magit-log-edit\\*$")))
	       ("ERC" (mode . erc-mode))
	       ("ELPA" (or (mode . package-menu-mode) (name . "\\*^\\*Package Info\\*$")))
	       ("Help" (or (mode . Info-mode) (mode . apropos-mode) (mode . Help-Mode) (mode . help-mode)))))))

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-default-sorting-mode 'major-mode
      ibuffer-expert t
      ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer nil
      ibuffer-sorting-mode 'recency
      ibuffer-use-header-line t)

;;==================;;
;;; Scratch Buffer ;;;
;;==================;;

;; bury *scratch* buffer instead of killing it

;; (defadvice kill-buffer (around kill-buffer-around-advice activate)
;;  (let ((buffer-to-kill (ad-get-arg 0)))
;;    (if (equal buffer-to-kill "*scratch*")
;;	(bury-buffer)
;;      ad-do-it)))

;; create or switch to scratch-buffer
(defun switch-to-scratch-and-back ()
  "Toggle between *scratch* buffer and the current buffer.
   If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
    (if (equal (current-buffer) scratch-buffer-name)
	(switch-to-buffer (other-buffer))
        (switch-to-buffer scratch-buffer-name) (lisp-interaction-mode))))

;; set the scratch message, It's the same as the lisp header
;; The idea is that I fiddle with code in the scratch buffer.
;; If I like it I will save to a file with C-c C-w
(custom-set-variables
 '(initial-scratch-message ";; ==================== 
;; Author:
;; Joshua Chubb <chubb.jp@gmail.com>
;;
;; DATE: 
;; 
;; ========================================"))

;;=============;;
;;; Variables ;;;
;;=============;;

(fset 'yes-or-no-p 'y-or-n-p)
(savehist-mode t)

;;=====================;;
;;; extra keybindings ;;;
;;=====================;;

;; File switches
(global-set-key (kbd "C-c 0 e") 'switch-to-dot-emacs)
(global-set-key (kbd "C-c 0 s") 'switch-to-stumpwmrc)
(global-set-key (kbd "C-c 0 c") 'switch-to-conkerorrc)
(global-set-key (kbd "C-c 2")   'switch-to-gtd)

;; Non-File Buffers
(global-set-key (kbd "C-c 1 d") 'dired)
(global-set-key (kbd "C-c 1 g") 'gnus)
(global-set-key (kbd "<f10>") 'switch-to-scratch-and-back)
(global-set-key (kbd "<f11>") 'magit-status)
(global-set-key (kbd "<f12>") 'org-agenda)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

;;=====================;;
;;; IDE useful stuffs ;;;
;;=====================;;

;; Auto-Insert stuff
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)

;; Auto-Complete

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (global-auto-complete-mode t)
;; (setq ac-auto-start 5 ac-ignore-case t ac-auto-show-menu t)

;;===============;;
;;; Major Modes ;;;
;;===============;;

;; Package
(when (load (expand-file-name "~/.emacs.d/elpa/package.el")) (package-initialize))
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org-mode" . "http://orgmode.org/pkg/daily/")))

;; Org-mode

(load-file "~/.emacs.d/org-config.el")
(load-file "~/.emacs.d/babel.el")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Haskell

(autoload 'haskell-mode "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file" "Major mode for editing haskell" t)
(setq haskell-font-lock-symbols t) ;; enable comments in haskell
;(let ( (fn-list '(turn on haskell-dot-mode turn-on-haskell-indent))
;  (mapc (lambda (fn) (add-hook 'haskell-mode-hook fn)) fn-list)))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode) )
;(require 'haskell)

;; LaTeX
(global-set-key (kbd "C-c e") 'insert-latex-block)

;; Ada
;;(load-file "~/.emacs.d/ada/ada-mode-keys.el");; don't understand how this doesn't work
(require 'ada-mode) ; maybe this will help

;;===============;;
;;; Minor Modes ;;;
;;===============;;

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

;;==============;;
;;; formatting ;;;
;;==============;;

;;====================;;
;;; Custom functions ;;;
;;====================;;

(defun switch-to-dot-emacs (&rest junk)
  "Switch to .emacs file"
  (interactive)
  (if (equal (buffer-name) "emacs.el")
      (eval-buffer)
    (find-file "~/bin/configs-and-stuffs/emacs.el")))

(defun switch-to-stumpwmrc (&rest junk)
  "Switch to stumpwmrc file"
  (interactive)
    (find-file "~/bin/configs-and-stuffs/stumpwmrc.el"))

(defun switch-to-conkerorrc (&rest junk)
  "Switch to stumpwmrc file"
  (interactive)
    (find-file "~/bin/configs-and-stuffs/conkerorrc"))

(defun switch-to-gtd (&rest junk)
  "Switch to gtd.org file"
  (interactive)
    (find-file "~/Documents/agenda/todo.org"))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido"
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun course-code (&rest junk)
  "Search for a COURSE-CODE appearing in 'Uni.org' and if found move the point to that location."
  (interactive)
  (switch-to-buffer "Uni.org")
  (goto-char (point-min))
  (let ((str (read-from-minibuffer "Enter course code: ")))
    (when (search-forward (concat "** " str "\t") nil nil)
      (forward-line 9))))

;;=============;;
;;; Skeletons ;;;
;;=============;;

(define-skeleton insert-fsa
  "Inserts an org-mode fsa"
  "File: "
  "#+begin_src latex :file " str
  ":packages '((\"\" \"tikz\")) :border 1em\n  \\usetikzlibrary{shapes,arrows}\n  \\tikzstyle{astate} = [circle,draw,text centered, font=\\footnotesize, fill=blue!25]\n  \\tikzstyle{rstate} = [circle,draw,text centered, font=\\footnotesize, fill=red!25]\n#+end_src")

(define-skeleton insert-latex-block
  "Inserts a LaTeX block"
  "Type: "
  "\\begin{" str
  "}\n\n\\end{" str
  "}")

;;(load-file "coversheet.el")

;;==============;;
;;; Exec stuff ;;;
;;==============;;

(server-start)
(desktop-load-default)
(require 'org-protocol)


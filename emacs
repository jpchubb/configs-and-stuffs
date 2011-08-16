(add-to-list 'load-path "~/.emacs.d/") ;; user load path
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/")
;;====================;;
;;;; Prettify emacs ;;;;
;;====================;;
(require 'zenburn)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(global-visual-line-mode)

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

;; pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode '(("(\\(lambda\\)\\>" (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ?λ))))))

;;=======================;;
;;;; extra keybindings ;;;;
;;=======================;;

;; File switches
(global-set-key (kbd "C-c 0 e") 'switch-to-dot-emacs)
(global-set-key (kbd "C-c 0 s") 'switch-to-stumpwmrc)
(global-set-key (kbd "C-c 0 c") 'switch-to-conkerorrc)
(global-set-key (kbd "C-c 2")   'switch-to-gtd)

;; programs
(global-set-key (kbd "C-c 1 d") 'dired)
(global-set-key (kbd "C-c 1 g") 'gnus)
(global-set-key (kbd "<f11>") 'magit-status)
(global-set-key (kbd "<f12>") 'org-agenda)

;;=======================;;
;;;; IDE useful stuffs ;;;;
;;=======================;;

;;=================;;
;;;; Major Modes ;;;;
;;=================;;

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


;;=================;;
;;;; Minor Modes ;;;;
;;=================;;

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

;;================;;
;;;; formatting ;;;;
;;================;;

;;======================;;
;;;; Custom functions ;;;;
;;======================;;

(defun switch-to-dot-emacs (&rest junk)
  "Switch to .emacs file"
  (interactive)
  (if (equal (buffer-name) "emacs")
      (eval-buffer)
    (find-file "~/bin/configs-and-stuffs/emacs")))

(defun switch-to-stumpwmrc (&rest junk)
  "Switch to stumpwmrc file"
  (interactive)
    (find-file "~/bin/configs-and-stuffs/stumpwmrc"))

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

;;===============;;
;;;; Skeletons ;;;;
;;===============;;

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


;;================;;
;;;; Exec stuff ;;;;
;;================;;

(server-start)
(desktop-load-default)
(require 'org-protocol)